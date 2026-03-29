package net.chwthewke.stnu
package spa
package prod

import cats.Id
import cats.data.NonEmptyList
import cats.data.NonEmptyVector
import cats.syntax.all.*
import monocle.syntax.all.*
import mouse.option.*
import scala.annotation.tailrec
import scala.collection.immutable.SortedMap

import data.Countable
import model.Item
import model.Recipe
import model.Transport
import model.prod.FlowEnd
import model.prod.Group
import protocol.persistence.ProcessSplitId

case class Flows(
    prod: ProdModel,
    prodHash: ProdModel.Hash,
    nextId: ProcessSplitId,
    endSplits: Map[EndId, ProcessSplits],
    itemFlows: Map[ClassName[Item], ItemFlows],
    ui: Flows.Ui
):

  private def toSplit( splitId: ProcessSplitId ): Option[Split[SrcDest]] =
    for
      ( fraction, group, endId ) <- endsBySplitId.get( splitId )
      splits                     <- endSplits.get( endId )
      splitIndex                 <- splits.splits.toVector.indexWhere( _._1 == splitId ).some.filter( _ >= 0 )
      srcDest                    <- endId match
                   case EndId.Process( recipe ) =>
                     prodRecipes
                       .get( recipe )
                       .map: process =>
                         process.recipe match
                           case _: Recipe.Extraction    => SrcDest.Extract( process )
                           case _: Recipe.NonExtraction => SrcDest.Step( process )
                   case EndId.Input( item )     => SrcDest.Input.some
                   case EndId.Requested( item ) => SrcDest.Requested.some
                   case EndId.Byproduct( item ) => SrcDest.Byproduct.some
    yield Split( splitId, endId, srcDest, splitIndex + 1, splits.splits.size, fraction, group )

  val splitsById: Map[ProcessSplitId, Split[SrcDest]] =
    ( for
      ( _, splits )  <- endSplits.iterator
      ( splitId, _ ) <- splits.splits.iterator
      split          <- toSplit( splitId ).iterator
    yield ( splitId, split ) ).toMap

  // NOTE these 2 exposed as it might be useful for testing
  lazy val prodRecipes: Map[ClassName[Recipe], ClockedRecipe] =
    prod.productionRows.fproductLeft( _.recipe.className ).toMap

  lazy val endsBySplitId: Map[ProcessSplitId, ( Double, Group, EndId )] =
    ( for
      ( endId, ProcessSplits( splits ) ) <- endSplits.iterator
      ( id, ( frac, group ) )            <- splits.iterator
    yield ( id, ( frac, group, endId ) ) ).toMap

  /**
   * Ok to call this if
   * @param splitId
   *   comes from somewhere in this [[Flows]]
   * @return
   *   the split for `splitId`
   */
  def getSplit( splitId: ProcessSplitId ): Split[SrcDest] = splitsById( splitId )

  lazy val itemTransports: Map[ClassName[Item], NonEmptyVector[ItemTransport]] =
    itemFlows.flatMap:
      case ( itemClass, itemFlows ) =>
        prod.env
          .getItem( itemClass )
          .map: item =>
            itemFlows.transports.zipWithIndex
              .map:
                case ( transportRef, index ) =>
                  transportRef.toItemTransport(
                    prod,
                    item,
                    index,
                    prodRecipes,
                    endsBySplitId,
                    endSplits,
                    splitsById,
                    itemFlows.transportSplits
                  )
          .tupleLeft( itemClass )

  val groups: Set[Group] =
    endSplits.unorderedFoldMap( _.splits.unorderedFoldMap { case ( _, group ) => group.ancestors } )

  lazy val groupFlows: Map[Group, GroupFlows] =
    groups.iterator.map( group => ( group, GroupFlows( this, group ) ) ).toMap

  //////////////////
  // UPDATES

  def setProduction( newProd: ProdModel ): Flows =
    if ( prodHash == ProdModel.solutionHash( newProd ) )
      copy( prod = newProd, prodHash = ProdModel.solutionHash( newProd ) )
    else Flows.init( newProd )

  def update( action: FlowAction ): Flows =
    action match
      case FlowAction.Reset                            => Flows.init( prod )
      case FlowAction.AbortModalFlowOp                 => copy( ui = ui.closeActionModal )
      case FlowAction.StartSplitSrcDest( pos )         => copy( ui = ui.setActionModal( splitActionModal( pos ) ) )
      case FlowAction.StartMergeSrcDest( pos )         => copy( ui = ui.setActionModal( mergeActionModal( pos ) ) )
      case FlowAction.MoveSrcDest( pos, amount, bump ) => move( pos, amount, bump )
      case FlowAction.SplitEqualSetCount( count )      => setSplitEqualCount( count )
      case FlowAction.SplitByMachineSetCount( count )  => setSplitByMachineCount( count )
      case FlowAction.SplitSrcDest( pos, splitType )   => split( pos, splitType ).copy( ui = ui.closeActionModal )
      case FlowAction.MergeSrcDest( pos, mergeType )   => merge( pos, mergeType ).copy( ui = ui.closeActionModal )
      case FlowAction.StartSplitTransport( modal )     => copy( ui = ui.setActionModal( modal.some ) )
      case FlowAction.SplitTransport( item, index, flowEnd, target, amount ) =>
        splitTransport( item, index, flowEnd, target, amount )
      case FlowAction.DeleteTransportSplit( item, index, direction, splitIndex ) =>
        println( s"deleteTransportSplit( $item, $index, $direction, $splitIndex )" )
        deleteTransportSplit( item, index, direction, splitIndex )

  /////////////////
  // ACTIONS

  // Set group

  def setGroup( endId: EndId, splitId: ProcessSplitId, group: Group ): Flows =
    this
      .focus( _.endSplits.index( endId ).splits.index( splitId )._2 )
      .replaceOption( group )
      .getOrElse( this )

  // Swap groups

  def swapGroups( from: Group, to: Group ): Flows =
    def graftGroup( depth: Int, to: Group, group: Group ): Group =
      Group( to.path ++ group.path.drop( depth ) )

    def swapAncestors( from: Group, to: Group )( group: Group ): Group =
      if ( group.path.startsWith( from.path ) )
        graftGroup( from.path.length, to, group )
      else if ( group.path.startsWith( to.path ) )
        graftGroup( to.path.length, from, group )
      else
        group

    this
      .focus( _.endSplits.each.splits.each._2 )
      .modify( swapAncestors( from, to ) )

  // Move

  def move( pos: SrcDestPos, amount: Int, bump: Boolean ): Flows =
    pos
      .getSplitId( itemFlows )
      .fold( this ): splitId =>
        def newItemTransport = ItemTransportRef( Map( pos.direction -> NonEmptyVector.one( splitId ) ) )

        this
          .focus( _.itemFlows.index( pos.item.className ) )
          .modifyA[Id]: ( itemTransports: ItemFlows ) =>
            val removed: NonEmptyVector[ItemTransportRef] =
              itemTransports.transports
                .focus( _.index( pos.index ).ends.at( pos.direction ) )
                .modifyA[Id]( nevOpt => nevOpt.flatMap( _.toVector.patch( pos.subIndex, Nil, 1 ).toNev ) )

            val updated: NonEmptyVector[ItemTransportRef] =
              if ( pos.index == 0 && amount == -1 ) newItemTransport +: removed
              else if ( pos.index == removed.length - 1 && amount == 1 ) removed :+ newItemTransport
              else if ( bump )
                val patchIx: Int = if ( amount == -1 ) pos.index else pos.index + 1
                NonEmptyVector.fromVectorUnsafe( removed.toVector.patch( patchIx, Seq( newItemTransport ), 0 ) )
              else
                removed
                  .focus( _.index( pos.index + amount ).ends.at( pos.direction ) )
                  .modifyA[Id]: ( itemTransportEnd: Option[NonEmptyVector[ProcessSplitId]] ) =>
                    itemTransportEnd.cata( _ :+ splitId, NonEmptyVector.one( splitId ) ).some

            updated
              .filter( _.ends.nonEmpty )
              .toNev
              .cata( ItemFlows( _, Vector.empty ), itemTransports )

  // Split

  def canSplit( pos: SrcDestPos ): Boolean =
    ( pos.getSplit( itemTransports ), pos.getTransport( itemTransports ) ).tupled.exists:
      case ( from, transport ) =>
        from.amount > transport.perMinute ||
        pos.getOppositeSplits( itemTransports ).exists( _.amount < from.amount - Countable.Tolerance ) ||
        from.item.value.process.exists( _.machineCount > 1 )

  private[prod] def splitActionModal( pos: SrcDestPos ): Option[ActionModal.SplitAction] =
    ( pos.getSplit( itemTransports ), pos.getLocal( itemTransports ) )
      .mapN( ActionModal.SplitAction( pos, _, _, pos.getOppositeSplits( itemTransports ) ) )

  private def setSplitEqualCount( count: Int ): Flows =
    this
      .focus( _.ui.actionModal.some )
      .modify:
        case sa: ActionModal.SplitAction => sa.copy( equalSplitCount = count.some )
        case other                       => other

  private def setSplitByMachineCount( count: Int ): Flows =
    this
      .focus( _.ui.actionModal.some )
      .modify:
        case sa: ActionModal.SplitAction => sa.copy( machineCount = sa.machineCount.map( t => ( count, t._2 ) ) )
        case other                       => other

  private def previewEvenSplit(
      from: Countable[Double, Split[SrcDest]],
      transportCount: Int,
      transport: Transport,
      others: NonEmptyList[List[Countable[Double, Split[SrcDest]]]]
  ): Option[List[Double]] =
    // NOTE caution, from is part of others
    Option
      .when( from.amount > transport.perMinute && transportCount > 1 ):
        val transportFlows: NonEmptyList[Double] = others.map( _.filterNot( _.item == from.item ).foldMap( _.amount ) )
        val totalFlow: Double                    = transportFlows.sumAll + from.amount
        // lf(i) + gp(i) = 1/n * (\sum lf(i) + from.amount)
        val grossParts: List[Double] = transportFlows.map( lf => totalFlow / others.length.toDouble - lf ).toList

        // TODO is this looping necessary? math it out
        @tailrec
        def loop( parts: List[Double] ): List[Double] =
          val ( neg, pos ) = parts.partition( _ < 0d )
          if ( neg.isEmpty ) pos
          else
            pos.toNel match
              case Some( posNel ) => loop( posNel.map( _ + neg.sum / pos.length ).toList )
              case None           => neg

        loop( grossParts.map( _ / from.amount ) )

  // NOTE in this version, the result, when defined, consists of fractions and is s.t. result.map(_.sum).forall(_ == 1d)
  private[prod] def previewSplitResultScaled( pos: SrcDestPos, splitType: SplitType ): Option[List[Double]] =
    splitType match
      case SplitType.Even =>
        (
          pos.getSplit( itemTransports ),
          pos.getTransportCount( itemTransports ),
          pos.getTransport( itemTransports ),
          pos.getAdjacentSplits( itemTransports ).toNel
        )
          .flatMapN( previewEvenSplit )
      case SplitType.Equal( countOpt ) =>
        countOpt.map: count =>
          List.fill( count )( 1d / count )
      case SplitType.EqualFixed( countOpt ) =>
        ( pos.getSplit( itemTransports ), countOpt ).flatMapN: ( split, count ) =>
          split.item.value.process.map: process =>
            val machineCount: Int = process.machineCount
            List
              .fill( count )( machineCount / count )
              .zipAll( List.fill( machineCount % count )( 1 ), 0, 0 )
              .map { case ( q, r ) => q + r }
              .filter( _ > 0 )
              .map { c => c.toDouble / machineCount }
      case SplitType.MachineCount( countsOpt ) =>
        ( pos.getSplit( itemTransports ), countsOpt ).flatMapN: ( split, count ) =>
          split.item.value.process.map: process =>
            val machineCount: Int = process.machineCount
            val fraction: Double  = count.toDouble / machineCount
            List( fraction, 1d - fraction )
      case SplitType.Remainder =>
        ( pos.getSplit( itemTransports ), pos.getLocal( itemTransports ) ).flatMapN: ( from, in ) =>
          def amount( direction: FlowEnd ): Double = in.getMachineFlows( direction ).foldMap( _.amount )
          val remainder: Double = ( amount( pos.direction ) - amount( pos.direction.opposite ) ) / from.amount
          Option.when( remainder > Countable.Tolerance && remainder < 1 - Countable.Tolerance ):
            List( 1d - remainder, remainder )
      case SplitType.Max =>
        ( pos.getSplit( itemTransports ), pos.getTransport( itemTransports ) )
          .flatMapN: ( from, in ) =>
            val fullTransportFraction: Double = in.perMinute.toDouble / from.amount
            Option.when( fullTransportFraction < 1d ):
              List( fullTransportFraction, 1d - fullTransportFraction )
      case SplitType.MaxAll =>
        ( pos.getSplit( itemTransports ), pos.getTransport( itemTransports ) )
          .flatMapN: ( from, in ) =>
            val fullTransportFraction: Double = in.perMinute.toDouble / from.amount
            Option.when( fullTransportFraction < 0.5d ):
              val count = ( from.amount / in.perMinute.toDouble ).floor.toInt
              List.fill( count )( fullTransportFraction ) ++
                ( 1 - count * fullTransportFraction ).some.filter( _ > Countable.Tolerance )
      case SplitType.Opposite( split ) =>
        pos
          .getSplit( itemTransports )
          .flatMap: from =>
            val splitFraction: Double = split.amount / from.amount
            Option.when( splitFraction < 1d ):
              List( splitFraction, 1d - splitFraction )

  private def toSplitMergePreview( pos: SrcDestPos, result: List[Double] ): SplitMergePreview =
    SplitMergePreview(
      result,
      pos.getTransport( itemTransports ).forall( in => result.exists( _ > in.perMinute ) ) // any overflow remaining?
    )

  def previewSplit( pos: SrcDestPos, splitType: SplitType ): Option[SplitMergePreview] =
    for
      scaledResult <- previewSplitResultScaled( pos, splitType )
      from         <- pos.getSplit( itemTransports )
    yield toSplitMergePreview( pos, scaledResult.map( _ * from.amount ) )

  def split( pos: SrcDestPos, splitType: SplitType ): Flows =
    ( pos.getSplitId( itemFlows ), previewSplitResultScaled( pos, splitType ).flatMap( _.toNel ) )
      .flatMapN: ( splitId, fractions ) =>
        endsBySplitId.get( splitId ).map { case ( f, grp, end ) => ( splitId, end, grp, fractions.map( _ * f ) ) }
      .map:
        case ( splitId, endId, group, fractions ) =>
          // new splits
          val addedSplits: List[( ProcessSplitId, ( Double, Group ) )] =
            fractions.tail.zipWithIndex.map:
              case ( f, ix ) => ( nextId + ix, ( f, group ) )

          this
            // 1. update endSplits (add new splits & update target split)
            .focus( _.endSplits.index( endId ).splits )
            .modifyA[Id]: ( splits: SortedMap[ProcessSplitId, ( Double, Group )] ) =>
              val updatedSplits: List[( ProcessSplitId, ( Double, Group ) )] =
                ( splitId, ( fractions.head, group ) ) :: addedSplits
              splits ++ updatedSplits
            // 2. update flow ends (add new splitIds where splitId is present)
            .focus( _.itemFlows.each.transports.each.ends.each )
            .filter( _.contains_( splitId ) )
            .modifyA[Id]: ( flowEnds: NonEmptyVector[ProcessSplitId] ) =>
              flowEnds ++ addedSplits._1F.toVector
            // 3. housekeeping: update nextId
            .focus( _.nextId )
            .modify( _ + addedSplits.length )
      .getOrElse( this )

  // Merge
  def canMerge( pos: SrcDestPos ): Boolean =
    pos
      .getSplitId( itemFlows )
      .flatMap( endsBySplitId.get )
      .flatMap( t => endSplits.get( t._3 ) )
      .exists( _.splits.size > 1 )

  private[prod] def mergeActionModal( pos: SrcDestPos ): Option[ActionModal.MergeAction] =
    ( pos.getSplit( itemTransports ), pos.getTransport( itemTransports ) )
      .mapN( ActionModal.MergeAction( pos, _, _, pos.getAdjacentSplits( itemTransports ) ) )

  def previewMerge( pos: SrcDestPos, mergeType: MergeType ): Option[SplitMergePreview] =
    previewMergeResult( pos, mergeType ).map( toSplitMergePreview( pos, _ ) )

  private def previewMergeResult( pos: SrcDestPos, mergeType: MergeType ): Option[List[Double]] =
    mergeType match
      case MergeType.Local =>
        ( pos.getSplit( itemTransports ), pos.getAdjacentSplits( itemTransports ).lift( pos.index ) ).flatMapN:
          ( from, local ) =>
            val mergeable: List[Countable[Double, Split[SrcDest]]] =
              local.filter( _.item.original == from.item.original )
            Option.when( mergeable.length > 1 )( List( mergeable.foldMap( _.amount ) ) )
      case MergeType.Global =>
        pos
          .getSplit( itemTransports )
          .flatMap: from =>
            val mergeable: List[Countable[Double, Split[SrcDest]]] =
              pos
                .getAdjacentSplits( itemTransports )
                .foldMap( _.filter( _.item.original == from.item.original ) )
            Option.when( mergeable.length > 1 )( List( mergeable.foldMap( _.amount ) ) )
      case MergeType.Adjacent( split ) =>
        pos
          .getSplit( itemTransports )
          .map( from => List( from.amount + split.amount ) )

  def merge( pos: SrcDestPos, mergeType: MergeType ): Flows =

    ( pos.getSplitId( itemFlows ), pos.getSplit( itemTransports ) ).tupled
      .fold( this ):
        case ( splitId, split ) =>
          def forTargetEnd( splitId: ProcessSplitId ): Boolean =
            endsBySplitId.get( splitId ).exists( _._3 == split.item.end )

          val mergeTargets: List[ProcessSplitId] =
            mergeType match
              case MergeType.Local =>
                this
                  .focus(
                    _.itemFlows
                      .index( pos.item.className )
                      .transports
                      .index( pos.index )
                      .ends
                      .index( pos.direction )
                      .each
                  )
                  .filter( forTargetEnd )
                  .getAll
              case MergeType.Global =>
                this
                  .focus( _.itemFlows.index( pos.item.className ).transports.each.ends.index( pos.direction ).each )
                  .filter( forTargetEnd )
                  .getAll
              case MergeType.Adjacent( split ) =>
                List( splitId, split.item.id )

          val toRemove: Set[ProcessSplitId] = mergeTargets.iterator.filterNot( _ == splitId ).toSet

          this
            // 1. update endSplits (remove merge targets & update target split)
            .focus( _.endSplits.index( split.item.end ).splits )
            .modifyA[Id]: ( splits: SortedMap[ProcessSplitId, ( Double, Group )] ) =>
              val fraction: Double = mergeTargets.foldMap( id => splits.get( id ).foldMap( _._1 ) )
              splits.removedAll( toRemove ).updatedWith( splitId )( to => to.map( t => ( fraction, t._2 ) ) )
            // 2. update flow ends (remove splitIds for merge targets)
            .focus( _.itemFlows.each )
            .modifyA[Id]: ( itemFlows: ItemFlows ) =>
              itemFlows.transports
                .focus( _.each.ends )
                .modifyA[Id]: ( ends: Map[FlowEnd, NonEmptyVector[ProcessSplitId]] ) =>
                  ends.mapFilter: splits =>
                    splits.filterNot( toRemove ).toNev
                // 3. remove any emptied ItemTransport
                .filter( _.ends.nonEmpty )
                .toNev
                .cata( ItemFlows( _, Vector.empty ), itemFlows )

  private def transportExcessAt( transport: ItemTransport, flowEnd: FlowEnd ): Option[Double] =
    val excess: Double =
      flowEnd match
        case FlowEnd.Source      => transport.sourceAmount - transport.destinationAmount
        case FlowEnd.Destination => transport.destinationAmount - transport.sourceAmount
    Option.when( excess > Countable.Tolerance )( excess )

  // Split transport
  //  - at Source: from transports which are unbalanced with sources > destinations
  //  - at Destination : from transports which are unbalanced with destinations > sources
  def previewSplitTransport(
      item: ClassName[Item],
      index: Int,
      flowEnd: FlowEnd
  ): Option[( Double, NonEmptyList[Int] )] =
    itemTransports
      .get( item )
      .flatMap: transports =>
        (
          // this transport's excess amount
          transports
            .get( index )
            .flatMap( transportExcessAt( _, flowEnd ) ),
          // potential peers (transports which are unbalanced the other way and don't already have a split with this transport)
          transports.zipWithIndex.toVector
            .mapFilter:
              case ( transport, ix ) =>
                Option.when(
                  transportExcessAt( transport, flowEnd.opposite ).isDefined &&
                    transport.getTransportSplits( flowEnd ).forall( _.item != index )
                )( ix )
            .toList
            .toNel
        ).tupled

  def splitTransport( item: ClassName[Item], index: Int, flowEnd: FlowEnd, target: Int, amount: Double ): Flows =
    val ( from, to ) = if ( flowEnd == FlowEnd.Source ) ( index, target ) else ( target, index )
    val split        = TransportSplit( amount, from, to )
    this
      .focus( _.itemFlows.index( item ).transportSplits )
      .modify( _ :+ split )
      .copy( ui = ui.closeActionModal )

  def deleteTransportSplit( item: ClassName[Item], transportIndex: Int, flowEnd: FlowEnd, splitIndex: Int ): Flows =
    this
      .focus( _.itemFlows.index( item ).transportSplits )
      .modify: ( transportSplits: Vector[TransportSplit] ) =>
        transportSplits.zipWithIndex
          .filter:
            case ( split, _ ) =>
              flowEnd match
                case FlowEnd.Destination => split.from == transportIndex
                case FlowEnd.Source      => split.to == transportIndex
          .lift( splitIndex )
          ._2F
          .cata( ix => transportSplits.patch( ix, Nil, 1 ), transportSplits )

object Flows:
  case class Ui( actionModal: Option[ActionModal] ):
    def setActionModal( action: Option[ActionModal] ): Ui = copy( actionModal = action )
    def closeActionModal: Ui                              = copy( actionModal = none )

  object Ui:
    val init: Ui = Ui( none )

  private def externalAmounts( prod: ProdModel ): List[Countable[Double, Item]] =
    prod.productionRows.foldMap( _.itemsPerMinute ).gather.filter( _.isSignificant )

  def init( prod: ProdModel ): Flows =

    val endIds: List[EndId] =
      prod.productionRows.map( cr => EndId.Process( cr.recipe.className ) ) ++
        externalAmounts( prod )
          .foldMap:
            case Countable( item, amount ) =>
              if ( amount < 0 ) Countable( item.className, -amount ).significant.map( EndId.Input( _ ) ).toList
              else
                val requestedAmount = prod.requested.getOrElse( item, 0d )
                List(
                  Countable( item.className, requestedAmount ).significant.map( EndId.Requested( _ ) ),
                  Countable( item.className, amount - requestedAmount ).significant.map( EndId.Byproduct( _ ) )
                ).flattenOption

    val endProcessSplitIds: SortedMap[EndId, ProcessSplitId] =
      endIds.zipWithIndex
        .map:
          case ( endId, index ) => ( endId, ProcessSplitId( index + 1 ) )
        .to( SortedMap )

    val endSplits: Map[EndId, ProcessSplits] =
      endProcessSplitIds.fmap( ProcessSplits.init )

    Flows(
      prod,
      ProdModel.solutionHash( prod ),
      ProcessSplitId( endIds.length + 1 ),
      endSplits,
      ItemFlows.init( prod.productionRows, endProcessSplitIds ),
      Ui.init
    )

  private def storeEndId( endId: EndId ): pp.EndId =
    endId match
      case EndId.Process( recipe ) => pp.EndId.Process( recipe )
      case EndId.Input( item )     => pp.EndId.Input( item.item )
      case EndId.Requested( item ) => pp.EndId.Requested( item.item )
      case EndId.Byproduct( item ) => pp.EndId.Byproduct( item.item )

  private def store( flows: Flows ): pp.Flows =
    pp.Flows(
      flows.prodHash,
      flows.nextId,
      flows.endSplits.iterator
        .map:
          case ( endId, splits ) =>
            (
              storeEndId( endId ),
              splits.splits.toVector.map { case ( id, ( fraction, group ) ) => ( id, fraction, group ) }
            )
        .toVector,
      flows.itemFlows
        .fmap: itemTransports =>
          pp.ItemFlows(
            itemTransports.transports.iterator
              .map: itemTransport =>
                ( for
                  ( end, splits ) <- itemTransport.ends.iterator
                  splitId         <- splits.iterator
                yield ( end, splitId ) ).toVector
              .toVector,
            itemTransports.transportSplits.map:
              case TransportSplit( amount, from, to ) => ( amount, from, to )
          )
    )

  given Conversion[Flows, pp.Flows]:
    override def apply( flows: Flows ): pp.Flows = store( flows )

  def from( prod: ProdModel, stored: pp.Flows ): Flows =
    restore( prod, stored ).getOrElse( init( prod ) )

  private def restore( prod: ProdModel, stored: pp.Flows ): Option[Flows] =
    Option.when( stored.prodHash == ProdModel.solutionHash( prod ) ):

      val endAmounts: Map[ClassName[Item], Double] =
        externalAmounts( prod ).map( ci => ( ci.item.className, ci.amount ) ).toMap
      val requestedAmounts: Map[ClassName[Item], Double] =
        prod.requested.map { case ( item, amount ) => ( item.className, amount ) }

      def restoreEndId( endId: pp.EndId ): EndId =
        endId match
          case pp.EndId.Process( recipe ) => EndId.Process( recipe )
          case pp.EndId.Input( item )     =>
            EndId.Input( Countable( item, -endAmounts.getOrElse( item, 0d ) ) )
          case pp.EndId.Requested( item ) =>
            EndId.Requested( Countable( item, requestedAmounts.getOrElse( item, 0d ) ) )
          case pp.EndId.Byproduct( item ) =>
            EndId.Byproduct(
              Countable( item, endAmounts.getOrElse( item, 0d ) - requestedAmounts.getOrElse( item, 0d ) )
            )

      val endSplits: Map[EndId, ProcessSplits] =
        stored.endSplits.toMap.map:
          case ( endId, split ) =>
            (
              restoreEndId( endId ),
              ProcessSplits(
                split.iterator.map { case ( id, fraction, group ) => ( id, ( fraction, group ) ) }.to( SortedMap )
              )
            )

      val itemFlowRefs: Map[ClassName[Item], ItemFlows] =
        stored.itemFlows.mapFilter: itemFlows =>
          itemFlows.itemTransports
            .map: transport =>
              ItemTransportRef(
                transport.groupMapReduce( _._1 )( t => NonEmptyVector.one( t._2 ) )( _.concatNev( _ ) )
              )
            .toNev
            .map:
              ItemFlows(
                _,
                itemFlows.transportSplits.map:
                  case ( amount, from, to ) => TransportSplit( amount, from, to )
              )

      Flows( prod, stored.prodHash, stored.nextId, endSplits, itemFlowRefs, Ui.init )
