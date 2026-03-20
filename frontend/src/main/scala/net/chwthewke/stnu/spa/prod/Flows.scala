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
    itemFlowRefs: Map[ClassName[Item], NonEmptyVector[ItemTransportRef]],
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

  lazy val itemFlows: Map[ClassName[Item], NonEmptyVector[ItemTransport]] =
    itemFlowRefs.flatMap:
      case ( itemClass, itemTransportRefs ) =>
        prod.env
          .getItem( itemClass )
          .map: item =>
            itemTransportRefs.map( _.toItemTransport( prod, item, prodRecipes, endsBySplitId, endSplits, splitsById ) )
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
      case FlowAction.AbortScrDestOp                   => copy( ui = ui.closeActionModal )
      case FlowAction.StartSplitSrcDest( pos )         => copy( ui = ui.setActionModal( splitActionModal( pos ) ) )
      case FlowAction.StartMergeSrcDest( pos )         => copy( ui = ui.setActionModal( mergeActionModal( pos ) ) )
      case FlowAction.MoveSrcDest( pos, amount, bump ) => move( pos, amount, bump )
      case FlowAction.SplitEqualSetCount( count )      => setSplitEqualCount( count )
      case FlowAction.SplitByMachineSetCount( count )  => setSplitByMachineCount( count )
      case FlowAction.SplitSrcDest( pos, splitType )   => split( pos, splitType ).copy( ui = ui.closeActionModal )
      case FlowAction.MergeSrcDest( pos, mergeType )   => merge( pos, mergeType ).copy( ui = ui.closeActionModal )

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
      .getSplitId( itemFlowRefs )
      .fold( this ): splitId =>
        def newItemTransport = ItemTransportRef( Map( pos.direction -> NonEmptyVector.one( splitId ) ) )

        this
          .focus( _.itemFlowRefs.index( pos.item.className ) )
          .modifyA[Id]: ( itemTransports: NonEmptyVector[ItemTransportRef] ) =>
            val removed: NonEmptyVector[ItemTransportRef] =
              itemTransports
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

            updated.filter( _.ends.nonEmpty ).toNev.getOrElse( removed )

  // Split

  def canSplit( pos: SrcDestPos ): Boolean =
    ( pos.getSplit( itemFlows ), pos.getTransport( itemFlows ) ).tupled.exists:
      case ( from, transport ) =>
        from.amount > transport.perMinute ||
        pos.getOppositeSplits( itemFlows ).exists( _.amount < from.amount - Countable.Tolerance )

  private[prod] def splitActionModal( pos: SrcDestPos ): Option[ActionModal.SplitAction] =
    ( pos.getSplit( itemFlows ), pos.getLocal( itemFlows ) )
      .mapN( ActionModal.SplitAction( pos, _, _, pos.getOppositeSplits( itemFlows ) ) )

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
          pos.getSplit( itemFlows ),
          pos.getTransportCount( itemFlows ),
          pos.getTransport( itemFlows ),
          pos.getAdjacentSplits( itemFlows ).toNel
        )
          .flatMapN( previewEvenSplit )
      case SplitType.Equal( countOpt ) =>
        countOpt.map: count =>
          List.fill( count )( 1d / count )
      case SplitType.EqualFixed( countOpt ) =>
        ( pos.getSplit( itemFlows ), countOpt ).flatMapN: ( split, count ) =>
          split.item.value.process.map: process =>
            val machineCount: Int = process.machineCount
            List
              .fill( count )( machineCount / count )
              .zipAll( List.fill( machineCount % count )( 1 ), 0, 0 )
              .map { case ( q, r ) => q + r }
              .filter( _ > 0 )
              .map { c => c.toDouble / machineCount }
      case SplitType.MachineCount( countsOpt ) =>
        ( pos.getSplit( itemFlows ), countsOpt ).flatMapN: ( split, count ) =>
          split.item.value.process.map: process =>
            val machineCount: Int = process.machineCount
            val fraction: Double  = count.toDouble / machineCount
            List( fraction, 1d - fraction )
      case SplitType.Remainder =>
        ( pos.getSplit( itemFlows ), pos.getLocal( itemFlows ) ).flatMapN: ( from, in ) =>
          def amount( direction: FlowEnd ): Double = in.get( direction ).foldMap( _.amount )
          val remainder: Double = ( amount( pos.direction ) - amount( pos.direction.opposite ) ) / from.amount
          Option.when( remainder > 0 && remainder < 1 ):
            List( 1d - remainder, remainder )
      case SplitType.Max =>
        ( pos.getSplit( itemFlows ), pos.getTransport( itemFlows ) )
          .flatMapN: ( from, in ) =>
            val fullTransportFraction: Double = in.perMinute.toDouble / from.amount
            Option.when( fullTransportFraction < 1d ):
              List( fullTransportFraction, 1d - fullTransportFraction )
      case SplitType.MaxAll =>
        ( pos.getSplit( itemFlows ), pos.getTransport( itemFlows ) )
          .flatMapN: ( from, in ) =>
            val fullTransportFraction: Double = in.perMinute.toDouble / from.amount
            Option.when( fullTransportFraction < 0.5d ):
              val count = ( from.amount / in.perMinute.toDouble ).floor.toInt
              List.fill( count )( fullTransportFraction ) ++
                ( 1 - count * fullTransportFraction ).some.filter( _ > Countable.Tolerance )
      case SplitType.Opposite( split ) =>
        pos
          .getSplit( itemFlows )
          .flatMap: from =>
            val splitFraction: Double = split.amount / from.amount
            Option.when( splitFraction < 1d ):
              List( splitFraction, 1d - splitFraction )

  private def toSplitMergePreview( pos: SrcDestPos, result: List[Double] ): SplitMergePreview =
    SplitMergePreview(
      result,
      pos.getTransport( itemFlows ).forall( in => result.exists( _ > in.perMinute ) ) // any overflow remaining?
    )

  def previewSplit( pos: SrcDestPos, splitType: SplitType ): Option[SplitMergePreview] =
    for
      scaledResult <- previewSplitResultScaled( pos, splitType )
      from         <- pos.getSplit( itemFlows )
    yield toSplitMergePreview( pos, scaledResult.map( _ * from.amount ) )

  def split( pos: SrcDestPos, splitType: SplitType ): Flows =
    ( pos.getSplitId( itemFlowRefs ), previewSplitResultScaled( pos, splitType ).flatMap( _.toNel ) )
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
            .focus( _.itemFlowRefs.each.each.ends.each )
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
      .getSplitId( itemFlowRefs )
      .flatMap( endsBySplitId.get )
      .flatMap( t => endSplits.get( t._3 ) )
      .exists( _.splits.size > 1 )

  private[prod] def mergeActionModal( pos: SrcDestPos ): Option[ActionModal.MergeAction] =
    ( pos.getSplit( itemFlows ), pos.getTransport( itemFlows ) )
      .mapN( ActionModal.MergeAction( pos, _, _, pos.getAdjacentSplits( itemFlows ) ) )

  def previewMerge( pos: SrcDestPos, mergeType: MergeType ): Option[SplitMergePreview] =
    previewMergeResult( pos, mergeType ).map( toSplitMergePreview( pos, _ ) )

  private def previewMergeResult( pos: SrcDestPos, mergeType: MergeType ): Option[List[Double]] =
    mergeType match
      case MergeType.Local =>
        ( pos.getSplit( itemFlows ), pos.getAdjacentSplits( itemFlows ).lift( pos.index ) ).flatMapN: ( from, local ) =>
          val mergeable: List[Countable[Double, Split[SrcDest]]] =
            local.filter( _.item.original == from.item.original )
          Option.when( mergeable.length > 1 )( List( mergeable.foldMap( _.amount ) ) )
      case MergeType.Global =>
        pos
          .getSplit( itemFlows )
          .flatMap: from =>
            val mergeable: List[Countable[Double, Split[SrcDest]]] =
              pos
                .getAdjacentSplits( itemFlows )
                .foldMap( _.filter( _.item.original == from.item.original ) )
            Option.when( mergeable.length > 1 )( List( mergeable.foldMap( _.amount ) ) )
      case MergeType.Adjacent( split ) =>
        pos
          .getSplit( itemFlows )
          .map( from => List( from.amount + split.amount ) )

  def merge( pos: SrcDestPos, mergeType: MergeType ): Flows =

    ( pos.getSplitId( itemFlowRefs ), pos.getSplit( itemFlows ) ).tupled
      .fold( this ):
        case ( splitId, split ) =>
          def forTargetEnd( splitId: ProcessSplitId ): Boolean =
            endsBySplitId.get( splitId ).exists( _._3 == split.item.end )

          val mergeTargets: List[ProcessSplitId] =
            mergeType match
              case MergeType.Local =>
                this
                  .focus(
                    _.itemFlowRefs.index( pos.item.className ).index( pos.index ).ends.index( pos.direction ).each
                  )
                  .filter( forTargetEnd )
                  .getAll
              case MergeType.Global =>
                this
                  .focus( _.itemFlowRefs.index( pos.item.className ).each.ends.index( pos.direction ).each )
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
            .focus( _.itemFlowRefs.each )
            .modifyA[Id]: ( itemTransports: NonEmptyVector[ItemTransportRef] ) =>
              itemTransports
                .focus( _.each.ends )
                .modifyA[Id]: ( ends: Map[FlowEnd, NonEmptyVector[ProcessSplitId]] ) =>
                  ends.mapFilter: splits =>
                    splits.filterNot( toRemove ).toNev
                // 3. remove any emptied ItemTransport
                .filter( _.ends.nonEmpty )
                .toNev
                .getOrElse( itemTransports )

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

    val itemFlowRefs: Map[ClassName[Item], NonEmptyVector[ItemTransportRef]] =
      val processesByClass: Map[ClassName[Recipe], ClockedRecipe] =
        prod.productionRows.fproductLeft( _.recipe.className ).toMap

      endProcessSplitIds.toVector
        .foldMap[Map[ClassName[Item], Map[FlowEnd, NonEmptyVector[ProcessSplitId]]]]:
          case ( EndId.Process( recipeClass ), id ) =>
            processesByClass
              .get( recipeClass )
              .foldMap: cr =>
                cr.ingredientsPerMinute
                  .foldMap( ci => Map( ci.item.className -> Map( FlowEnd.Destination -> NonEmptyVector.one( id ) ) ) )
                  |+| cr.productsPerMinute
                    .foldMap( ci => Map( ci.item.className -> Map( FlowEnd.Source -> NonEmptyVector.one( id ) ) ) )
          case ( EndId.Input( ci ), id ) =>
            Map( ci.item -> Map( FlowEnd.Source -> NonEmptyVector.one( id ) ) )
          case ( EndId.Requested( ci ), id ) =>
            Map( ci.item -> Map( FlowEnd.Destination -> NonEmptyVector.one( id ) ) )
          case ( EndId.Byproduct( ci ), id ) =>
            Map( ci.item -> Map( FlowEnd.Destination -> NonEmptyVector.one( id ) ) )
        .fmap( byEnd => NonEmptyVector.one( ItemTransportRef( byEnd ) ) )

    Flows(
      prod,
      ProdModel.solutionHash( prod ),
      ProcessSplitId( endIds.length + 1 ),
      endSplits,
      itemFlowRefs,
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
      flows.itemFlowRefs
        .fmap: itemTransports =>
          itemTransports.iterator
            .map: itemTransport =>
              ( for
                ( end, splits ) <- itemTransport.ends.iterator
                splitId         <- splits.iterator
              yield ( end, splitId ) ).toVector
            .toVector
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

      val itemFlowRefs: Map[ClassName[Item], NonEmptyVector[ItemTransportRef]] =
        stored.itemFlows.mapFilter: transports =>
          transports
            .map: transport =>
              ItemTransportRef(
                transport.groupMapReduce( _._1 )( t => NonEmptyVector.one( t._2 ) )( _.concatNev( _ ) )
              )
            .toNev

      Flows( prod, stored.prodHash, stored.nextId, endSplits, itemFlowRefs, Ui.init )
