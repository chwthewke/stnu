package net.chwthewke.stnu
package spa
package prod

import cats.data.NonEmptyVector
import cats.syntax.all.*
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.*
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

import data.Countable
import model.Item
import model.Recipe
import model.prod.FlowEnd
import protocol.persistence.ProcessSplitId

trait FlowsProperties:
  self: ScalaCheckSuite =>

  given approxItemIO: Approx[ItemIO[SrcDest]] = new Gathering with Approx:
    override def approx( x: ItemIO[SrcDest], y: ItemIO[SrcDest] ): Boolean =
      x.sources.map( _.map( showSrcDest ) ) =~ y.sources.map( _.map( showSrcDest ) )
        && x.destinations.map( _.map( showSrcDest ) ) =~ y.destinations.map( _.map( showSrcDest ) )

  given [A <: SrcDest] => Approx[Split[A]]:
    override def approx( x: Split[A], y: Split[A] ): Boolean =
      x.fraction =~ y.fraction
        && x.original == y.original
        && x.split == y.split
        && x.max == y.max
        && x.group == y.group

  given Approx[ProcessSplits]:
    override def approx( x: ProcessSplits, y: ProcessSplits ): Boolean =
      x.splits =~ y.splits

  given Approx[Flows]:
    override def approx( x: Flows, y: Flows ): Boolean =
      x.prodHash == y.prodHash
        && x.nextId == y.nextId
        && x.endSplits =~ y.endSplits
        && x.itemFlows == y.itemFlows

  trait Gathering:
    given approxVectorCountable[A]: Approx[Vector[Countable[Double, A]]] = new Approx:
      override def approx( x: Vector[Countable[Double, A]], y: Vector[Countable[Double, A]] ): Boolean =
        !( x ++ y.map( _.mapAmount( -_ ) ) ).gather.exists( _.isSignificant )

  val commonFlowsProperties: Vector[( String, Flows => Unit )] =
    Vector(
      // TODO terminal-end splits are in root group
      ( "endsBySplitId has all split ids", endsBySplitIdHasAllSplitIds ),
      ( "splitsById has all split ids", splitsByIdHasAllSplitIds ),
      ( "endSplits has splits for all end/number from itemFlows", endSplitsHasAllSplitsInItemFlows ),
      ( "process splits cover original processes", processSplitsCoverOriginalProcesses ),
      ( "process splits add up to the original", processSplitsSumToOriginal ),
      ( "item flows are non-empty", itemFlowsAreNonEmpty ),
      ( "item flows cover original item's I/O", itemFlowsCoverOriginalItemIOs ),
      ( "item flows sources match process splits", itemFlowSourcesMatchSplits ),
      ( "item flows destinations match process splits", itemFlowDestinationsMatchSplits ),
      ( "item flow source amounts match production", itemFlowSourcesMatchProduction ),
      ( "item flow destination amounts match consumption", itemFlowDestinationMatchConsumption ),
      ( "split conserving amounts where possible", previewSplitResultConservesAmount ),
      ( "be storable as a pp.Flows", storable )
    )

  val freshFlowsProperties: Vector[( String, Flows => Unit )] =
    Vector(
      ( "item flows are balanced", itemFlowsAreBalanced )
    )

  def checkFlowsProperties(
      name: String,
      flows: Gen[FlowsSetup],
      properties: Vector[( String, Flows => Unit )] = commonFlowsProperties
  )( implicit loc: munit.Location ): Unit =
    properties.foreach:
      case ( propName, prop ) =>
        property( s"$name: $propName" ):
          forAll( flows )( sf => prop( sf.flows ) )

  private def splitIdsOf( flows: Flows ): SortedSet[ProcessSplitId] =
    flows.endSplits.unorderedFoldMap( _.splits.keySet ) ++
      flows.itemFlows.unorderedFoldMap( _.transports.foldMap( _.ends.unorderedFoldMap( _.toVector.to( SortedSet ) ) ) )

  def endsBySplitIdHasAllSplitIds( flows: Flows ): Unit =
    splitIdsOf( flows ).foreach: splitId =>
      assert( flows.endsBySplitId.contains( splitId ) )

  def splitsByIdHasAllSplitIds( flows: Flows ): Unit =
    splitIdsOf( flows ).foreach: splitId =>
      assert( flows.splitsById.contains( splitId ) )

  def endSplitsHasAllSplitsInItemFlows( flows: Flows ): Unit =
    val endIdAndNumbers: Set[( EndId, Int )] =
      flows.itemTransports.unorderedFoldMap:
        _.foldMap: it =>
          ( it.sources.map( cs => ( cs.item.end, cs.item.split ) ) ++
            it.destinations.map( cs => ( cs.item.end, cs.item.split ) ) ).toSet

    endIdAndNumbers.foreach:
      case ( endId, splitNumber ) =>
        assert( flows.endSplits.get( endId ).exists( _.splits.size >= splitNumber ) )

  def processSplitsCoverOriginalProcesses( flows: Flows ): Unit =

    val processRecipes =
      flows.endSplits.keySet.collect:
        case EndId.Process( recipe, _ ) => recipe

    assert(
      clue( processRecipes ) ==
        clue( flows.prod.productionRows.map( _.recipe.className ).toSet )
    )

  def processSplitsSumToOriginal( flows: Flows ): Unit =
    flows.endSplits.values.foreach: processSplits =>
      assert( processSplits.splits._1F.sumAll =~ 1.0d )

  def itemFlowsAreNonEmpty( flows: Flows ): Unit =
    assert(
      flows.itemTransports.forall: t =>
        val ( item, flow ) = clue( t )
        flow.nonEmpty &&
        flow.forall( it =>
          it.sources.gather.exists( _.isSignificant ) ||
            it.destinations.gather.exists( _.isSignificant )
        )
    )

  def sourceFlowsArePositive( flows: Flows ): Unit =
    assert(
      flows.itemTransports.forall: t =>
        val ( item, flows ) = clue( t )
        flows.forall: it =>
          it.sources.forall: flow =>
            clue( flow ).amount > -Countable.Tolerance
    )

  def destinationFlowsArePositive( flows: Flows ): Unit =
    assert(
      flows.itemTransports.forall: t =>
        val ( item, flows ) = clue( t )
        flows.forall: it =>
          it.destinations.forall: flow =>
            clue( flow ).amount > -Countable.Tolerance
    )

  def itemFlowsAreBalanced( flows: Flows ): Unit =
    assert(
      flows.itemTransports.forall: t =>
        val ( item, flow ) = clue( t )
        flow.forall( sub => clue( sub.sources ).foldMap( _.amount ) =~ clue( sub.destinations ).foldMap( _.amount ) )
    )

  private def itemIO( prod: ProdModel ): SortedMap[Item, ItemIO[SrcDest]] =
    ItemIO.of( prod.productionRows, prod.requested )

  // TODO does something make sense to rewrite this test without ItemIO?
  def itemFlowsCoverOriginalItemIOs( flows: Flows ): Unit =
    def reduce1ToItemIO( itemTransport: ItemTransport ): ItemIO[SrcDest] =
      ItemIO(
        itemTransport.sources.map( _.map( _.original ) ),
        itemTransport.destinations.map( _.map( _.original ) )
      )

    def reduceToItemIO( itemTransports: NonEmptyVector[ItemTransport] ): ItemIO[SrcDest] =
      itemTransports.foldMap( reduce1ToItemIO )

    def withClue( itemIO: ItemIO[SrcDest] ): ItemIO[SrcDest] =
      clue( ShownItemIO( itemIO ).toString )
      itemIO

    itemIO( flows.prod ).toVector
      .map:
        case ( item, itemIO ) =>
          ( item, itemIO, flows.itemTransports.get( item.className ) )
      .foreach: t =>
        val ( item, itemIO, itemTransportOpt ) = t

        assert( itemTransportOpt.exists( tr => withClue( reduceToItemIO( tr ) ) =~ withClue( itemIO ) ) )

  def itemFlowSourcesMatchProduction( flows: Flows ): Unit =
    flows.itemTransports.foreach:
      case ( item, transports ) =>
        transports.toVector.foreach: transport =>
          transport.sources
            .map( _.map( _.value ) )
            .collect:
              case Countable( SrcDest.Step( process ), amount )    => ( process, amount )
              case Countable( SrcDest.Extract( process ), amount ) => ( process, amount )
            .foreach:
              case ( process, flowAmount ) =>
                process.productsPerMinute.find( _.item.className == item ) match
                  case Some( Countable( _, processAmount ) ) =>
                    assert( clue( processAmount ) =~ clue( flowAmount ) )
                  case None =>
                    fail( "missing product", clues( item, process ) )

  def itemFlowDestinationMatchConsumption( flows: Flows ): Unit =
    flows.itemTransports.foreach:
      case ( item, transports ) =>
        transports.toVector.foreach: transport =>
          transport.destinations
            .map( _.map( _.value ) )
            .collect:
              case Countable( SrcDest.Step( process ), amount ) => ( process, amount )
            .foreach:
              case ( process, flowAmount ) =>
                process.ingredientsPerMinute.find( _.item.className == item ) match
                  case Some( Countable( _, processAmount ) ) =>
                    assert( clue( processAmount ) =~ clue( flowAmount ) )
                  case None =>
                    fail( "missing ingredient", clues( item, process ) )

  private def itemFlowEndsMatchPeerSplits( flows: Flows )(
      flowEnd: FlowEnd,
      processFlows: ClockedRecipe => List[Countable[Double, Item]]
  ): Unit =
    def prodRecipe( endId: EndId ): Option[ClockedRecipe] =
      endId match
        case EndId.Process( recipe, boost ) => flows.prodRecipes.get( ( recipe, boost ) )
        case _                              => none

    val splitsProducers: Vector[( ClassName[Item], ClassName[Recipe], Int, Double )] =
      ( for
        ( endId, processSplits )       <- flows.endSplits.iterator
        process                        <- prodRecipe( endId ).iterator
        product                        <- processFlows( process ).iterator
        ( ( _, ( fraction, _ ) ), ix ) <- processSplits.splits.toVector.zipWithIndex.iterator
      yield ( product.item.className, process.recipe.className, ix + 1, product.amount * fraction ) ).toVector

    val itemFlowsEnds: Vector[( ClassName[Item], ClassName[Recipe], Int, Double )] =
      ( for
        ( itemClass, itemTransports ) <- flows.itemTransports.iterator
        itemTransport                 <- itemTransports.iterator
        source                        <- itemTransport.getMachineFlows( flowEnd ).iterator
        process                       <- source.item.original.process.iterator
      yield ( itemClass, process.recipe.className, source.item.split, source.amount ) ).toVector

    assert( clue( splitsProducers.sorted ) =~ clue( itemFlowsEnds.sorted ) )

  def itemFlowSourcesMatchSplits( flows: Flows ): Unit =
    itemFlowEndsMatchPeerSplits( flows )( FlowEnd.Source, _.productsPerMinute )

  def itemFlowDestinationsMatchSplits( flows: Flows ): Unit =
    itemFlowEndsMatchPeerSplits( flows )( FlowEnd.Destination, _.ingredientsPerMinute )

  def flowsPoses( flows: Flows ): Vector[SrcDestPos] =
    ( for
      ( itemClass, itemTransports ) <- flows.itemTransports.iterator
      item                          <- flows.prod.env.getItem( itemClass ).iterator
      ( itemTransport, index )      <- itemTransports.iterator.zipWithIndex
      flowEnd                       <- FlowEnd.cases.iterator
      subIndex                      <- itemTransport.getMachineFlows( flowEnd ).indices.iterator
    yield SrcDestPos( item, flowEnd, index, subIndex ) ).toVector

  def splitTypes( flows: Flows, pos: SrcDestPos ): Vector[SplitType] =
    Vector( SplitType.Even, SplitType.Remainder, SplitType.Max, SplitType.MaxAll )
    ++ pos
      .getOppositeSplits( flows.itemTransports )
      .map: oppositeFlowEnd =>
        SplitType.Opposite( oppositeFlowEnd )
    ++ ( pos.getLocal( flows.itemTransports ), pos.getSplit( flows.itemTransports ) )
      .mapN: ( itemTransport, split ) =>
        val amount = split.amount
        val unit   = itemTransport.transport.perMinute
        SplitType.Equal( Option.when( amount > unit )( ( amount / unit.toDouble ).ceil.toInt ) )

  def previewSplitResultConservesAmount( flows: Flows ): Unit =
    flowsPoses( flows )
      .mproduct( pos => splitTypes( flows, pos ) )
      .foreach:
        case ( pos, splitType ) =>
          assert(
            flows
              .previewSplitResultScaled( clue( pos ), clue( splitType ) )
              .forall( fracs => clue( fracs ).sum =~ 1.0d )
          )

  def storable( flows: Flows ): Unit =
    assert( clue( flows ) =~ Flows.from( flows.prod, clue( flows: pp.Flows ) ) )
