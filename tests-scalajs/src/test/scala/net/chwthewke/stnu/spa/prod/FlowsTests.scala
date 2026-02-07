package net.chwthewke.stnu
package spa.prod

import cats.Show
import cats.data.NonEmptyList
import cats.syntax.all.*
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.cats.implicits.*

import model.Tier

class FlowsTests extends ScalaCheckSuite with GameFixture with FlowsProperties:

  import FlowsTests.*

  def initialFlows(
      maxTierGen: Gen[Tier] = Gen.choose( 2, 9 ).map( Tier( _ ) ),
      requestSizeGen: Gen[Int] = Gen.choose( 1, 20 )
  ): Gen[Flows] =
    Gen
      .delay( // don't wanna hit defaultEnv/defaultSolutions before fixture init
        ProdModelGenerators
          .prodModel( defaultEnv, defaultSolutions )( maxTierGen = maxTierGen, requestSizeGen = requestSizeGen )
      )
      .map( prod => Flows.init( prod ) )

  val initialFlowsSetup: Gen[FlowsSetup] =
    initialFlows().map( InitialFlows( _ ) )

  def moveFrom( flows: Flows ): Gen[Op] =
    ( Gen.oneOf( flowsPoses( flows ) ), Gen.oneOf( -1, 1 ), Gen.oneOf( false, true ) ).mapN( Op.Move( _, _, _ ) )

  def splitOf( flows: Flows ): Gen[Option[Op]] =

    val splits =
      ( for
        pos       <- flowsPoses( flows ).iterator
        action    <- flows.splitActionModal( pos ).iterator
        splitType <-
          ( action.even :: action.equal :: action.remainder :: action.max :: action.maxAll :: action.opposite ).iterator
        if flows.previewSplit( pos, splitType ).nonEmpty
      yield Op.Split( pos, splitType ) ).toVector

    if ( splits.isEmpty ) Gen.const( none )
    else Gen.oneOf( splits ).map( _.some )

  def mergeOf( flows: Flows ): Gen[Option[Op]] =

    val merges =
      ( for
        pos       <- flowsPoses( flows ).iterator
        action    <- flows.mergeActionModal( pos ).iterator
        mergeType <- ( action.local :: action.global :: action.adjacent ).iterator
        if flows.previewMerge( pos, mergeType ).nonEmpty
      yield Op.Merge( pos, mergeType ) ).toVector

    if ( merges.isEmpty ) Gen.const( none )
    else Gen.oneOf( merges ).map( _.some )

  def afterMove( flowsGen: Gen[Flows] = initialFlows( requestSizeGen = Gen.choose( 2, 20 ) ) ): Gen[AfterOps] =
    for
      initial <-
        flowsGen
          .suchThat: flows =>
            flows.itemFlows.values.exists( _.exists( it => it.sources.length > 1 || it.destinations.length > 1 ) )
      move <- moveFrom( initial )
    yield AfterOps.single( initial, move )

  def afterSplit( flowsGen: Gen[Flows] = initialFlows( requestSizeGen = Gen.choose( 6, 20 ) ) ): Gen[AfterOps] =
    for
      initial <- flowsGen
                   .suchThat( flows => flowsPoses( flows ).exists( p => flows.canSplit( p ) ) )
      splitOpt <- splitOf( initial )
      split    <- splitOpt.fold( Gen.fail )( Gen.const )
    yield AfterOps.single( initial, split )

  def afterMerge1Split(
      flowsGen: Gen[Flows] = initialFlows( requestSizeGen = Gen.choose( 6, 20 ) )
  ): Gen[FlowsSetup] =
    for
      splitSetup <- afterSplit( flowsGen )
      mergeOpt   <- mergeOf( splitSetup.flows )
      merge      <- mergeOpt.fold( Gen.fail )( Gen.const )
    yield splitSetup.andThen( merge )

  def afterSplits(
      countGen: Gen[Int] = Gen.choose( 1, 5 ), // >= 1
      flowsGen: Gen[Flows] = initialFlows( requestSizeGen = Gen.choose( 6, 20 ) )
  ): Gen[AfterOps] =
    ( countGen, afterSplit( flowsGen ) ).flatMapN: ( count, initial ) =>
      ( count, initial ).tailRecM:
        case ( n, setup ) =>
          if ( n <= 1 ) Gen.const( Right( setup ) )
          else
            for
              splitOpt <- splitOf( setup.flows )
              split    <- splitOpt.fold( Gen.fail )( Gen.const )
            yield Left( ( n - 1, setup.andThen( split ) ) )

  def afterMergeNSplits(
      flowsGen: Gen[Flows] = initialFlows( requestSizeGen = Gen.choose( 6, 20 ) ),
      splitsGen: Gen[Int] = Gen.choose( 2, 5 )
  ): Gen[AfterOps] =
    for
      splitSetup <- afterSplits( splitsGen, flowsGen )
      mergeOpt   <- mergeOf( splitSetup.flows )
      merge      <- mergeOpt.fold( Gen.fail )( Gen.const )
    yield splitSetup.andThen( merge )

  checkFlowsProperties( "initial flows", initialFlowsSetup, commonFlowsProperties ++ freshFlowsProperties )

  checkFlowsProperties( "after move", afterMove() )

  checkFlowsProperties( "after split", afterSplit() )

  checkFlowsProperties( "after splits", afterSplits() )

  checkFlowsProperties( "after merge (1 split)", afterMerge1Split() )

  checkFlowsProperties( "after merge (2-5 splits)", afterMergeNSplits() )

object FlowsTests:
  case class InitialFlows( flows: Flows ) extends FlowsSetup:
    override def toString: String = s"INITIAL ${ShownFlows( flows )}"

  enum Op:
    case Move( pos: SrcDestPos, amount: Int, bump: Boolean )
    case Split( pos: SrcDestPos, splitType: SplitType )
    case Merge( pos: SrcDestPos, mergeType: MergeType )

    def apply( flows: Flows ): Flows =
      this match
        case Move( pos, amount, bump ) => flows.move( pos, amount, bump )
        case Split( pos, splitType )   => flows.split( pos, splitType )
        case Merge( pos, mergeType )   => flows.merge( pos, mergeType )

    override def toString: String =
      this match
        case Op.Move( pos, amount, false ) => s"MOVE @ $pos BY $amount"
        case Op.Move( pos, amount, true )  => s"BUMP @ $pos BY $amount"
        case Op.Split( pos, splitType )    => s"SPLIT @ $pos WITH $splitType"
        case Op.Merge( pos, mergeType )    => s"MERGE @ $pos WITH $mergeType"

  object Op:
    given Show[Op] = Show.fromToString

  case class AfterOps( initial: Flows, previous: Flows, ops: NonEmptyList[Op] ) extends FlowsSetup:
    override def flows: Flows = ops.head( previous )

    override def toString: String =
      s"""AFTER ${ops.reverse.mkString_( ", " )}
         |FINAL ${ShownFlows( flows )}
         |
         |${Option.when( previous ne initial )( s"PREVIOUS ${ShownFlows( previous )}" ).orEmpty}
         |
         |INITIAL ${ShownFlows( initial )}
         |""".stripMargin

    def andThen( op: Op ): AfterOps = AfterOps( initial, flows, op :: ops )

  object AfterOps:
    def single( initial: Flows, op: Op ): AfterOps =
      AfterOps( initial, initial, NonEmptyList.one( op ) )
