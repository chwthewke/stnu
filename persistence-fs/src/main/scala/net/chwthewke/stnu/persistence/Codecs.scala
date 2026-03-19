package net.chwthewke.stnu
package persistence

import cats.InvariantSemigroupal
import cats.syntax.all.*
import fs2.io.file.Path
import java.time.Instant
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet
import scodec.Attempt
import scodec.Codec
import scodec.Err
import scodec.codecs

import model.ClockSpeedPreset
import model.ExtractorType
import model.Item
import model.Machine
import model.Recipe
import model.ResourceDistrib
import model.Transport
import model.prod.FlowEnd
import model.prod.Group
import protocol.persistence.EndId
import protocol.persistence.ExtractionOptions
import protocol.persistence.Flows
import protocol.persistence.LogisticsOptions
import protocol.persistence.Plan
import protocol.persistence.PlanId
import protocol.persistence.PlanName
import protocol.persistence.PlanSummary
import protocol.persistence.PowerOptions
import protocol.persistence.ProcessSplitId
import protocol.persistence.ProductionUi
import protocol.persistence.RecipeOptions
import protocol.persistence.RequestSelection
import protocol.persistence.ResourceOptions

object Codecs:
  private given InvariantSemigroupal[Codec]:
    override def imap[A, B]( fa: Codec[A] )( f: A => B )( g: B => A ): Codec[B] = fa.xmap( f, g )

    override def product[A, B]( fa: Codec[A], fb: Codec[B] ): Codec[( A, B )] = fa :: fb

  // do not use this if you ever want to add new enum members (except at the end)
  def enumOrdinal[A]( numCodec: Codec[Int], ev: CustomEnum[A] ): Codec[A] =
    numCodec.exmap(
      i => Attempt.fromOption( ev.cases.lift( i ), Err( s"$i not in [0;${ev.cases.length - 1}]" ) ),
      a => Attempt.successful( ev.indexOf( a ) )
    )

  def enumName[A]( ev: CustomEnum[A] ): Codec[A] =
    codecs.utf8_32.exmap(
      s => Attempt.fromEither( ev.withNameEither( s ).leftMap( Err( _ ) ) ),
      a => Attempt.successful( ev.keyOf( a ) )
    )

  def setOfN[A]( countCodec: Codec[Int], itemCodec: Codec[A] ): Codec[Set[A]] =
    codecs.vectorOfN( countCodec, itemCodec ).xmap( _.toSet, _.toVector )

  def sortedSetOfN[A: Ordering]( countCodec: Codec[Int], itemCodec: Codec[A] ): Codec[SortedSet[A]] =
    codecs.vectorOfN( countCodec, itemCodec ).xmap( _.to( SortedSet ), _.toVector )

  val path: Codec[Path] = codecs.utf8_32.xmap( Path( _ ), _.toString )

  val instant: Codec[Instant] = codecs.zlong.xmap( Instant.ofEpochMilli, _.toEpochMilli )

  def className[A]: Codec[ClassName[A]] = codecs.utf8_32.xmap( ClassName( _ ), _.name )

  val planId: Codec[PlanId] = codecs.int32.xmap( PlanId( _ ), _.id )

  val planName: Codec[PlanName] = codecs.utf8_32.xmap( PlanName( _ ), _.name )

  val planSummary: Codec[PlanSummary] =
    val requested: Codec[Vector[( ClassName[Item], Double )]] =
      codecs.vectorOfN( codecs.int16, className[Item] :: codecs.double )
    ( planId, planName, instant, requested ).imapN( PlanSummary.apply )( Tuple.fromProductTyped )

  val recipeOptions: Codec[RecipeOptions] =
    val allowedRecipes: Codec[Set[ClassName[Recipe.Manufacturing]]] =
      setOfN( codecs.int16, className[Recipe.Manufacturing] )
    ( codecs.bool, allowedRecipes ).imapN( RecipeOptions.apply )( Tuple.fromProductTyped )

  val extractorType: Codec[ExtractorType] = enumName( ExtractorType )

  val resourceDistrib: Codec[ResourceDistrib] =
    ( codecs.int16, codecs.int16, codecs.int16 ).imapN( ResourceDistrib.apply )( Tuple.fromProductTyped )

  val resourceOptions: Codec[ResourceOptions] =
    val vectorCodec: Codec[Vector[( ExtractorType, Vector[( ClassName[Item], ResourceDistrib )] )]] =
      codecs.vectorOfN(
        codecs.int8,
        extractorType :: codecs.vectorOfN( codecs.int16, className[Item] :: resourceDistrib )
      )
    vectorCodec.xmap(
      vector => ResourceOptions( vector.iterator.map { case ( ex, items ) => ( ex, items.toMap ) }.toMap ),
      _.resourceNodes.iterator.map { case ( ex, items ) => ( ex, items.toVector ) }.toVector
    )

  val extractionOptions: Codec[ExtractionOptions] =
    val resourceWeightSliders: Codec[Map[ClassName[Item], Int]] =
      codecs.vectorOfN( codecs.uint8, className[Item] :: codecs.int8 ).xmap( _.toMap, _.toVector )

    (
      className[Machine],
      enumName( ClockSpeedPreset ),
      setOfN( codecs.uint8, extractorType ),
      setOfN( codecs.uint16, className[Item] ),
      resourceWeightSliders
    ).imapN( ExtractionOptions.apply )( Tuple.fromProductTyped )

  val logisticsOptions: Codec[LogisticsOptions] =
    (
      codecs.optional( codecs.bool, className[Transport] ),
      codecs.optional( codecs.bool, className[Transport] ),
      sortedSetOfN( codecs.uint8, className[Transport] ),
      sortedSetOfN( codecs.uint8, className[Transport] )
    ).imapN( LogisticsOptions.apply )( Tuple.fromProductTyped )

  val powerOptions: Codec[PowerOptions] =
    setOfN( codecs.uint8, className[Machine] ).xmap( PowerOptions( _ ), _.allowedGenerators )

  val requestSelection: Codec[RequestSelection] =
    codecs
      .vectorOfN( codecs.uint16, className[Item] :: codecs.double )
      .xmap( vector => RequestSelection( vector.to( SortedMap ) ), _.requestedAmounts.toVector )

  val processSplitId: Codec[ProcessSplitId] =
    codecs.int32.xmap( ProcessSplitId( _ ), _.id )

  val productionUi: Codec[ProductionUi] =
    (
      codecs.optional( codecs.bool, codecs.vectorOfN( codecs.int32, processSplitId ) ),
      codecs.vectorOfN( codecs.int32, processSplitId )
    ).imapN( ProductionUi.apply )( Tuple.fromProductTyped )

  val group: Codec[Group] =
    codecs.vectorOfN( codecs.uint8, codecs.uint16 ).xmap( Group( _ ), _.path )

  val flowEnd: Codec[FlowEnd] =
    codecs.bool.xmap( b => if ( b ) FlowEnd.Source else FlowEnd.Destination, _ == FlowEnd.Source )

  val endId: Codec[EndId] =
    val process: Codec[EndId.Process]     = className[Recipe].xmap( EndId.Process( _ ), _.recipe )
    val input: Codec[EndId.Input]         = className[Item].xmap( EndId.Input( _ ), _.item )
    val requested: Codec[EndId.Requested] = className[Item].xmap( EndId.Requested( _ ), _.item )
    val byproduct: Codec[EndId.Byproduct] = className[Item].xmap( EndId.Byproduct( _ ), _.item )

    codecs
      .discriminated[EndId]
      .by( codecs.uint2 )
      .typecase( 0, process )
      .typecase( 1, input )
      .typecase( 2, requested )
      .typecase( 3, byproduct )

  val flows: Codec[Flows] =
    val endSplits: Codec[Vector[( EndId, Vector[( ProcessSplitId, Double, Group )] )]] =
      codecs.vectorOfN(
        codecs.uint16,
        endId :: codecs.vectorOfN( codecs.int32, processSplitId :: codecs.double :: group )
      )
    val itemFlows: Codec[Map[ClassName[Item], Vector[Vector[( FlowEnd, ProcessSplitId )]]]] =
      codecs
        .vectorOfN(
          codecs.int32,
          className[Item] ::
            codecs.vectorOfN( codecs.int32, codecs.vectorOfN( codecs.int32, flowEnd :: processSplitId ) )
        )
        .xmap( _.toMap, _.toVector )
    ( codecs.int32, processSplitId, endSplits, itemFlows ).imapN( Flows.apply )( Tuple.fromProductTyped )

  val plan: Codec[Plan] =
    (
      planName,
      recipeOptions,
      resourceOptions,
      extractionOptions,
      logisticsOptions,
      powerOptions,
      requestSelection,
      flows,
      productionUi
    ).imapN( Plan.apply )( Tuple.fromProductTyped )
