package net.chwthewke.stnu
package spa
package prod

import cats.Foldable
import cats.Monoid
import cats.data.Ior
import cats.data.NonEmptyList
import cats.syntax.all.*
import scala.annotation.tailrec
import scala.collection.Factory
import scala.util.hashing.MurmurHash3

import data.Countable
import model.ClockSpeedPreset
import model.ExtractionRecipes
import model.ExtractorType
import model.Form
import model.Item
import model.Machine
import model.Recipe
import model.ResourceDistrib
import model.Transport
import model.prod.Group
import protocol.persistence.ProcessSplitId
import protocol.solver.BoostedRecipe
import protocol.solver.SolverRequest
import protocol.solver.SolverResponse
import spa.plan.ExtractionOptions
import spa.plan.RequestsModel
import spa.prod.ProdModel.Solution

case class ProdModel(
    env: Env,
    requestSelection: RequestsModel,
    solution: Option[ProdModel.Solution],
    resourceNodes: Map[ExtractorType, Map[ClassName[Item], ResourceDistrib]],
    extractionOptions: ExtractionOptions,
    belts: NonEmptyList[Transport],
    pipelines: NonEmptyList[Transport]
):
  lazy val result: Option[Solution.Result] = solution.flatMap( _.result )

  lazy val dirtyRequestRows: List[Countable[Double, Item]] =
    val currentRequestMap =
      requestSelection.requested.mapFilter( _.traverse( env.getItem ) ).fproductLeft( _.item.className ).toMap
    val solvedRequestMap = solution.flatMap( _.result ).foldMap( _.request.fproductLeft( _.item.className ) ).toMap
    currentRequestMap
      .align( solvedRequestMap )
      .mapFilter:
        case Ior.Left( r )    => r.some
        case Ior.Right( s )   => s.mapAmount( -_ ).some
        case Ior.Both( r, s ) => r.mapAmount( _ - s.amount ).significant
      .values
      .toList
      .sorted( env.itemOrder.contramap( _.item ) )

  lazy val ( extractionRows: List[ClockedRecipe], otherInputs: List[Countable[Double, Item]] ) =
    result.foldMap( _.inputs.sortBy( _.item )( env.itemOrder ).foldMap( extractionRowsFor ) )

  private def extractionRowsFor(
      item: Countable[Double, Item]
  ): ( List[ClockedRecipe], List[Countable[Double, Item]] ) =
    @tailrec
    def loop(
        acc: List[ClockedRecipe],
        candidates: List[( Recipe.Extraction, Option[Int], ClockSpeedPreset.Extraction )],
        amount: Double
    ): ( List[ClockedRecipe], List[Countable[Double, Item]] ) =
      candidates match
        case Nil => ( acc, item.withAmount( amount ).significant.toList )
        case ( recipe, maxCountOpt, maxClockSpeed ) :: otherCandidates =>
          val perMinute: Double =
            recipe.productsPerMinute.find( _.item.className == item.item.className ).foldMap( _.amount )
          def available( count: Int ): Double = perMinute * maxClockSpeed.value.fraction * count

          def toClockedRecipe( cr: Countable[Double, Recipe.Extraction] ): Option[ClockedRecipe] =
            cr.significant.map( scr => ClockedRecipe.overclocked( scr.map( BoostedRecipe( _, maxClockSpeed ) ) ) )

          maxCountOpt.filter( available( _ ) < amount ) match
            case Some( maxCount ) =>
              loop(
                toClockedRecipe( Countable( recipe, maxCount.toDouble ) ) ++: acc,
                otherCandidates,
                amount - perMinute * maxCount
              )
            case None =>
              (
                toClockedRecipe( Countable( recipe, amount / perMinute ) ) ++: acc,
                Nil
              )

    loop( Nil, extractionRecipesFor( item.item ), item.amount )

  def extractionRecipesFor( item: Item ): List[( Recipe.Extraction, Option[Int], ClockSpeedPreset.Extraction )] =

    def allowedRecipes(
        machine: Machine,
        extractionRecipes: ExtractionRecipes
    ): List[( Recipe.Extraction, Option[Int], ClockSpeedPreset.Extraction )] =
      val clockSpeed =
        machine.machineType.extractor match
          case Some( ExtractorType.WaterPump ) =>
            if ( extractionOptions.excludeWaterPumpFromOverclocking )
              ClockSpeedPreset.`100%`
            else extractionOptions.clockSpeed
          case Some( ExtractorType.FicsmasTree ) => ClockSpeedPreset.`100%`
          case _                                 => extractionOptions.clockSpeed

      extractionRecipes match
        case ExtractionRecipes.Fixed( recipe ) =>
          ( recipe, None, clockSpeed ) :: Nil
        case ExtractionRecipes.Variable( byPurity ) =>
          val extractorAllowed =
            machine.machineType.extractor match
              case Some( ExtractorType.Miner ) =>
                extractionOptions.extractors.contains( ExtractorType.Miner ) &&
                machine.className == extractionOptions.minerClass
              case Some( other ) => extractionOptions.extractors.contains( other )
              case None          => false

          if ( extractorAllowed )
            val distrib: ResourceDistrib =
              machine.machineType.extractor.flatMap( resourceNodes.get ).flatMap( _.get( item.className ) ).orEmpty
            byPurity.toMap.toList.reverse.map:
              case ( purity, recipe ) => ( recipe, Some( distrib.get( purity ) ), clockSpeed )
          else Nil

    val machineTypeOrdering: Ordering[Machine] =
      if ( extractionOptions.preferFracking( item.className ) ) ProdModel.frackingLast.reverse
      else ProdModel.frackingLast

    env.game.extractionRecipes
      .collect:
        case ( ( `item`, machine ), recipes ) => ( machine, recipes )
      .toVector
      .sortBy( _._1 )( machineTypeOrdering )
      .foldMap:
        case ( machine, recipes ) => allowedRecipes( machine, recipes )

  lazy val extractedItems: List[Countable[Double, Item]] =
    result.foldMap( _.inputs.sortBy( _.item.displayName ).toList )

  private lazy val manufacturingRows: Vector[ClockedRecipe] =
    result
      .foldMap( _.recipes.toVector )
      .filter( _.isSignificant )
      .map( ClockedRecipe.overclocked )

  lazy val manufacturingResources: Map[ClockedRecipe, Double] =
    ResourceAttribution( env, extractedItems.toVector, manufacturingRows )

  @tailrec
  private def sort(
      feasible: Set[ClassName[Item]],
      toSort: Vector[ClockedRecipe],
      acc: List[ClockedRecipe]
  ): List[ClockedRecipe] =
    if ( toSort.isEmpty ) acc
    else
      val ( next: Vector[ClockedRecipe], rest: Vector[ClockedRecipe] ) =
        toSort.partition( _.recipe.ingredients.forall( ci => feasible( ci.item.className ) ) )
      if ( next.isEmpty ) acc ++ toSort
      else
        val nextFeasible: Set[ClassName[Item]] =
          feasible ++ next.foldMap( _.recipe.productsList.map( _.item.className ).toSet )
        sort( nextFeasible, rest, acc ++ next )

  lazy val productionRows: List[ClockedRecipe] =
    val extracted: Set[ClassName[Item]] = extractionRows.foldMap( _.recipe.productsList.map( _.item.className ).toSet )
    extractionRows ++ sort( extracted, manufacturingRows, Nil )

  lazy val requested: Map[Item, Double] =
    solution.flatMap( _.result ).foldMap( _.request.map( ci => ( ci.item, ci.amount ) ) ).toMap

  def selectTransport( item: Item, amount: Double ): Countable[Double, Transport] =
    @tailrec
    def loop( l: NonEmptyList[Transport] ): Transport =
      if ( l.head.perMinute.toDouble + Countable.Tolerance >= amount ) l.head
      else
        l.tail.toNel match
          case Some( tail ) => loop( tail )
          case None         => l.head
    val transport =
      if ( item.form == Form.Solid )
        loop( belts )
      else
        loop( pipelines )
    Countable( transport, amount / transport.perMinute )

  def bestTransport( item: Item ): Transport =
    if ( item.form == Form.Solid ) belts.last
    else pipelines.last

object ProdModel:
  type Hash = Int

  // a hash to check whether a `Flows` is valid for a ProdModel
  def solutionHash( model: ProdModel ): Hash =
    MurmurHash3.caseClassHash(
      (
        model.env.game.version.version,
        model.solution
          .flatMap( _.result )
          .foldMap: solution =>
            MurmurHash3.caseClassHash(
              (
                MurmurHash3.unorderedHash( solution.request.map( ci => ci.item.className ) ),
                MurmurHash3.unorderedHash( solution.inputs.map( ci => ci.item.className ) ),
                MurmurHash3.unorderedHash(
                  solution.recipes
                    .map( _.item )
                    .map( br =>
                      if ( br.usedSlots == 0 )
                        br.recipe.className
                      else
                        ClassName[Recipe.NonExtraction]( show"${br.recipe.className}_${br.usedSlots}" )
                    )
                )
              )
            )
      )
    )

  enum Solution:
    case Result(
        request: Vector[Countable[Double, Item]],
        inputs: Vector[Countable[Double, Item]],
        recipes: Vector[Countable[Double, BoostedRecipe[Recipe.NonExtraction]]]
    )
    case Failure( solverMessage: String )

    def result: Option[Result] = this match
      case r: Solution.Result => r.some
      case _                  => none

  object Solution:
    private val cacheMessage: String  = "Try clearing the cache and reloading."
    private val dataErrorModelVersion = s"Application error (Invalid model version). $cacheMessage"
    private def dataErrorClasses( classes: NonEmptyList[ClassName[Any]] ) =
      classes.mkString_( "Application error: Invalid classes ", ", ", s". $cacheMessage" )
    private def solverError( state: String ): String =
      show"Solver state <$state>. Try adding recipes, lowering amounts or increasing resource nodes."

    def apply( env: Env, request: SolverRequest, response: SolverResponse ): Solution =
      response match
        case SolverResponse.Solution( inputs, recipes ) =>
          Result(
            request.requested.mapFilter( _.traverse( env.getItem ) ),
            inputs.mapFilter( _.traverse( env.getItem ) ),
            recipes.mapFilter( _.traverse( _.traverse( env.getRecipe ) ) )
          )
        case SolverResponse.InvalidModelVersion       => Failure( dataErrorModelVersion )
        case SolverResponse.InvalidClasses( classes ) => Failure( dataErrorClasses( classes ) )
        case SolverResponse.SolverError( state )      => Failure( solverError( state ) )

  private val frackingLast: Ordering[Machine] =
    def isFracking( m: Machine ): Boolean = m.machineType.extractor.contains( ExtractorType.Fracking )
    Ordering.by( isFracking )

  case class Row(
      process: ClockedRecipe,
      splitId: ProcessSplitId,
      splitNumber: Int,
      splitCount: Int,
      fraction: Double,
      group: Group
  ):
    def end: EndId = EndId.process( process )

  case class Ui(
      prodHash: Option[ProdModel.Hash],
      productionSummaryExpanded: Boolean,
      expandedGroupSummaries: Set[Group],
      detailedGroupSummaries: Set[Group],
      expandedRows: Set[ProcessSplitId],
      rowOrder: Option[Vector[ProcessSplitId]],
      completed: Set[ProcessSplitId],
      showAllFlows: Boolean
  ):
    def setProductionSummaryExpanded( isOpen: Boolean ): Ui =
      copy( productionSummaryExpanded = isOpen )

    extension [A]( self: Set[A] )
      def toggle( elt: A ): Set[A] =
        if ( self.contains( elt ) ) self.excl( elt ) else self.incl( elt )

    def toggleGroupSummaryExpanded( group: Group ): Ui =
      copy( expandedGroupSummaries = expandedGroupSummaries.toggle( group ) )

    def toggleGroupSummaryFlat( group: Group ): Ui =
      copy( detailedGroupSummaries = detailedGroupSummaries.toggle( group ) )

    def setFlows( flows: Flows ): Ui =
      if ( prodHash.contains( flows.prodHash ) )
        val rows: Vector[Row]                    = Ui.initRowOrder[Vector]( flows )
        val rowIndexSet: Set[ProcessSplitId]     = rows.iterator.map( _.splitId ).toSet
        val currentOrderSet: Set[ProcessSplitId] = rowOrder.foldMap( _.toSet )
        val unsortedRows: Vector[ProcessSplitId] = rows.iterator.map( _.splitId ).filterNot( currentOrderSet ).toVector
        val flowsGroups: Set[Group]              = flows.groups

        copy(
          expandedRows = expandedRows.filter( rowIndexSet ),
          expandedGroupSummaries = expandedGroupSummaries.intersect( flowsGroups ),
          detailedGroupSummaries = detailedGroupSummaries.intersect( flowsGroups ),
          rowOrder = rowOrder.map( _.filter( rowIndexSet ) ++ unsortedRows ),
          completed = completed.filter( rowIndexSet )
        )
      else Ui.init.copy( prodHash = flows.prodHash.some )

    def toggleProductionRowExpanded( row: ProcessSplitId ): Ui =
      copy( expandedRows = expandedRows.toggle( row ) )

    private def nthIndexWhere[A]( vector: Vector[A] )( n: Int, pred: A => Boolean, from: Int ): Option[Int] =
      val ( s, v ) = if ( n < 0 ) ( -1, -n ) else ( 1, n )
      @tailrec
      def loop( ix: Int, lastMatch: Option[Int], matchCount: Int ): Option[Int] =
        if ( ix < 0 ) lastMatch
        else if ( pred( vector( ix ) ) )
          if ( matchCount >= v - 1 )
            ix.some
          else loop( ix + s, ix.some, matchCount + 1 )
        else loop( ix + s, lastMatch, matchCount )

      loop( from + s, none, 0 )

    // moves the element at index from, on the other side of the element at index beyond
    private def moveElement[A]( vector: Vector[A], from: Int, beyond: Int ): Vector[A] =
      if ( beyond > from )
        vector
          .patch( beyond + 1, Vector( vector( from ) ), 0 )
          .patch( from, Nil, 1 )
      else
        vector
          .patch( from, Nil, 1 )
          .patch( beyond, Vector( vector( from ) ), 0 )

    def moveProductionRow( rows: Vector[Row] )( index: ProcessSplitId, amount: Int ): Ui =
      val rowMap: Map[ProcessSplitId, Row] = rows.fproductLeft( _.splitId ).toMap
      val order: Vector[ProcessSplitId]    = rowOrder.getOrElse( rows.map( _.splitId ) )

      ( for
        row     <- rowMap.get( index )
        current <- order.indexOf( index ).some.filter( _ >= 0 )
        target  <- nthIndexWhere( order )( amount, rowMap.get( _ ).exists( _.group == row.group ), current )
      yield copy( rowOrder = moveElement( order, current, target ).some ) ).getOrElse( this )

    def toggleMarkComplete( row: ProcessSplitId ): Ui =
      copy( completed = if ( completed( row ) ) completed.excl( row ) else completed.incl( row ) )

    def setShowAllFlows( enable: Boolean ): Ui =
      copy( showAllFlows = enable )

  object Ui:
    val init: Ui = Ui( none, false, Set.empty, Set.empty, Set.empty, none, Set.empty, showAllFlows = true )

    def initRowOrder[F[_]: Foldable]( flows: Flows )( using CC: Factory[Row, F[Row]], M: Monoid[F[Row]] ): F[Row] =
      flows.prod.productionRows
        .foldMap: process =>
          val endId = EndId.process( process )
          flows.endSplits
            .get( endId )
            .foldMap: splits =>
              splits.splits.iterator.zipWithIndex
                .map:
                  case ( ( splitId, ( fraction, group ) ), ix ) =>
                    Row( process, splitId, ix + 1, splits.splits.size, fraction, group )
                .to( CC )

    given Conversion[Ui, pp.ProductionUi]:
      override def apply( ui: Ui ): pp.ProductionUi =
        pp.ProductionUi( ui.rowOrder, ui.completed.toVector )

    def from( prodHash: ProdModel.Hash, ui: pp.ProductionUi ): Ui =
      Ui(
        prodHash.some,
        false,
        Set.empty,
        Set.empty,
        Set.empty,
        ui.productionRowOrder,
        ui.complete.toSet,
        showAllFlows = true
      )
