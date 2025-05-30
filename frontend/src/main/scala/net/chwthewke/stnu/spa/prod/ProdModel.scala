package net.chwthewke.stnu
package spa
package prod

import cats.data.Ior
import cats.data.NonEmptyList
import cats.syntax.all.*
import scala.annotation.tailrec
import scala.collection.immutable.SortedMap

import data.Countable
import model.ExtractionRecipes
import model.ExtractorType
import model.Item
import model.Machine
import model.Recipe
import model.ResourceDistrib
import model.Transport
import protocol.solver
import protocol.solver.SolverRequest
import protocol.solver.SolverResponse
import spa.plan.ExtractionOptions
import spa.plan.RequestSelectionModel
import spa.prod.ProdModel.Solution

case class ProdModel(
    env: Env,
    requestSelection: RequestSelectionModel,
    solution: Option[ProdModel.Solution],
    resourceNodes: Map[ExtractorType, Map[ClassName[Item], ResourceDistrib]],
    extractionOptions: ExtractionOptions,
    belts: NonEmptyList[Transport],
    pipelines: NonEmptyList[Transport],
    expandedRecipe: Option[ClassName[Recipe]]
):
  private lazy val result: Option[Solution.Result] = solution.flatMap( _.result )

  lazy val currentRequest: Vector[Countable[Double, Item]] =
    requestSelection.requested.mapFilter( _.traverse( env.getItem ) )

  def dirtyRequestRows: List[Countable[Double, Item]] =
    val currentRequestMap = currentRequest.fproductLeft( _.item.className ).toMap
    val solvedRequestMap  = solution.flatMap( _.result ).foldMap( _.request.fproductLeft( _.item.className ) ).toMap
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
        candidates: List[( Recipe.Extraction, Option[Int] )],
        amount: Double
    ): ( List[ClockedRecipe], List[Countable[Double, Item]] ) =
      candidates match
        case Nil                                        => ( acc, item.withAmount( amount ).significant.toList )
        case ( recipe, maxCountOpt ) :: otherCandidates =>
          val perMinute: Double =
            recipe.productsPerMinute.find( _.item.className == item.item.className ).foldMap( _.amount )
          def available( count: Int ): Double = perMinute * extractionOptions.clockSpeed.value.fraction * count

          maxCountOpt.filter( available( _ ) < amount ) match
            case Some( maxCount ) =>
              loop(
                Countable( recipe, maxCount.toDouble ).significant
                  .map( ClockedRecipe.overclocked( _, extractionOptions.clockSpeed ) ) ++: acc,
                otherCandidates,
                amount - perMinute * maxCount
              )
            case None =>
              (
                Countable( recipe, amount / perMinute ).significant
                  .map( ClockedRecipe.overclocked( _, extractionOptions.clockSpeed ) ) ++: acc,
                Nil
              )

    loop( Nil, extractionRecipesFor( item.item ), item.amount )

  def extractionRecipesFor( item: Item ): List[( Recipe.Extraction, Option[Int] )] =

    def allowedRecipes(
        machine: Machine,
        extractionRecipes: ExtractionRecipes
    ): List[( Recipe.Extraction, Option[Int] )] =
      extractionRecipes match
        case ExtractionRecipes.Fixed( recipe )      => ( recipe, None ) :: Nil
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
              case ( purity, recipe ) => ( recipe, Some( distrib.get( purity ) ) )
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

  private lazy val productionRows: Vector[ClockedRecipe] =
    result
      .foldMap( _.recipes.toVector )
      .filter( _.isSignificant )
      .map( ClockedRecipe.roundUp )

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

  lazy val rows: List[ClockedRecipe] =
    val extracted: Set[ClassName[Item]] = extractionRows.foldMap( _.recipe.productsList.map( _.item.className ).toSet )
    extractionRows ++ sort( extracted, productionRows, Nil )
    
  lazy val itemIO: SortedMap[Item, ItemIO[SrcDest]] = ItemIO.of(
    rows,
    currentRequest.map { case Countable( item, amount ) => ( item, amount ) }.toMap
  )

object ProdModel:
  enum Solution:
    case Result(
        request: Vector[Countable[Double, Item]],
        inputs: Vector[Countable[Double, Item]],
        recipes: Vector[Countable[Double, Recipe.NonExtraction]]
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
            recipes.mapFilter( _.traverse( env.getRecipe ) )
          )
        case SolverResponse.InvalidModelVersion       => Failure( dataErrorModelVersion )
        case SolverResponse.InvalidClasses( classes ) => Failure( dataErrorClasses( classes ) )
        case SolverResponse.SolverError( state )      => Failure( solverError( state ) )

  private val frackingLast: Ordering[Machine] =
    def isFracking( m: Machine ): Boolean = m.machineType.extractor.contains( ExtractorType.Fracking )
    Ordering.by( isFracking )
