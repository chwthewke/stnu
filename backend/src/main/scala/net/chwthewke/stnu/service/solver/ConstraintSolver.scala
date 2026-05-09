package net.chwthewke.stnu
package service
package solver

import cats.syntax.all.*
import org.ojalgo.optimisation.Expression
import org.ojalgo.optimisation.ExpressionsBasedModel
import org.ojalgo.optimisation.Optimisation
import org.ojalgo.optimisation.Variable

import data.Countable
import model.ClockSpeed
import model.ClockSpeedPreset
import model.Item
import model.Recipe
import protocol.solver.BoostedRecipe
import protocol.solver.SolverRequest
import protocol.solver.SolverResponse

trait ConstraintSolver:
  def solve(
      requested: Vector[Countable[Double, Item]],
      recipes: Vector[BoostableRecipe],
      inputs: Map[ClassName[Item], SolverRequest.Resource],
      maxProductionBoost: Int,
      manufacturingClockSpeed: ClockSpeedPreset
  ): Either[SolverResponse.Error, SolverResponse.Solution]

object ConstraintSolver extends ConstraintSolver:

  private def itemVarName( item: ClassName[Item] ): String                             = show"I__$item"
  private def boostedRecipeVarName( recipe: ClassName[Recipe], boost: Int ): String    = show"B_${boost}_$recipe"
  private def boostedRecipeIntVarName( recipe: ClassName[Recipe], boost: Int ): String = show"C_${boost}_$recipe"
  private def boostedRecipeExpr( recipe: ClassName[Recipe], boost: Int ): String       = show"U_${boost}_$recipe"
  private def itemExprName( item: ClassName[Item] ): String                            = show"X__$item"
  private val shardsExprName: String                                                   = "S"

  case class BoostedRecipeVars(
      main: Variable,
      int: Variable
  )

  def solve(
      requested: Vector[Countable[Double, Item]],
      recipes: Vector[BoostableRecipe],
      inputs: Map[ClassName[Item], SolverRequest.Resource],
      maxProductionBoost: Int,
      manufacturingClockSpeed: ClockSpeedPreset
  ): Either[SolverResponse.Error, SolverResponse.Solution] =
    val model: ExpressionsBasedModel = new ExpressionsBasedModel

    def boostedRecipe( boostableRecipe: BoostableRecipe ): Option[BoostedRecipe[Recipe.NonExtraction]] =
      boostableRecipe.recipe match
        case r: Recipe.Manufacturing =>
          r.productionBoost.flatMap: boost =>
            Option.when( 0 < boostableRecipe.maxBoost && boostableRecipe.maxBoost <= maxProductionBoost ):
              val maxBoostedClockSpeedUnbounded: ClockSpeed =
                ClockSpeed.ofFraction(
                  boostableRecipe.maxClockSpeed.fraction / ( 1d + boostableRecipe.maxBoost * boost.effect )
                )
              val clockSpeed: ClockSpeed = maxBoostedClockSpeedUnbounded.min( manufacturingClockSpeed.value )
              BoostedRecipe( r, boost.slots, clockSpeed )
        case _: Recipe.PowerGeneration => None

    val recipeKeys: Vector[BoostedRecipe[Recipe.NonExtraction]] =
      recipes.foldMap: boostableRecipe =>
        Vector( BoostedRecipe( boostableRecipe.recipe, ClockSpeedPreset.`100%` ) ) // regular
          ++ boostedRecipe( boostableRecipe )                                      // boosted

    // println( s"RECIPES w/ boost\n  ${recipeKeys.map( _.map( _.className ) ).mkString( "\n  " )}" )

    val shardsExpr: Expression =
      model
        .addExpression( shardsExprName )
        .lower( 0 )
        .upper( maxProductionBoost )

    def mkRecipeVars( key: BoostedRecipe[Recipe.NonExtraction] ): Variable =
      val name: String   = boostedRecipeVarName( key.recipe.className, key.usedSlots )
      val weight: Double =
        key.recipe.powerConsumption( key.usedSlots, key.maxClockSpeed ).average * PowerWeight

      val mainVar: Variable = model.addVariable( name ).weight( weight ).lower( 0d )

      if ( key.usedSlots > 0 )
        val intVar: Variable =
          model
            .addVariable( boostedRecipeIntVarName( key.recipe.className, key.usedSlots ) )
            .lower( 0d )
            .integer()
            .weight( 1d ) // trying something
        model
          .addExpression( boostedRecipeExpr( key.recipe.className, key.usedSlots ) )
          .set( mainVar, -1d )
          .set( intVar, 1d )
          .lower( 0 ): Unit
        shardsExpr.set( intVar, key.usedSlots ): Unit

      mainVar

    val recipeVars1: Map[BoostedRecipe[ClassName[Recipe.NonExtraction]], Variable] =
      recipeKeys
        .map: recipe =>
          ( recipe.map( _.className ), mkRecipeVars( recipe ) )
        .toMap

    def inputVar( item: ClassName[Item], resource: SolverRequest.Resource ): Variable =
      resource.cap.foldLeft(
        model
          .addVariable( itemVarName( item ) )
          .lower( 0d )
          .weight( resource.weight )
      )( ( v, cap ) => v.upper( cap ) )

    val inputVars: Map[ClassName[Item], Variable] = inputs.map:
      case ( item, resource ) => ( item, inputVar( item, resource ) )

    val requestedByItem: Map[ClassName[Item], Double] =
      requested.foldMap:
        case Countable( item, amount ) => Map( ( item.className, amount ) )

    val itemExprs: Map[ClassName[Item], Expression] =
      ( recipes.foldMap( _._1.items.toSet.map( _.className ) ) ++ requested.map( _.item.className ) ).toVector
        .map: item =>
          (
            item,
            model
              .addExpression( itemExprName( item ) )
              .lower( requestedByItem.get( item ).orEmpty )
          )
        .toMap

    recipeKeys.foreach:
      case boosted @ BoostedRecipe( recipe, slots, clockSpeed ) =>
        boosted
          .itemsPerMinuteMap( clockSpeed )
          .foreach:
            case ( item, amount ) =>
              itemExprs( item.className ).set( recipeVars1( boosted.map( _.className ) ), amount )

    inputVars.foreach:
      case ( item, inputVar ) =>
        itemExprs
          .get( item )
          .foreach:
            _.set( inputVar, 1d )

    val result: Optimisation.Result = model.minimise()

    def extractValues[K]( vars: Map[K, Variable] ): Vector[Countable[Double, K]] =
      vars
        .flatMap:
          case ( k, v ) =>
            val amount = Option( v.getValue ).foldMap( _.doubleValue() )
            Option.when( amount != 0d )( Countable( k, amount ) )
        .toVector

    Option
      .when( result.getState.isSuccess ):
//        import org.ojalgo.structure.Access1D
//        import scala.jdk.CollectionConverters._
//        val variables                                 = model.getVariables.asScala
//        val variableValues                            = variables.map( _.getValue ).asJava
//        def symExpr( expression: Expression ): String =
//          expression.getLinearEntrySet.asScala.toVector
//            .map: e =>
//              val variable = variables( e.getKey.index )
//              s" ${e.getValue} * ${variable.getName} "
//            .mkString( "+" )
//
//        def showExpr( expression: Expression ): String =
//          s"""$expression
//             |    = ${symExpr( expression )}
//             |    = ${expression.evaluate( Access1D.wrap( variableValues ) )}
//             |""".stripMargin
//        println(
//          s"""MODEL: ${result.getState}
//             |VARS:
//             |  ${variables.map( v => s"${v.getName} = ${v.getValue}" ).mkString( "\n  " )}
//             |EXPRS:
//             |  ${model.getExpressions.asScala.map( showExpr ).mkString( "\n  " )}
//             |""".stripMargin
//        )

        val solutionRecipes: Vector[Countable[Double, BoostedRecipe[ClassName[Recipe.NonExtraction]]]] =
          extractValues( recipeVars1 ).map:
            case Countable( boosted, amount ) =>
              Countable( boosted, amount * boosted.maxClockSpeed.fraction )

        SolverResponse.Solution( extractValues( inputVars ), solutionRecipes )
      .toRight( SolverResponse.SolverError( result.getState.toString.toLowerCase ) )
  private val PowerWeight: Double = 1e-6
