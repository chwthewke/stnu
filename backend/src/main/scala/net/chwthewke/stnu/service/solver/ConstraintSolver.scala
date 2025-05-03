package net.chwthewke.stnu
package service
package solver

import cats.data.EitherT
import cats.effect.Sync
import cats.syntax.all.*
import org.ojalgo.optimisation.Expression
import org.ojalgo.optimisation.ExpressionsBasedModel
import org.ojalgo.optimisation.Optimisation
import org.ojalgo.optimisation.Variable

import data.Countable
import model.Item
import model.Recipe
import protocol.solver.SolverRequest
import protocol.solver.SolverResponse

class ConstraintSolver[F[_]]( using F: Sync[F] ):

  def solve(
      requested: Vector[Countable[Double, Item]],
      recipes: Vector[Recipe],
      inputs: Map[ClassName[Item], SolverRequest.Resource]
  ): EitherT[F, SolverResponse.Error, SolverResponse.Ok] =
    EitherT( F.interruptible( doSolve( requested, recipes, inputs ) ) )

  private def itemVarName( item: ClassName[Item] ): String       = show"I__$item"
  private def recipeVarName( recipe: ClassName[Recipe] ): String = show"R__$recipe"
  private def exprName( item: ClassName[Item] ): String          = show"X__$item"

  private def doSolve(
      requested: Vector[Countable[Double, Item]],
      recipes: Vector[Recipe],
      inputs: Map[ClassName[Item], SolverRequest.Resource]
  ): Either[SolverResponse.Error, SolverResponse.Ok] =
    val model: ExpressionsBasedModel = new ExpressionsBasedModel

    val recipeVars: Map[ClassName[Recipe], Variable] =
      recipes
        .map: recipe =>
          val name   = recipeVarName( recipe.className )
          val weight = recipe.power.average * ConstraintSolver.PowerWeight
          (
            recipe.className,
            model.addVariable( name ).weight( weight ).lower( 0d )
          )
        .toMap

    def inputVar( item: ClassName[Item], resource: SolverRequest.Resource ): Variable =
      model
        .addVariable( itemVarName( item ) )
        .lower( 0d )
        .upper( resource.cap )
        .weight( resource.weight )

    val inputVars: Map[ClassName[Item], Variable] = inputs.map:
      case ( item, resource ) => ( item, inputVar( item, resource ) )

    val requestedByItem: Map[ClassName[Item], Double] =
      requested.foldMap:
        case Countable( item, amount ) => Map( ( item.className, amount ) )

    val itemExprs: Map[ClassName[Item], Expression] =
      recipes
        .foldMap( _.itemsPerMinuteMap.keySet )
        .toVector
        .map: item =>
          (
            item.className,
            model
              .addExpression( exprName( item.className ) )
              .lower( requestedByItem.get( item.className ).orEmpty )
          )
        .toMap

    recipes.foreach: recipe =>
      recipe.itemsPerMinuteMap.foreach:
        case ( item, amount ) =>
          itemExprs( item.className ).set( recipeVars( recipe.className ), amount )

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
            val amount = v.getValue.doubleValue()
            Option.when( amount != 0d )( Countable( k, amount ) )
        .toVector

    Option
      .when( result.getState.isSuccess ):
        SolverResponse.Solution(
          extractValues( inputVars ),
          extractValues( recipeVars )
        )
      .toRight( SolverResponse.SolverError( result.getState.toString.toLowerCase ) )

object ConstraintSolver:
  private val PowerWeight: Double = 1e-6
