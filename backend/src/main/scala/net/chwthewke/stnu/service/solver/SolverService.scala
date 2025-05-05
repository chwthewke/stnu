package net.chwthewke.stnu
package service
package solver

import cats.data.EitherT
import cats.effect.Async
import cats.syntax.all.*
import org.http4s.HttpRoutes
import org.http4s.Method.POST
import org.http4s.circe.CirceEntityCodec.*

import data.Countable
import model.Item
import model.Model
import model.Recipe
import protocol.codec.UriCodec
import protocol.solver.SolverApi
import protocol.solver.SolverRequest
import protocol.solver.SolverResponse

class SolverService[F[_]: Async](
    private val models: Map[ModelVersionId, Model],
    private val solver: ConstraintSolver[F]
) extends SolverApi[F]
    with UriCodec.Dsl[F]:
  private def validateInputs( model: Model )(
      requested: Vector[Countable[Double, ClassName[Item]]],
      recipeSelection: Vector[ClassName[Recipe]],
      resources: Map[ClassName[Item], SolverRequest.Resource]
  ): Either[
    SolverResponse.Error,
    ( Vector[Countable[Double, Item]], Vector[Recipe], Map[ClassName[Item], SolverRequest.Resource] )
  ] =
    (
      requested.traverse: item =>
        item.traverse( cn => model.items.get( cn ).toValidNel( cn: ClassName[Any] ) ),
      recipeSelection.traverse: cn =>
        model.recipes.get( cn ).toValidNel( cn: ClassName[Any] ),
      resources.toVector
        .traverse:
          case ( cn, res ) => model.items.get( cn ).toValidNel( cn: ClassName[Any] ).as( ( cn, res ) )
    )
      .mapN( ( req, recSel, rsrcs ) => ( req, recSel, rsrcs.toMap ) )
      .leftMap( SolverResponse.InvalidClasses( _ ) )
      .toEither

  def solveEither( request: SolverRequest ): EitherT[F, SolverResponse.Error, SolverResponse.Ok] =
    for
      model <-
        EitherT.fromEither[F]:
          models.get( request.modelVersion ).toRight( SolverResponse.InvalidModelVersion )
      ( requested, recipes, resources ) <-
        EitherT.fromEither[F]:
          validateInputs( model )( request.requested, request.recipeSelection, request.resources )
      solution <- solver.solve( requested, recipes, resources )
    yield solution

  override def solve( request: SolverRequest ): F[SolverResponse] =
    solveEither( request ).merge

  val routes: HttpRoutes[F] =
    val SA = SolverApi
    HttpRoutes.of:
      case req @ POST -> SA.postSolverRequest() =>
        for
          solverRequest  <- req.as[SolverRequest]
          solverResponse <- solve( solverRequest )
          resp           <- Ok( solverResponse )
        yield resp

object SolverService:
  def apply[F[_]: Async]( models: Vector[Model] ): SolverService[F] =
    new SolverService[F]( models.fproductLeft( _.version.version ).toMap, new ConstraintSolver[F] )
