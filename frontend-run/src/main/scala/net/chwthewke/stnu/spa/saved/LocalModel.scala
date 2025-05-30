package net.chwthewke.stnu
package spa
package saved

import cats.syntax.all.*
import io.circe.derivation.ConfiguredDecoder
import io.circe.derivation.ConfiguredEncoder
import io.circe.parser
import io.circe.syntax.*
import tyrian.Cmd

object LocalModel:
  case class Saved(
      browseModel: LocalBrowseModel.Saved,
      planModel: LocalPlanModel.Saved
  ) derives ConfiguredEncoder

  object Saved:
    def apply[F[_]]( appModel: MainModel.Loaded[F] ): Saved =
      appModel match
        case MainModel.Loaded( _, _, _, browsePage, planPage ) =>
          Saved( LocalBrowseModel.Saved( browsePage ), LocalPlanModel.Saved( planPage ) )

    def encode( saved: Saved ): String = saved.asJson.noSpaces

  case class Loaded(
      browseModel: LocalBrowseModel.Loaded,
      planModel: LocalPlanModel.Loaded
  ) derives ConfiguredDecoder:
    def patch[F[_]]( appModel: MainModel.Loaded[F] ): MainModel.Loaded[F] =
      appModel.copy(
        browsePage = browseModel.toBrowseModel,
        planPage = planModel.patch( appModel.planPage )
      )

    def loadInto[F[_]]( appModel: MainModel[F] ): Option[( MainModel[F], Cmd[F, Msg] )] =
      appModel match
        case m: MainModel.Loaded[F] => ( patch( m ) -> Cmd.None ).some
        case _                      => none

  object Loaded:
    def decode( src: Option[String] ): Either[String, Loaded] =
      src
        .toRight( "No saved model to load" )
        .flatMap( parser.decode[Loaded]( _ ).leftMap( e => s"Saved model decoder error: ${e.getMessage}" ) )
