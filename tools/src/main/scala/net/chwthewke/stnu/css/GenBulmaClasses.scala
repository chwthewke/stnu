package net.chwthewke.stnu
package css

import cats.effect.Async
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all.*
import fs2.Stream
import fs2.io.file.Files
import fs2.io.file.Path

class GenBulmaClasses[F[_]: Async]( using Files: Files[F] ):
  def run: F[Unit] =
    val packageDecls: Vector[String] = Vector( "net.chwthewke.stnu", "spa", "css" )
    val packageDirs: Vector[String]  = packageDecls.flatMap( _.split( '.' ).toVector )
    val sources: Path                = Path( "." ) / "frontend" / "src" / "main" / "scala"
    val traitName: String            = "BulmaClasses"
    val targetDir: Path              = packageDirs.foldLeft( sources )( _ / _ )
    val target: Path                 = targetDir / s"$traitName.scala"

    Files.createDirectories( targetDir ) *>
      Stream
        .emit[F, String]( RenderBulmaClasses.renderClasses( packageDecls, traitName ) )
        .through( Files.writeUtf8( target ) )
        .compile
        .drain

object GenBulmaClasses extends IOApp:
  override def run( args: List[String] ): IO[ExitCode] =
    new GenBulmaClasses[IO].run.as( ExitCode.Success )
