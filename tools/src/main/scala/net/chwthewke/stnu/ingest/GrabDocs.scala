package net.chwthewke.stnu
package ingest

import cats.effect.Async
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.std.Console
import cats.syntax.all.*
import fs2.io.file.Path
import fs2.io.file.Files
import fs2.text
import io.circe.Json
import java.nio.charset.StandardCharsets

class GrabDocs[F[_]: Async]( val version: DataVersionStorage )( using Files: Files[F] ):

  private val console: Console[F] = Console.make[F]

  def run: F[Json] =
    val docsPath: Path = version.gameSource / "CommunityResources" / "Docs" / version.docsFile
    val destDir: Path  = Path( "." ) / "tools" / "src" / "main" / "resources" / version.docsKey
    val destPath: Path = destDir / "Docs.json"

    for
      json <- readJsonString[Json]:
                Files
                  .readAll( docsPath )
                  .through( text.decodeWithCharset[F]( StandardCharsets.UTF_16 ) )
      _ <- console.println( show"Loaded ${docsPath.toString} and parsed as JSON." )
      _ <- Files.createDirectories( destDir )
      _ <- writeJson( json, destPath )
      _ <- console.println( show"Prettified and wrote ${destPath.toString} as UTF-8." )
    yield json

object GrabDocs:
  abstract class Program( storage: DataVersionStorage ) extends IOApp:
    override def run( args: List[String] ): IO[ExitCode] =
      new GrabDocs[IO]( storage ).run.as( ExitCode.Success )

object GrabDocsR1_0 extends GrabDocs.Program( DataVersionStorage.Release1_0 )
