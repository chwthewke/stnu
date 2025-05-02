package net.chwthewke.stnu
package css

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.Sync
import cats.syntax.all.*
import fs2.io.file.Path
import fs2.Stream
import fs2.io.file.Files
import java.nio.file.Paths
import java.util
import java.util.zip.ZipEntry
import java.util.zip.ZipFile
import scala.collection.immutable.SortedSet
import scala.jdk.CollectionConverters.*

class RenderPhosphorIconsClasses[F[_]](
    private val source: Path
)( using F: Sync[F], Files: Files[F] ):
  def run: F[Unit] =
    val packageDecls: Vector[String] = Vector( "net.chwthewke.stnu", "spa", "css" )
    val packageDirs: Vector[String]  = packageDecls.flatMap( _.split( '.' ).toVector )
    val sources: Path                = Path( "." ) / "frontend" / "src" / "main" / "scala"
    val traitName: String            = "PhosphorIcons"
    val targetDir: Path              = packageDirs.foldLeft( sources )( _ / _ )
    val target: Path                 = targetDir / s"$traitName.scala"

    Files.createDirectories( targetDir ) *>
      Stream
        .eval( readIcons.map( renderIcons( packageDecls, traitName, _ ) ) )
        .through( Files.writeUtf8( target ) )
        .compile
        .drain

  def readZipEntries( path: Path ): Stream[F, Path] =
    Stream.force:
      F.delay( new ZipFile( path.toNioPath.toFile ) )
        .map: zipFile =>
          Stream
            .fromBlockingIterator[F]( zipFile.entries().asInstanceOf[util.Enumeration[ZipEntry]].asScala, 1 )
            .map( ( e: ZipEntry ) => Path( e.getName ) )

  def readIcons: F[Map[String, SortedSet[String]]] =
    readZipEntries( source )
      .mapFilter( p =>
        p.names.map( _.toString ).toList match
          case "PNGs" :: tpe :: name :: Nil if name.endsWith( ".png" ) =>
            Some( Map( tpe -> SortedSet( name.stripSuffix( ".png" ).stripSuffix( s"-$tpe" ) ) ) )
          case _ => None
      )
      .compile
      .foldMonoid

  def renderIcons( packageDecls: Seq[String], traitName: String, icons: Map[String, SortedSet[String]] ): String =
    val common: SortedSet[String] = icons.values.reduceOption( _.intersect( _ ) ).combineAll

    def renderIconValue( name: String ): String =
      s"""    val `${RenderBulmaClasses.camelCase( name )}`: A = cls( "$name" )"""

    def renderIconsTrait( tpe: String, iconClasses: SortedSet[String] ): String =
      s"""  object $tpe extends Common:
         |    override protected def cls( name: String ): A = tcls( "$tpe", name )
         |${iconClasses.toVector.map( renderIconValue ).mkString( "\n" )}
         |""".stripMargin

    def renderSubClasses =
      icons.toVector
        .map:
          case ( tpe, icons ) => renderIconsTrait( tpe, icons -- common )
        .mkString( "\n" )

    s"""////////////////////////////////////////
       |// GENERATED, DO NOT EDIT
       |//  see GenBulmaClasses
       |
       |${packageDecls.map( pkg => s"package $pkg" ).mkString( "\n" )}
       |
       |trait $traitName[A]:
       |  def tcls( tpe: String, name: String ): A  
       |
       |  trait Common:
       |    protected def cls( name: String ): A
       |${common.toVector.map( renderIconValue ).mkString( "\n" )}
       |
       |$renderSubClasses
       |""".stripMargin

object RenderPhosphorIconsClasses extends IOApp:
  private def source: Path =
    Path.fromNioPath( Paths.get( sys.props( "user.home" ) ) ) / "Downloads" / "phosphor-icons.zip"

  override def run( args: List[String] ): IO[ExitCode] =
    new RenderPhosphorIconsClasses[IO]( source ).run.as( ExitCode.Success )
