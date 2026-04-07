package net.chwthewke.stnu
package spa.prod

import cats.data.NonEmptyMap
import cats.syntax.all.*
import io.circe.Decoder
import io.circe.parser
import munit.AnyFixture
import munit.FunSuite
import scala.collection.immutable.SortedMap
import scala.compiletime.uninitialized

import model.Model
import model.ModelIndex
import protocol.solver.ExportedSolutions
import spa.Env

trait GameFixture extends FunSuite:

  import GameFixture.*

  def defaultEnv: Env                     = data().envs.last._2
  def defaultSolutions: ExportedSolutions =
    data().exportedSolutions.byModelVersion.getOrElse( data().envs.last._1, ExportedSolutions( SortedMap.empty ) )

  def `r1.1`: Env =
    data().envs( ModelVersionId( 7 ) ).getOrElse( throw new IllegalStateException( "r1.1 model missing" ) )

  val data: Fixture[TestData] =
    new Fixture[TestData]( "envs" ):
      private var cell: TestData = uninitialized

      override def apply(): TestData = cell

      override def beforeAll(): Unit =
        super.beforeAll()

        cell = readTestData.fold( t => throw t, identity )

  override def munitFixtures: Seq[AnyFixture[?]] = super.munitFixtures :+ data

object GameFixture:

  private def readTestData: Either[Throwable, TestData] =
    val fs = scalajs.js.Dynamic.global.require( "fs" )

    def read[A: Decoder]( path: String ) =
      Either
        .catchNonFatal( fs.readFileSync( path, "utf8" ) )
        .flatMap( txt => parser.decode[A]( txt.toString ) )

    val assetsDir     = "assets/src/main/resources"
    val solutionsFile = "testkit/src/test/resources/solutions.json"

    for
      index    <- read[ModelIndex]( s"$assetsDir/index.json" )
      indexNev <- index.versions.toVector.toNev.toRight( new NoSuchElementException( "empty model index" ) )
      envs     <- indexNev.traverse: version =>
                read[Model]( s"$assetsDir/${version.key}/model.json" ).tupleLeft( version.version )
      solutions <- read[ExportedSolutions.ByModelVersion]( solutionsFile )
    yield TestData( envs.toNem.fmap( model => Env( model, Map.empty, Map.empty, Map.empty ) ), solutions )

  case class TestData( envs: NonEmptyMap[ModelVersionId, Env], exportedSolutions: ExportedSolutions.ByModelVersion )
