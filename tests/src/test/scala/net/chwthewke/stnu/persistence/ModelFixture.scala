package net.chwthewke.stnu
package persistence

import cats.effect.IO
import cats.effect.Resource
import cats.syntax.all.*
import munit.AnyFixture
import munit.CatsEffectSuite
import munit.catseffect.IOFixture

import protocol.game.FullModel

trait ModelFixture extends CatsEffectSuite:

  def models: Map[DataVersionStorage, FullModel] = modelsFixture()

  private def loadModels: IO[Map[DataVersionStorage, FullModel]] =
    DataVersionStorage.cases
      .traverse: version =>
        ( assets.loadModel[IO]( version.modelVersion ), assets.loadIconIndex[IO]( version.modelVersion ) )
          .mapN( ( m, i ) => ( version, FullModel( m, i ) ) )
      .map( _.toMap )

  val modelsFixture: IOFixture[Map[DataVersionStorage, FullModel]] =
    ResourceSuiteLocalFixture( "models", Resource.eval( loadModels ) )

  override def munitFixtures: Seq[AnyFixture[?]] = super.munitFixtures :+ modelsFixture
