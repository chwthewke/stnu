package net.chwthewke.stnu
package ingest

import cats.effect.IO
import io.circe.Decoder.Result
import io.circe.Json
import io.circe.syntax.*
import munit.CatsEffectSuite

import model.Model

class LoaderTests extends CatsEffectSuite:
  DataVersionStorage.cases.foreach: version =>
    test( s"loading ${version.docsKey} GameData succeeds" ):
      Loader[IO]( version ).use:
        _.gameData.assert( _ => true )

    test( s"${version.docsKey} data class names are not too long" ):
      Loader[IO]( version ).use:
        _.gameData
          .assert: data =>
            data.items.keys.forall( _.name.length <= 256 ) &&
              data.extractors.keys.forall( _.name.length <= 256 ) &&
              data.manufacturers.keys.forall( _.name.length <= 256 ) &&
              data.recipes.map( _.className ).forall( _.name.length <= 256 ) &&
              data.powerGenerators.keys.forall( _.name.length <= 256 ) &&
              data.schematics.map( _.className ).forall( _.name.length <= 256 ) &&
              data.conveyorBelts.map( _.className ).forall( _.name.length <= 256 ) &&
              data.pipelines.map( _.className ).forall( _.name.length <= 256 ) &&
              data.buildingDescriptors.keys.forall( _.name.length <= 256 )

    test( s"loading ${version.docsKey} map config succeeds" ):
      Loader[IO]( version ).use:
        _.mapConfig
          .assert( _ => true )

    test( s"loading ${version.docsKey} model succeeds" ):
      Loader[IO]( version ).use:
        _.model
          .assert( _ => true )

    test( s"${version.docsKey} model round-trips to JSON" ):
      Loader[IO]( version )
        .use:
          _.model.map: model =>
            val json: Json             = model.asJson
            val decoded: Result[Model] = json.as[Model]
            assertEquals( decoded, Right( model ) )
        .unsafeRunSync()
