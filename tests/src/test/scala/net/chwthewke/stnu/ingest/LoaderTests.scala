package net.chwthewke.stnu
package ingest

import cats.effect.IO
import munit.CatsEffectSuite

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
