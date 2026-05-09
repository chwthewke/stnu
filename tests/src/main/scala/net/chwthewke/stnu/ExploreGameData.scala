package net.chwthewke.stnu

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all.*
import scala.collection.immutable.SortedSet

import game.GameData
import ingest.Loader

object ExploreGameData extends IOApp:
  def gameData( version: DataVersionStorage = DataVersionStorage.Release1_1 ): IO[GameData] =
    Loader[IO]( version ).use( _.gameData )

  def showItemStackSizes( gameData: GameData ): String =
    gameData.items.values.map( _.stackSize ).to( SortedSet ).mkString( ", " )

  def showManufacturerProductionBoost( gameData: GameData ): String =
    gameData.manufacturers.values.toVector
      .fmap: m =>
        f"""${m.displayName}
           |  Somersloop slots: ${m.productionShardSlotSize}
           |  Somersloop effect: ${m.productionShardBoostMultiplier}%.2f
           |  Somersloop power exponent: ${m.productionBoostPowerConsumptionExponent}%.4f
           |""".stripMargin
      .mkString_( "\n" )

  override def run( args: List[String] ): IO[ExitCode] =
    gameData()
      .map( showItemStackSizes )
      .flatMap( IO.println )
      .as( ExitCode.Success )
