package net.chwthewke.stnu
package game

import cats.effect.IO
import cats.syntax.all.*
import munit.CatsEffectSuite
import scala.collection.immutable.SortedMap

import ingest.Loader
import model.Tier

class MilestoneAnalyzerTests extends CatsEffectSuite:
  import MilestoneAnalyzerTests.Wrapper

  DataVersionStorage.cases.foreach: version =>
    test( s"The MilestoneAnalyzer for version ${version.docsKey} should succeed" ):
      val analyses =
        Loader[IO]( version )
          .use:
            _.gameData.map( data => MilestoneAnalyzer.run( SchematicsGameData.init( data ) ) )
          .unsafeRunSync()
      assert( clue( new Wrapper( analyses ) ).keys.isEmpty )

    def higherItemTiers(
        data: GameData,
        analyses: Analyses[Milestone]
    ): Vector[( ClassName[GameRecipe], Tier, ClassName[GameItem], Tier )] =
      analyses.recipes.toVector.foldMap:
        case ( rc, recipeTier ) =>
          data.recipes
            .find( _.className == rc )
            .foldMap: recipe =>
              recipe.products.toList
                .map( _.item )
                .mapFilter: ic =>
                  analyses.items.get( ic ).map( itemTier => ( rc, recipeTier, ic, itemTier ) )
                .filter:
                  case ( _, rt, _, it ) => it > rt
                .toVector

    test( s"In ${version.docsKey}, when a recipe produces an item, item tier <= recipe tier" ):
      val ( data, analyses ) =
        Loader[IO]( version )
          .use:
            _.gameData.fproduct( data => MilestoneAnalyzer.run( SchematicsGameData.init( data ) ) )
          .unsafeRunSync()
      assert( higherItemTiers( data, clue( new Wrapper( analyses ) ).analyses.mapFilter( _.left.toOption ) ).isEmpty )

object MilestoneAnalyzerTests:
  class Wrapper( val analyses: Analyses[Either[Milestone, AllOf[AnalysisItem]]] ):
    val notAnalyzed: Analyses[AllOf[AnalysisItem]] = analyses.mapFilter( _.toOption )
    val analyzed: Analyses[Milestone]              = analyses.mapFilter( _.left.toOption )

    def keys: Vector[ClassName[Any]] = notAnalyzed.keys

    override def toString: String = showAnalyses

    def showAnalysisItem( item: AnalysisItem ): String =
      val s = show"${item.displayName} [${item.className}]"
      val t = analyses.get( item ).fold( "?" )( e => e.fold( t => t.toString, _ => "D" ) )
      s"[$t] $s"

    def showOneOf( oneOf: OneOf[AnalysisItem] ): String =
      oneOf.items
        .map( showAnalysisItem )
        .mkString( "ONE OF\n  ", "\n  ", "" )

    def showAllOf( allOf: AllOf[AnalysisItem] ): String =
      allOf.items
        .map( it => showOneOf( it ).linesIterator.mkString( "\n  " ) )
        .mkString( "ALL OF\n  ", "\n  ", "" )

    def showAnalyses: String =
      def showMap[C]( m: SortedMap[ClassName[C], Either[Milestone, AllOf[AnalysisItem]]] ): String =
        m.toVector
          .map:
            case ( cn, Left( t ) ) => show"  [$t] $cn\n"
            case ( cn, Right( a ) ) =>
              show"""  [D] $cn
                    |    ${showAllOf( a ).linesIterator.toVector.mkString( "", "\n    ", "" )}
                    |""".stripMargin
          .mkString

      s"""
         |OK ${analyzed.size} KO ${notAnalyzed.size}
         |SCHEMATICS
         |${showMap( analyses.schematics )}
         |ITEMS
         |${showMap( analyses.items )}
         |RECIPES
         |${showMap( analyses.recipes )}
         |POWER GENERATORS
         |${showMap( analyses.powerGenerators )}
         |EXTRACTORS
         |${showMap( analyses.extractors )}
         |""".stripMargin
