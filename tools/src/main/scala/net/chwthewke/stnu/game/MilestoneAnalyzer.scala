package net.chwthewke.stnu
package game

import cats.data.Ior
import cats.syntax.all.*
import mouse.option.*
import scala.annotation.tailrec

import data.newts.Any

object MilestoneAnalyzer:
  def step( solve: ( Analyses[Either[Milestone, AllOf[AnalysisItem]]], AllOf[AnalysisItem] ) => Option[Milestone] )(
      analyses: Analyses[Either[Milestone, AllOf[AnalysisItem]]]
  ): ( Any, Analyses[Either[Milestone, AllOf[AnalysisItem]]] ) =
    analyses.traverse: itemAnalysis =>
      itemAnalysis.flatTraverse: deps =>
        solve( analyses, deps ).cata(
          milestone => ( Any( true ), Left( milestone ) ),
          ( Any( false ), Right( deps ) )
        )

  def solveTight(
      analyses: Analyses[Either[Milestone, AllOf[AnalysisItem]]],
      deps: AllOf[AnalysisItem]
  ): Option[Milestone] =
    deps
      .traverse( analyses.get( _ ).flatMap( _.left.toOption ) )
      .map( _.items.mapFilter( _.items.minimumOption ).maximumOption.getOrElse( Milestone.Zero ) )

  def solveLoose(
      analyses: Analyses[Either[Milestone, AllOf[AnalysisItem]]],
      deps: AllOf[AnalysisItem]
  ): Option[Milestone] =
    deps.items
      .traverse: oneOf =>  // in each OneOf clause
        oneOf.items        //
          .traverseFilter: // gather analyzed dependencies
            case AnalysisItem.OfSchematic( schematic ) => // schematics are always required
              analyses.schematics.get( schematic.className ).flatMap( _.left.toOption ).map( _.some )
            case item => analyses.get( item ).flatMap( _.left.toOption ).some
          .flatMap( _.toNev )     // if at least one is analyzed
          .map( _.minimum )       // take the lowest milestone
      .flatMap( _.maximumOption ) // then take the max across OneOf clauses

  private def checkProgress(
      desc: String,
      prev: Analyses[Either[Milestone, AllOf[AnalysisItem]]],
      next: Analyses[Either[Milestone, AllOf[AnalysisItem]]],
      watchItems: Set[ClassName[GameItem]] = Set(),
      // Set( "Desc_IonizedFuel_C", "Desc_CrystalShard_C" ).map( ClassName( _ ) ),
      watchRecipes: Set[ClassName[GameRecipe]] = Set()
//      Set(
//        "Recipe_PackagedIonizedFuel_C",
//        "Recipe_UnpackageIonizedFuel_C",
//        "Build_GeneratorFuel_C__Desc_IonizedFuel_C",
//        "Recipe_Alternate_IonizedFuel_Dark_C",
//        "Recipe_IonizedFuel_C"
//      ).map( ClassName( _ ) )
  ): Unit =
    def progress[A, B, C](
        p: Map[ClassName[C], Either[A, B]],
        n: Map[ClassName[C], Either[A, B]]
    ): Map[ClassName[C], ( B, A )] =
      p.align( n )
        .mapFilter:
          case Ior.Left( a )    => none
          case Ior.Right( b )   => none
          case Ior.Both( a, b ) => ( a.toOption, b.left.toOption ).tupled

    val itemProgress: Map[ClassName[GameItem], ( AllOf[AnalysisItem], Milestone )] =
      progress( prev.items, next.items )
    def allItems: String = itemProgress.keys.toSeq.mkString_( ", " )

    itemProgress
      .filter: kv =>
        watchItems.contains( kv._1 )
      .foreach:
        case ( k, ( deps, res ) ) =>
          println( show"item $k progressed in $desc from $deps to $res with $allItems." )

    val recipeProgress: Map[ClassName[GameRecipe], ( AllOf[AnalysisItem], Milestone )] =
      progress( prev.recipes, next.recipes )
    def allRecipes: String = recipeProgress.keys.toSeq.mkString_( ", " )

    recipeProgress
      .filter: kv =>
        watchRecipes.contains( kv._1 )
      .foreach:
        case ( k, ( deps, res ) ) =>
          println( show"recipe $k progressed in $desc from $deps to $res with $allRecipes." )

  @tailrec
  def analyze(
      analyses: Analyses[Either[Milestone, AllOf[AnalysisItem]]]
  ): Analyses[Either[Milestone, AllOf[AnalysisItem]]] =
    val ( tightProgress, tightAnalyses ) = step( solveTight )( analyses )
    checkProgress( "tight", analyses, tightAnalyses )
    if ( tightProgress.getAny ) analyze( tightAnalyses )
    else
      val ( looseProgress, looseAnalyses ) = step( solveLoose )( tightAnalyses )
      checkProgress( "loose", tightAnalyses, looseAnalyses )
      if ( looseProgress.getAny ) analyze( looseAnalyses )
      else looseAnalyses

  def run( schematicsData: SchematicsGameData ): Analyses[Either[Milestone, AllOf[AnalysisItem]]] =
    analyze( Analyses.init( schematicsData ) )

  def apply( data: GameData ): Analyses[Milestone] =
    val schematicsGameData                                         = SchematicsGameData.init( data )
    val analyses: Analyses[Either[Milestone, AllOf[AnalysisItem]]] = run( schematicsGameData )
    val notAnalyzed: Analyses[AllOf[AnalysisItem]]                 = analyses.mapFilter( _.toOption )
    val analyzed: Analyses[Milestone]                              = analyses.mapFilter( _.left.toOption )
    notAnalyzed.keys.foreach: cn =>
      println( show"[WARN] not analyzed $cn" )
    schematicsGameData.completeAnalyses( analyzed )
