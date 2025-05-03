package net.chwthewke.stnu
package game

import cats.Show
import cats.data.OptionT
import cats.parse.Parser
import cats.parse.Parser0
import cats.syntax.all.*
import io.circe.Decoder

import data.Countable

case class Schematic(
    className: ClassName[Schematic],
    displayName: String,
    `type`: SchematicType,
    cost: Vector[Countable[Double, ClassName[GameItem]]],
    techTier: Int,
    requireAllDependencies: Boolean,
    schematicDependencies: Vector[ClassName[Schematic]],
    unlocks: Vector[ClassName[GameRecipe]]
)

object Schematic:

  given Show[Schematic] =
    Show.show: schematic =>
      val depsTag: String = if ( schematic.requireAllDependencies ) "ALL OF " else "ONE OF "
      show"""${schematic.displayName} # ${schematic.className}
            |  type: ${schematic.`type`}
            |  tier: ${schematic.techTier}
            |  cost: ${schematic.cost.mkString_( ", " )}
            |  deps: $depsTag${schematic.schematicDependencies.mkString_( ", " )}
            |  unlocks: ${schematic.unlocks.mkString_( ", " )}
            |""".stripMargin

  import Parsers.*

  private val schematicDependencyClass: ClassName[Any] = ClassName( "BP_SchematicPurchasedDependency_C" )

  private val recipeUnlockClass: ClassName[Any] = ClassName( "BP_UnlockRecipe_C" )

  private val itemCostParser: Parser0[Vector[Countable[Double, ClassName[GameItem]]]] =
    Parsers.countableList.map( _.toList.toVector )

  private val itemCostDecoder: Decoder[Vector[Countable[Double, ClassName[GameItem]]]] =
    itemCostParser.decoder.orElse( Decoder.const( Vector.empty ) )

  private def classListParser[A]: Parser0[Vector[ClassName[A]]] =
    Parsers.bpGeneratedClassList | Parser.pure( Vector.empty )

  private def classListDecoder[A](
      matchingClass: ClassName[Any],
      classListField: String
  ): Decoder[Option[Vector[ClassName[A]]]] =
    Decoder.instance: hc =>
      OptionT
        .liftF( hc.get[ClassName[Any]]( "Class" ) )
        .filter( _ == matchingClass )
        .flatMapF( _ =>
          hc.get[Option[Vector[ClassName[A]]]]( classListField )( Decoder.decodeOption( classListParser.decoder ) )
        )
        .value

  private val dependenciesDecoder: Decoder[( Vector[ClassName[Schematic]], Boolean )] =
    Decoder
      .decodeVector(
        OptionT( classListDecoder[Schematic]( schematicDependencyClass, "mSchematics" ) )
          .semiflatMap( d =>
            Parsers.booleanString.decoder
              .prepare( _.downField( "mRequireAllSchematicsToBePurchased" ) )
              .map( req => ( d, req ) )
          )
          .value
      )
      .map( _.flattenOption )
      .emap( depBlocks =>
        if ( depBlocks.size > 1 )
          Left( "Multiple mSchematicDependencies items" )
        else
          Right( depBlocks.headOption.getOrElse( ( Vector.empty, false ) ) )
      )

  private val unlocksDecoder: Decoder[Vector[ClassName[GameRecipe]]] =
    Decoder
      .decodeVector( classListDecoder[GameRecipe]( recipeUnlockClass, "mRecipes" ) )
      .map( v => v.mapFilter( identity ).combineAll )

  given Decoder[Schematic] = Decoder.instance: hc =>
    (
      hc.get[ClassName[Schematic]]( "ClassName" ),
      hc.get[String]( "mDisplayName" ),
      hc.get[SchematicType]( "mType" ),
      hc.get( "mCost" )( itemCostDecoder ),
      hc.get[Int]( "mTechTier" ),
      hc.get( "mSchematicDependencies" )( dependenciesDecoder ),
      hc.get( "mUnlocks" )( unlocksDecoder )
    ).mapN( ( cn, dn, typ, c, tier, deps, unlocks ) => Schematic( cn, dn, typ, c, tier, deps._2, deps._1, unlocks ) )
