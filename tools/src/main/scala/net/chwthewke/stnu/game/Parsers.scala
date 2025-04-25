package net.chwthewke.stnu
package game

import cats.data.NonEmptyList
import cats.parse.Numbers
import cats.parse.Parser
import cats.parse.Parser0
import cats.syntax.all.*
import io.circe.Decoder

import data.Countable

object Parsers:
  def listOf[A]( p: Parser[A] ): Parser[List[A]] =
    Parser.char( '(' ) *> p.repSep0( Parser.char( ',' ) ) <* Parser.char( ')' )

  def listOf1[A]( p: Parser[A] ): Parser[NonEmptyList[A]] =
    Parser.char( '(' ) *> p.repSep( Parser.char( ',' ) ) <* Parser.char( ')' )

  private val bpSep: Set[Char] = Set( ',', '.', '/', '"', ')', '\'' )

  private val bpNoSep: Parser[Char] = Parser.charWhere( c => !bpSep.contains( c ) ).withContext( "bpNoSep" )

  private val classCore: Parser[ClassName] =
    bpNoSep.rep.string.repSep( Parser.char( '/' ) ).void *> Parser.char( '.' ) *>
      bpNoSep.rep.string.map( ClassName( _ ) )

  val oldBpGeneratedClass: Parser[ClassName] =
    Parser.string( "/Script/Engine." ).?.void.with1 *>
      Parser.string( """BlueprintGeneratedClass'"/Game/FactoryGame/""" ).void *>
      classCore <*
      Parser.string( """"'""" )

  val newBpGeneratedClass: Parser[ClassName] =
    Parser.string( """"/Script/Engine.BlueprintGeneratedClass'/Game/FactoryGame/""" ).void *>
      classCore <* Parser.string( """'"""" )

  val bpGeneratedClass: Parser[ClassName] =
    ( newBpGeneratedClass.backtrack | oldBpGeneratedClass ).withContext( "bpGeneratedClass" )

  val bpGeneratedClassList: Parser[Vector[ClassName]] =
    listOf1( bpGeneratedClass ).map( _.toList.toVector )

  val manufacturerClass: Parser[ClassName] =
    val classNameParser: Parser[ClassName] = Parser.char( '/' ) *> classCore
    classNameParser.surroundedBy( Parser.char( '"' ) ) | classNameParser

  val manufacturerClassList: Parser0[List[ClassName]] =
    ( Parser.char( '(' ) *> manufacturerClass.repSep( Parser.char( ',' ) ) <* Parser.char( ')' ) ).map( _.toList ) |
      Parser.pure( Nil )

  val countable: Parser[Countable[Double, ClassName]] =
    ( ( Parser.string( "(ItemClass=" ) *> bpGeneratedClass <* Parser.string( ",Amount=" ) ) ~
      Numbers.jsonNumber.mapFilter( _.toDoubleOption ) <*
      Parser.char( ')' ) )
      .map( Countable.apply[Double, ClassName].tupled )

  val countableList: Parser[NonEmptyList[Countable[Double, ClassName]]] =
    Parser.char( '(' ) *> countable.repSep( Parser.char( ',' ) ) <* Parser.char( ')' )

  val countableListOrEmpty: Parser0[List[Countable[Double, ClassName]]] =
    countableList.map( _.toList ) | Parser.pure( Nil )

  val texture2d: Parser[IconData] =
    ( Parser.string( "Texture2D " ) *>
      ( Parser.char( '/' ) *> bpNoSep.rep.string.repSep( Parser.char( '/' ) ) ) ~
      ( Parser.char( '.' ) *> bpNoSep.rep.string ) )
      .map:
        case ( pkgPath, texture ) =>
          IconData( pkgPath.init.mkString( "/" ), pkgPath.last, texture )

  val booleanString: Parser[Boolean] =
    Parser.string( "False" ).as( false ) | Parser.string( "True" ).as( true )

  extension [A]( e: Enum[A] )
    def parser: Parser[A] =
      Parser.oneOf( e.cases.map( v => Parser.string( e.keyOf( v ) ).as( v ) ).toList )

  extension [A]( self: Parser0[A] )
    def decoder: Decoder[A] =
      Decoder[String].emap( str => self.parseAll( str ).leftMap( err => s"parsing $str as $self: $err" ) )
