package net.chwthewke.stnu
package spa

import cats.UnorderedFoldable
import cats.parse.Parser
import cats.parse.Parser0
import cats.parse.Rfc5234
import cats.parse.strings.Json
import cats.syntax.all.*

opaque type SearchTerms = Vector[String]

object SearchTerms:
  val empty: SearchTerms                                 = Vector.empty
  inline def apply( terms: Vector[String] ): SearchTerms = terms
  private val parser: Parser0[SearchTerms]               =
    ( Json.delimited.parser | Parser.charsWhile( c => !c.isWhitespace ) )
      .repSep0( Rfc5234.wsp.rep )
      .map( _.toVector )
  def parse( str: String ): Either[String, SearchTerms] =
    parser.parseAll( str ).leftMap( _.toString() )

  extension ( terms: SearchTerms )
    def matches[F[_]: UnorderedFoldable]( targets: F[String] ): Boolean =
      terms.map( _.toLowerCase ).forall( term => targets.exists( _.toLowerCase.contains( term ) ) )
