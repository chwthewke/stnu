package net.chwthewke.stnu
package debug

import scala.annotation.switch

sealed trait Renderer[A]:
  self =>

  def renderTo( appendable: Appendable, value: A ): Appendable

  final def render( value: A ): String =
    renderTo( new java.lang.StringBuilder(), value ).toString

  final def contramap[B]( f: B => A ): Renderer[B] =
    new Renderer[B]:
      override def renderTo( appendable: Appendable, value: B ): Appendable =
        self.renderTo( appendable, f( value ) )

object Renderer:
  def fromToString[A]: Renderer[A] =
    new Renderer[A]:
      override def renderTo( appendable: Appendable, value: A ): Appendable =
        appendable.append( value.toString )

  def escaped[A](
      start: Appendable => Appendable = identity,
      finish: Appendable => Appendable = identity
  )( using ev: A => Iterable[Char] ): Renderer[A] =
    new Renderer[A]:
      override def renderTo( appendable: Appendable, value: A ): Appendable =
        finish( ev( value ).foldLeft( start( appendable ) )( writeEscaped ) )

  // taken from circe, thanks
  private def toHex( nibble: Int ): Char = ( nibble + ( if ( nibble >= 10 ) 87 else 48 ) ).toChar

  private def writeEscaped( to: Appendable, c: Char ): Appendable =
    val esc = ( c: @switch ) match {
      case '"'  => '"'
      case '\\' => '\\'
      case '\b' => 'b'
      case '\f' => 'f'
      case '\n' => 'n'
      case '\r' => 'r'
      case '\t' => 't'
      case _    => ( if ( Character.isISOControl( c ) ) 1 else 0 ).toChar
    }
    if ( esc == 0 )
      to.append( c )
    else if ( esc == 1 )
      to.append( '\\' )
        .append( 'u' )
        .append( toHex( ( c >> 12 ) & 15 ) )
        .append( toHex( ( c >> 8 ) & 15 ) )
        .append( toHex( ( c >> 4 ) & 15 ) )
        .append( toHex( c & 15 ) )
    else
      to.append( '\\' ).append( esc )
  //
