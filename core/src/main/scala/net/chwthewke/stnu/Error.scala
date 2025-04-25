package net.chwthewke.stnu

import cats.Reducible
import cats.syntax.all.*

case class Error( msg: String ) extends RuntimeException( msg, null, false, false )

object Error:
  def apply[F[_]: Reducible]( errs: F[String] ): Error = Error( errs.intercalate( "," ) )
