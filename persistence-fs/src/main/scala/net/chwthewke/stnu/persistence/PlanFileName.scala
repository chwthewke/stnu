package net.chwthewke.stnu
package persistence

import cats.syntax.all.*
import fs2.io.file.Path
import scala.util.hashing.MurmurHash3
import scodec.bits.ByteVector

import protocol.persistence.PlanId
import protocol.persistence.PlanName

case class PlanFileName( planId: PlanId, name: PlanName ):
  lazy val hashed: PlanFileName.Hashed = PlanFileName.Hashed( this )
  def summaryFile: Path                = hashed.summaryFile
  def planFile: Path                   = hashed.planFile

object PlanFileName:
  def nameHash( name: PlanName ): String =
    val hash: Int = MurmurHash3.stringHash( name.name )
    ByteVector.fromInt( hash ).toHex

  case class Hashed( hash: String, id: PlanId ):
    def summaryFile: Path = Path( show"$hash.$id.sps" )
    def planFile: Path    = Path( show"$hash.$id.sp" )

  object Hashed:
    def apply( planFileName: PlanFileName ): Hashed =
      Hashed( nameHash( planFileName.name ), planFileName.planId )

    def unapply( name: String ): Option[Hashed] =
      name.split( '.' ).toList match
        case h :: i :: Nil =>
          val l = h.toLowerCase
          Option.when( l.forall( c => '0' <= c && c <= '9' || 'a' <= c && c <= 'z' ) )( () ) *>
            i.toIntOption.map( id => Hashed( l, PlanId( id ) ) )
        case _ => None

  object Summary:
    def unapply( path: Path ): Option[Hashed] =
      val pstring: String = path.fileName.toString
      if ( pstring.endsWith( ".sps" ) ) Hashed.unapply( pstring.dropRight( 4 ) )
      else none

  object Plan:
    def unapply( path: Path ): Option[Hashed] =
      val pstring: String = path.fileName.toString
      if ( pstring.endsWith( ".sp" ) ) Hashed.unapply( pstring.dropRight( 3 ) )
      else none
