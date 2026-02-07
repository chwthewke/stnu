package net.chwthewke.stnu

import cats.syntax.all.*
import io.circe.Json
import io.circe.JsonObject
import scala.collection.immutable.SortedSet

// NOTE keeping this around just in case
object JsonDiff:
  def apply( left: Json, right: Json ): List[String] = jsonDiff( Vector.empty, left, right )

  def jsonDiff( path: Vector[String], left: Json, right: Json ): List[String] =
    def tpe( json: Json ): String =
      json.fold( "null", _ => "boolean", _ => "number", _ => "string", _ => "array", _ => "object" )

    def diff( lv: String, rv: String ): String = s"${path.mkString( "." )}: $lv <-> $rv"

    def cmpAtoms[A]( lv: A, rvo: Option[A] ): List[String] =
      rvo.foldMap( rv => if ( lv == rv ) Nil else List( diff( lv.toString, rv.toString ) ) )

    def diffArrays( lv: Vector[Json], rvo: Option[Vector[Json]] ): List[String] =
      rvo.foldMap: rv =>
        if ( lv.length != rv.length ) List( diff( s"Array(${lv.length})", s"Array(${rv.length})" ) )
        else lv.indices.toVector.foldMap( i => jsonDiff( path :+ s"[$i]", lv( i ), rv( i ) ) )

    def diffObjects( lv: JsonObject, rvo: Option[JsonObject] ): List[String] =
      rvo.foldMap: rv =>
        val lk = lv.keys.to( SortedSet )
        val rk = rv.keys.to( SortedSet )
        if ( lk != rk )
          List(
            diff(
              lk.diff( rk ).mkString_( "", ", ", ", ..." ),
              rk.diff( lk ).mkString_( "", ", ", ", ..." )
            )
          )
        else
          lk.toList.foldMap: k =>
            ( lv( k ), rv( k ) )
              .mapN: ( lc, rc ) =>
                jsonDiff( path :+ k, lc, rc )
              .orEmpty

    val ltpe = tpe( left )
    val rtpe = tpe( right )
    if ( ltpe != rtpe ) List( diff( s"<$ltpe>", s"<$rtpe>" ) )
    else
      left.fold(
        Nil,
        cmpAtoms( _, right.asBoolean ),
        cmpAtoms( _, right.asNumber ),
        cmpAtoms( _, right.asString ),
        diffArrays( _, right.asArray ),
        diffObjects( _, right.asObject )
      )
