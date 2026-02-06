package net.chwthewke.stnu
package persistence

import doobie.*
import doobie.postgres.implicits.*
import scala.reflect.ClassTag

import model.ClockSpeedPreset
import model.ExtractorType
import model.ResourcePurity
import protocol.persistence.PlanId
import protocol.persistence.PlanName

given Meta[PlanId]              = Meta[Int].imap( PlanId( _ ) )( _.id )
given Meta[PlanName]            = Meta[String].imap( PlanName( _ ) )( _.name )
given [A] => Meta[ClassName[A]] = Meta[String].imap( ClassName( _ ) )( _.name )
given Meta[ClockSpeedPreset]    = Meta[String].tiemap( ClockSpeedPreset.withNameEither )( _.toString )
given Meta[AllowedClassType]    = pgEnumStringOpt( "plan_allowed_t", AllowedClassType.withNameOption, _.toString )
given Meta[ExtractorType]       = Meta[String].tiemap( ExtractorType.withNameEither )( _.toString )
given Meta[ResourcePurity]      = pgEnumStringOpt( "purity_t", ResourcePurity.withNameOption, _.toString )

given [A: ClassTag] => ( m: Meta[Array[A]] ) => Meta[Vector[A]] =
  m.timap( _.toVector )( _.toArray )
given [A: ClassTag] => ( m: Meta[Option[Array[A]]] ) => Meta[Option[Vector[A]]] =
  m.timap( _.map( _.toVector ) )( _.map( _.toArray ) )
