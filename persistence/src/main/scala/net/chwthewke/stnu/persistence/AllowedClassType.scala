package net.chwthewke.stnu
package persistence

enum AllowedClassType( override val toString: String ):
  case Recipe        extends AllowedClassType( "recipe" )
  case ExtractorType extends AllowedClassType( "extractor_type" )
  case Belt          extends AllowedClassType( "belt" )
  case Pipeline      extends AllowedClassType( "pipeline" )
  case Generator     extends AllowedClassType( "generator" )

object AllowedClassType extends Enum[AllowedClassType]
