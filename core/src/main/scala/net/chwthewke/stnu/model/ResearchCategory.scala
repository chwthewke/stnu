package net.chwthewke.stnu
package model

enum ResearchCategory( val keys: Vector[String] ):
  case AlienMegafauna  extends ResearchCategory( Vector( "AO", "ACarapace", "AOrgans" ) )
  case AlienTechnology extends ResearchCategory( Vector( "Alien" ) )
  case PowerSlugs      extends ResearchCategory( Vector( "PowerSlugs" ) )
  case Nutrients       extends ResearchCategory( Vector( "Nutrients" ) )
  case XMas            extends ResearchCategory( Vector( "XMas" ) )
  case Quartz          extends ResearchCategory( Vector( "Quartz" ) )
  case Caterium        extends ResearchCategory( Vector( "Caterium" ) )
  case Sulfur          extends ResearchCategory( Vector( "Sulfur" ) )
  case Mycelia         extends ResearchCategory( Vector( "Mycelia" ) )
  case FlowerPetals    extends ResearchCategory( Vector( "FlowerPetals" ) )

object ResearchCategory
    extends Enum[ResearchCategory]
    with CatsEnum[ResearchCategory]
    with OrderEnum[ResearchCategory]
    with CirceEnum[ResearchCategory]
