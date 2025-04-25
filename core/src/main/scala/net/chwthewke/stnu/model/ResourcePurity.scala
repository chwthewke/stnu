package net.chwthewke.stnu
package model

enum ResourcePurity( val entryName: String, val multiplier: Double ):
  case Impure extends ResourcePurity( "impure", 0.5d )
  case Normal extends ResourcePurity( "normal", 1.0d )
  case Pure   extends ResourcePurity( "pure", 2.0d )

  override def toString: String = entryName

object ResourcePurity
    extends Enum[ResourcePurity]
    with CatsEnum[ResourcePurity]
    with OrderEnum[ResourcePurity]
    with CirceEnum[ResourcePurity]
