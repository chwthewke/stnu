package net.chwthewke.stnu
package model

enum Form:
  case Solid, Liquid, Gas

object Form extends Enum[Form] with CatsEnum[Form] with OrderEnum[Form] with CirceEnum[Form]
