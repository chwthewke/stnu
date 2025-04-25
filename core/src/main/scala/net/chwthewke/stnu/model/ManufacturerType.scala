package net.chwthewke.stnu
package model

enum ManufacturerType( val description: String ):
  case Manufacturer         extends ManufacturerType( "Manufacturer" )
  case VariableManufacturer extends ManufacturerType( "Manufacturer (variable power)" )

object ManufacturerType
    extends Enum[ManufacturerType]
    with CatsEnum[ManufacturerType]
    with OrderEnum[ManufacturerType]
    with CirceEnum[ManufacturerType]
