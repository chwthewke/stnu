package net.chwthewke.stnu
package model

opaque type MachineType = ExtractorType | ManufacturerType | PowerGeneratorType.type

object MachineType
    extends Enum[MachineType]
    with CatsEnum[MachineType]
    with OrderEnum[MachineType]
    with CirceEnum[MachineType]:

  override val values: Array[MachineType] =
    ExtractorType.values ++ ManufacturerType.values :+ PowerGeneratorType

  inline def apply( manufacturerType: ManufacturerType ): MachineType          = manufacturerType
  inline def apply( extractorType: ExtractorType ): MachineType                = extractorType
  inline def apply( powerGeneratorType: PowerGeneratorType.type ): MachineType = powerGeneratorType

  extension ( machineType: MachineType )
    def extractor: Option[ExtractorType] = machineType match
      case extractorType: ExtractorType => Some( extractorType )
      case _                            => None

    def manufacturer: Option[ManufacturerType] = machineType match
      case manufacturerType: ManufacturerType => Some( manufacturerType )
      case _                                  => None

    def isExtractor: Boolean      = extractor.isDefined
    def isManufacturer: Boolean   = manufacturer.isDefined
    def isPowerGenerator: Boolean = machineType == PowerGeneratorType

    def is( extractorType: ExtractorType ): Boolean       = extractor.contains( extractorType )
    def is( manufacturerType: ManufacturerType ): Boolean = manufacturer.contains( manufacturerType )
