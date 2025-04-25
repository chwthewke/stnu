package net.chwthewke.stnu
package model

opaque type MachineType = ExtractorType | ManufacturerType

object MachineType
    extends CustomEnum[MachineType]
    with CatsEnum[MachineType]
    with OrderEnum[MachineType]
    with CirceEnum[MachineType]:
  override val cases: Vector[MachineType] = ExtractorType.cases ++ ManufacturerType.cases

  inline def apply( manufacturerType: ManufacturerType ): MachineType = manufacturerType
  inline def apply( extractorType: ExtractorType ): MachineType       = extractorType

  extension ( machineType: MachineType )
    def extractor: Option[ExtractorType] = machineType match
      case extractorType: ExtractorType => Some( extractorType )
      case _                            => None

    def manufacturer: Option[ManufacturerType] = machineType match
      case manufacturerType: ManufacturerType => Some( manufacturerType )
      case _                                  => None

    def isExtractor: Boolean    = extractor.isDefined
    def isManufacturer: Boolean = manufacturer.isDefined

    def is( extractorType: ExtractorType ): Boolean       = extractor.contains( extractorType )
    def is( manufacturerType: ManufacturerType ): Boolean = manufacturer.contains( manufacturerType )
