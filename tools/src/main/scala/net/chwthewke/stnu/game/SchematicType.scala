package net.chwthewke.stnu
package game

enum SchematicType( val entryName: String ):
  case Alternate     extends SchematicType( "EST_Alternate" )
  case Custom        extends SchematicType( "EST_Custom" )
  case Customization extends SchematicType( "EST_Customization" )
  case HardDrive     extends SchematicType( "EST_HardDrive" )
  case Mam           extends SchematicType( "EST_MAM" )
  case Milestone     extends SchematicType( "EST_Milestone" )
  case Shop          extends SchematicType( "EST_ResourceSink" )
  case Tutorial      extends SchematicType( "EST_Tutorial" )

object SchematicType
    extends Enum[SchematicType]
    with CatsEnum[SchematicType]
    with OrderEnum[SchematicType]
    with CirceEnum[SchematicType]:
  override def keyOf( schematicType: SchematicType ): String = schematicType.entryName
