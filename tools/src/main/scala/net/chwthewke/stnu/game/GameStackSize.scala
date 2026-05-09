package net.chwthewke.stnu
package game

enum GameStackSize( val entryName: String, val simpleAmount: Int ):
  case One    extends GameStackSize( "SS_ONE", 1 )
  case Small  extends GameStackSize( "SS_SMALL", 50 )
  case Medium extends GameStackSize( "SS_MEDIUM", 100 )
  case Big    extends GameStackSize( "SS_BIG", 200 )
  case Huge   extends GameStackSize( "SS_HUGE", 500 )
  case Fluid  extends GameStackSize( "SS_FLUID", 50 )

object GameStackSize
    extends CustomEnum[GameStackSize]
    with CatsEnum[GameStackSize]
    with OrderEnum[GameStackSize]
    with CirceEnum[GameStackSize]:
  override def keyOf( size: GameStackSize ): String = size.entryName
