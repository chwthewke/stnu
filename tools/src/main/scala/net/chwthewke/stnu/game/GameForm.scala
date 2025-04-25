package net.chwthewke.stnu
package game

enum GameForm( val entryName: String, val simpleAmountFactor: Int ):
  case Solid   extends GameForm( "RF_SOLID", 1 )
  case Liquid  extends GameForm( "RF_LIQUID", 1000 )
  case Gas     extends GameForm( "RF_GAS", 1000 )
  case Invalid extends GameForm( "RF_INVALID", 1 )

object GameForm extends Enum[GameForm] with CatsEnum[GameForm] with CirceEnum[GameForm]:
  override def keyOf( form: GameForm ): String = form.entryName
