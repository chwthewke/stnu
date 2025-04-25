package net.chwthewke.stnu
package game

import cats.Order
import cats.Show
import cats.syntax.all.*

final case class GameItem(
    className: ClassName,
    displayName: String,
    form: GameForm,
    energyValue: Double,
    sinkPoints: Int,
    smallIcon: IconData,
    nativeClass: NativeClass
):
  def fuelValue: Double = energyValue * form.simpleAmountFactor // MJ

  override def toString: String = GameItem.showItem.show( this )

object GameItem:
  given showItem: Show[GameItem] = Show.show: item =>
    show"""${item.displayName} # ${item.className}
          |Form: ${item.form}
          |Energy: ${item.energyValue} MJ
          |Sink: ${item.sinkPoints} points
          |Icon: ${item.smallIcon}
          |Native class: ${item.nativeClass}
          |""".stripMargin

  given Order[GameItem]    = Order.by( _.displayName )
  given Ordering[GameItem] = Order.catsKernelOrderingForOrder
