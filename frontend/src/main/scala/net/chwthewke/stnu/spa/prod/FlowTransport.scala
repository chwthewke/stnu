package net.chwthewke.stnu
package spa
package prod

import data.Countable
import model.Transport

trait FlowTransport:
  def transport: Transport
  def amount: Double
  final def transportAmount: Countable[Double, Transport] = Countable( transport, amount / transport.perMinute )
  final def overflow: Boolean                             = transportAmount.amount > 1d + Countable.Tolerance
  def balanced: Boolean
