package net.chwthewke.stnu
package spa
package prod

import data.Countable
import model.Transport

trait FlowTransport:
  def transport: Countable[Double, Transport]
  def overflow: Boolean = transport.amount > 1d + Countable.Tolerance
  def balanced: Boolean
