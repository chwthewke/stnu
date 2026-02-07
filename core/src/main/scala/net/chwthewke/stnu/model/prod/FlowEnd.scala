package net.chwthewke.stnu
package model
package prod

enum FlowEnd:
  case Source
  case Destination

  def opposite: FlowEnd =
    this match
      case Source      => Destination
      case Destination => Source

object FlowEnd extends Enum[FlowEnd] with CatsEnum[FlowEnd] with CirceEnum[FlowEnd]
