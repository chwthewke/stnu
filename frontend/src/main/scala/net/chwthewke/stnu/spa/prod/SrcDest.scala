package net.chwthewke.stnu
package spa.prod

import model.Recipe

sealed trait SrcDest

object SrcDest:
  sealed trait Src  extends SrcDest
  sealed trait Dest extends SrcDest

  final case class Extract( recipe: ClassName[Recipe] ) extends Src
  final case class Step( recipe: ClassName[Recipe] )    extends Src with Dest
  case object Input                                     extends Src
  case object Requested                                 extends Dest
  case object Byproduct                                 extends Dest
