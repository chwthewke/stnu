package net.chwthewke.stnu
package debug

import cats.Eval

trait TupleInspect[T <: Tuple] extends ( T => Vector[Eval[Data]] )

object TupleInspect:
  given TupleInspect[EmptyTuple]:
    override def apply( value: EmptyTuple ): Vector[Eval[Data]] = Vector.empty

  given [H, T <: Tuple] => ( inspectH: Inspect[H], inspectT: TupleInspect[T] ) => TupleInspect[H *: T]:
    override def apply( value: H *: T ): Vector[Eval[Data]] =
      val h: Eval[Data]         = Eval.later( inspectH( value.head ) )
      val t: Vector[Eval[Data]] = inspectT( value.tail )
      h +: t
