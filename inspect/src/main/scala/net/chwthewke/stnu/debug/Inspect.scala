package net.chwthewke.stnu
package debug

trait Inspect[A] extends ( A => Data )

object Inspect extends InspectInstances0:
  def apply[A]( using ev: Inspect[A] ): Inspect[A] = ev

abstract class InspectInstances0 extends InspectInstances1:
  given basic[A]( using inspect: BasicInspect[A] ): Inspect[A] = inspect.instance

  given tuple[T <: Tuple]( using inspect: TupleInspect[T] ): Inspect[T] =
    ( value: T ) => Data.Tuple( inspect( value ) )

abstract class InspectInstances1 extends InspectInstances2:
  given derived[A]( using inspect: DerivedInspect[A] ): Inspect[A] = inspect.instance

abstract class InspectInstances2:
  given default[A]( using inspect: DefaultInspect[A] ): Inspect[A] =
    inspect.instance
