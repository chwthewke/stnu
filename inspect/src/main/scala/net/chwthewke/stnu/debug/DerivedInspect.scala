package net.chwthewke.stnu
package debug

import cats.Eval

opaque type DerivedInspect[A] = Inspect[A]

object DerivedInspect:
  def apply[A]( using ev: DerivedInspect[A] ): Inspect[A]                     = ev.instance
  extension [A]( derivedInspect: DerivedInspect[A] ) def instance: Inspect[A] = derivedInspect

  import shapeless3.deriving.K0.*
  import shapeless3.deriving.Labelling

  given product[A]( using instances: => ProductInstances[Inspect, A], labelling: => Labelling[A] ): DerivedInspect[A] =
    new InspectProduct[A]( () => instances, () => labelling )

  private class InspectProduct[A](
      val instances: () => ProductInstances[Inspect, A],
      val labelling: () => Labelling[A]
  ) extends Inspect[A]:
    override def apply( value: A ): Data = {
      val lab = labelling()
      Data.Prod(
        lab.label,
        lab.elemLabels.zipWithIndex
          .map:
            case ( lab, ix ) =>
              ( lab, Eval.later( instances().project( value )( ix )( [a] => ( inspect, a ) => inspect( a ) ) ) )
          .toVector
      )
    }

  given coproduct[A]( using
      instances: => CoproductInstances[Inspect, A],
      labelling: => Labelling[A]
  ): DerivedInspect[A] =
    new InspectCoproduct[A]( () => instances, () => labelling )

  private class InspectCoproduct[A](
      val instances: () => CoproductInstances[Inspect, A],
      val labelling: () => Labelling[A]
  ) extends Inspect[A]:
    override def apply( value: A ): Data =
      Data.Coprod(
        labelling().label,
        Eval.later( instances().fold( value )( [a] => ( inspect, a ) => inspect( a ) ) )
      )
