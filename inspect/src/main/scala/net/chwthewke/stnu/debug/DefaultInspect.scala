package net.chwthewke.stnu
package debug

opaque type DefaultInspect[A] = Inspect[A]

object DefaultInspect:
  extension [A]( defaultInspect: DefaultInspect[A] ) def instance: Inspect[A] = defaultInspect

  given [A] => DefaultInspect[A] = Impl.asInstanceOf

  private object Impl extends Inspect[Any]:
    override def apply( value: Any ): Data = Data.Opaque( value.toString )
