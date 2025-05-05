package net.chwthewke.stnu
package spa

case class InputModel( input: Option[String], output: Option[String] ):
  def restore: InputModel = InputModel( input, input )

object InputModel:
  val init: InputModel                         = InputModel( None, None )
  def withDefault( value: String ): InputModel = InputModel( Some( value ), Some( value ) )
  def onInput( value: String ): InputModel     = InputModel( Some( value ), None )
