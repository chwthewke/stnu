package net.chwthewke.stnu
package spa

package object saved:
  def fromInputModel( input: InputModel ): Option[String] = input.input
  def toInputModel( value: Option[String] ): InputModel   = value.fold( InputModel.init )( InputModel.withDefault )

  def fromSearchQuery( search: SearchQuery ): Option[String] = fromInputModel( search.inputModel )
  def toSearchQuery( value: Option[String] ): SearchQuery    = value.fold( SearchQuery.init )( SearchQuery.apply )
