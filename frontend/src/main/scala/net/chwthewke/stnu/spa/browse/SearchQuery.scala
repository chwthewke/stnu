package net.chwthewke.stnu
package spa
package browse

case class SearchQuery(
    terms: Vector[String],
    inputModel: InputModel
):
  def restore: SearchQuery   = SearchQuery( terms, inputModel.restore )
  def input: Option[String]  = inputModel.input
  def output: Option[String] = inputModel.output

object SearchQuery:
  val init: SearchQuery                     = SearchQuery( Vector.empty, InputModel.init )
  val clear: SearchQuery                    = SearchQuery( Vector.empty, InputModel( None, Some( "" ) ) )
  def onInput( value: String ): SearchQuery = SearchQuery( extractTerms( value ), InputModel.onInput( value ) )
  private def extractTerms( input: String ): SearchTerms =
    input.split( ' ' ).toVector // not particularly safe but oh well
