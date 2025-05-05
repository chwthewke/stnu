package net.chwthewke.stnu
package spa

import cats.syntax.all.*

case class SearchQuery(
    terms: SearchTerms,
    inputModel: InputModel,
    hasError: Boolean
):
  def restore: SearchQuery =
    inputModel.input.fold( SearchQuery.init )( SearchQuery.parse( _, inputModel ) )
  def input: Option[String]                 = inputModel.input
  def output: Option[String]                = inputModel.output
  def onInput( value: String ): SearchQuery = SearchQuery.onInput( value )
  def clear: SearchQuery                    = SearchQuery.clear

object SearchQuery:
  val init: SearchQuery  = SearchQuery( SearchTerms.empty, InputModel.init, hasError = false )
  val clear: SearchQuery = SearchQuery( SearchTerms.empty, InputModel( None, Some( "" ) ), hasError = false )

  private def parse( value: String, inputModel: InputModel ): SearchQuery =
    val wordsOrError = SearchTerms.parse( value )
    SearchQuery( wordsOrError.getOrElse( SearchTerms.empty ), inputModel, wordsOrError.isLeft )

  def apply( value: String ): SearchQuery   = parse( value, InputModel.withDefault( value ) )
  def onInput( value: String ): SearchQuery = parse( value, InputModel.onInput( value ) )
