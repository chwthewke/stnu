package net.chwthewke.stnu
package spa.views

import tyrian.Html

import spa.SearchQuery
import spa.css.Bulma
import spa.css.Phosphor

object WordsSearch:
  val b: Bulma    = Bulma
  val p: Phosphor = Phosphor

  def apply[M]( model: SearchQuery, inputMsg: String => M, resetMsg: M ): Html[M] =
    Html.div( b.field + b.isExpanded )(
      Html.div( b.field + b.hasAddons )(
        Html.div( b.control + b.hasIconsLeft + b.isExpanded )(
          Html.input(
            b.input + Option.when( model.hasError )( b.isDanger ),
            Html.placeholder := "Search",
            Html.onInput( str => inputMsg( str ) ),
            model.output.map( v => Html.value := v )
          ),
          Html.span( b.icon + b.isLeft )(
            Html.i( p.regular.`magnifyingGlass` )()
          )
        ),
        Html.div( b.control )(
          Html.button(
            b.button + b.isInfo + b.isMedium,
            Html.onClick( resetMsg )
          )( Html.i( p.regular.`backspace` )() )
        )
      ),
      Html.p( b.help + b.isDanger )(
        Option.when( model.hasError )( Html.text( "Quotes \" must be closed" ) )
      )
    )
