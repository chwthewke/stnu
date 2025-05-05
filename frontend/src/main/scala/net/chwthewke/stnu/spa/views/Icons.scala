package net.chwthewke.stnu
package spa
package views

import org.http4s.Uri
import tyrian.Attr
import tyrian.CSS
import tyrian.Html

import css.Bulma
import model.Item

object Icons:
  val b: Bulma = Bulma

  def item( env: Env, item: Item ): Html[Nothing] = Icons.item()( env, item )
  def item( attrs: Attr[Nothing]* )( env: Env, item: Item ): Html[Nothing] =
    icon( ( Html.title := item.displayName ) +: attrs* )( env.itemIcons.get( item.className ) )

  def icon( attrs: Attr[Nothing]* )( src: Option[Uri] ): Html[Nothing] =
    Html.img(
      attrs.toList
        ++ List[Attr[Nothing]]( b.image + b.is24x24, Html.style( CSS.display( "inline-flex" ) ) )
        ++ src.map( uri => Html.src := uri.renderString )
    )
