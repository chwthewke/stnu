package net.chwthewke.stnu
package spa
package views

import cats.syntax.all.*
import org.http4s.Uri
import tyrian.Attr
import tyrian.CSS
import tyrian.Html
import tyrian.Style

import model.Item
import model.Machine
import model.Transport
import spa.css.Bulma
import spa.css.Classes
import spa.css.CssClass

object Icons:
  val b: Bulma = Bulma

  private case class IconAttrs[+M](
      attrs: List[Attr[M]] = Nil,
      styles: List[Style] = Nil,
      classes: Classes = Classes(),
      size: CssClass = b.is24x24
  )

  opaque type Icon[+M] = IconAttrs[M]

  def icon: Icon[Nothing] = IconAttrs[Nothing]()

  object Icon:
    private def setTitle[M]( icon: Icon[M], title: String ): Icon[M] =
      icon.withAttrs[M]( Html.title := title )

    private def make[M]( icon: Icon[M], src: Option[Uri] ): Html[M] =
      Html.img(
        icon.attrs
          ++ List[Attr[Nothing]](
            b.image +: icon.size +: icon.classes,
            Html.styles( CSS.display( "inline-block" ) :: icon.styles* )
          )
          ++ src.map( uri => Html.src := uri.renderString )
      )

    private def makeItem[M]( icon: Icon[M], env: Env, item: ClassName[Item] ): Html[M] =
      make( icon, env.itemIcons.get( item ) )

    private def makeMachine[M]( icon: Icon[M], env: Env, machine: ClassName[Machine] ): Html[M] =
      make( icon, env.machineIcons.get( machine ) )

    private def makeTransport[M]( icon: Icon[M], env: Env, transport: ClassName[Transport] ): Html[M] =
      make( icon, env.transportIcons.get( transport ) )

    extension [M]( icon: Icon[M] )
      def withAttrs[N >: M]( attrs: Attr[N]* ): Icon[N] = icon.copy[N]( attrs = icon.attrs ++ attrs )
      def withStyles( styles: Style* ): Icon[M]         = icon.copy( styles = icon.styles ++ styles )
      def withClasses( classes: Classes ): Icon[M]      = icon.copy( classes = icon.classes + classes )
      def withSize( size: CssClass ): Icon[M]           = icon.copy( size = size )

      def verticalAlign( value: String = "middle" ): Icon[M] =
        withStyles( CSS.verticalAlign( value ) )
      def withDropShadow( stdDev: String = "1px", color: String = "white" ): Icon[M] =
        withStyles( CSS.filter( s"drop-shadow( 0px 0px $stdDev $color )" ) )

      def item( env: Env, item: ClassName[Item] ): Html[M] = makeItem( icon, env, item )
      def item( env: Env, item: Item ): Html[M] = makeItem( setTitle( icon, item.displayName ), env, item.className )

      def machine( env: Env, machine: ClassName[Machine] ): Html[M] = makeMachine( icon, env, machine )
      def machine( env: Env, machine: Machine ): Html[M]            =
        makeMachine( setTitle( icon, machine.displayName ), env, machine.className )

      def transport( env: Env, transport: ClassName[Transport] ): Html[M] = makeTransport( icon, env, transport )
      def transport( env: Env, transport: Transport ): Html[M]            =
        makeTransport( setTitle( icon, transport.displayName ), env, transport.className )
