package net.chwthewke.stnu
package spa
package views

import cats.syntax.all.*
import scala.annotation.tailrec
import tyrian.Html
import tyrian.Style

import model.prod.Group
import spa.css.Bulma
import spa.css.Classes
import spa.css.Phosphor
import spa.prod.FlowTransport
import spa.prod.Groups

object FlowElements:
  val b: Bulma    = Bulma
  val p: Phosphor = Phosphor

  def transportHeader( env: Env, flowTransport: FlowTransport, index: Option[Int], warnings: Boolean ): Html[Nothing] =
    Html.h3(
      b.subtitle + b.hasTextCentered + b.isSize6
    )(
      RecipeFrag.numberedIconTag( env, flowTransport.transport ),
      Html.span( va(), b.ml2 )(
        s"${flowTransport.transport.item.displayName}" + index.foldMap( ix => s" #${ix + 1}" )
      ),
      Option.when( warnings && flowTransport.overflow )(
        Html.i( p.fill.warning + b.hasTextWarning + b.ml2, va() )()
      ),
      Option.when( warnings && !flowTransport.balanced )(
        Html.i( p.fill.notEquals + b.hasTextDanger + b.ml2, va() )()
      )
    )

  def longGroupName( group: Group ): String = if ( group == Group.root ) "the root group" else s"group $group"

  def groupHue( groups: Groups, group: Group ): Double =
    @tailrec
    def loop( acc: Double, gamut: Double, current: Group, currentTree: Groups ): Double =
      current.path.toNev match
        case Some( nev ) =>
          val w    = currentTree.subGroupWidthWithNewChild
          val step = gamut / w
          val next = acc + ( nev.head.toDouble - 0.5d ) * step
          currentTree.subGroups.flatMap( _.get( nev.head ) ) match
            case None         => next
            case Some( subs ) => loop( next, step, Group( nev.tail ), subs )
        case None => acc

    loop( 0d, 360d, group, groups )

  def groupHueStyle( groups: Groups, group: Group ): Style =
    Style(
      ( "--bulma-info-h", groupHue( groups, group ).toString ),
      ( "--bulma-info", "hsla(var(--bulma-info-h), var(--bulma-info-s), var(--bulma-info-l), 1)" )
    )

  def groupButton[A](
      groups: Groups,
      group: Group,
      newGroup: Boolean,
      action: Option[A],
      classes: Classes = None
  ): Html[A] =
    Html.button(
      classes + b.isSize7 + b.button + b.isSmall +
        ( if ( group == Group.root ) b.isLight else b.isInfo ) +
        Option.when( newGroup )( b.isOutlined ),
      Html.style( groupHueStyle( groups, group ) ),
      action.map( Html.onClick )
    )( group.show )
