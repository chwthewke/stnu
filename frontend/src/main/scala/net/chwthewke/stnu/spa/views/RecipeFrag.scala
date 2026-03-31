package net.chwthewke.stnu
package spa.views

import cats.syntax.all.*
import scala.concurrent.duration.FiniteDuration
import tyrian.Attr
import tyrian.CSS
import tyrian.Elem
import tyrian.Html

import data.Countable
import model.Item
import model.Machine
import model.Power
import model.Recipe
import model.Transport
import spa.Env
import spa.css.Bulma
import spa.css.Classes
import spa.css.Phosphor
import spa.views.Icons.Icon

object RecipeFrag:
  // TODO merge (partially?) into Elements
  val b: Bulma    = Bulma
  val p: Phosphor = Phosphor

  sealed trait IconMagnet:
    def getIcon[M]( env: Env, iconFactory: Icons.Icon[M] ): Html[M]

  object IconMagnet:
    given Conversion[Item, IconMagnet]:
      override def apply( x: Item ): IconMagnet = new IconMagnet:
        override def getIcon[M]( env: Env, iconFactory: Icons.Icon[M] ): Html[M] = iconFactory.item( env, x )

    given icim: Conversion[ClassName[Item], IconMagnet] = ( c: ClassName[Item] ) =>
      new IconMagnet:
        override def getIcon[M]( env: Env, iconFactory: Icon[M] ): Html[M] = iconFactory.item( env, c )

    given Conversion[Transport, IconMagnet]:
      override def apply( x: Transport ): IconMagnet = new IconMagnet:
        override def getIcon[M]( env: Env, iconFactory: Icon[M] ): Html[M] = iconFactory.transport( env, x )

    given tcim: Conversion[ClassName[Transport], IconMagnet] = ( c: ClassName[Transport] ) =>
      new IconMagnet:
        override def getIcon[M]( env: Env, iconFactory: Icon[M] ): Html[M] = iconFactory.transport( env, c )

    given Conversion[Machine, IconMagnet]:
      override def apply( x: Machine ): IconMagnet = new IconMagnet:
        override def getIcon[M]( env: Env, iconFactory: Icon[M] ): Html[M] = iconFactory.machine( env, x )

    given mcim: Conversion[ClassName[Machine], IconMagnet] = ( c: ClassName[Machine] ) =>
      new IconMagnet:
        override def getIcon[M]( env: Env, iconFactory: Icon[M] ): Html[M] = iconFactory.machine( env, c )

  def numberedIconTag[A, I]( env: Env, ci: Countable[Double, I], attrs: Attr[A]* )( using
      c: Conversion[I, IconMagnet]
  ): Html[A] =
    Html.span( List[Attr[Nothing]]( b.tag + b.isMedium ) )(
      Html.strong( Numbers.showDouble1M( ci.amount ) ),
      nbsp,
      c( ci.item ).getIcon( env, icon.verticalAlign().withDropShadow() )
    )

  def numberedIcon[A, I]( env: Env, ci: Countable[Int, I], classes: Classes, attrs: Attr[A]* )( using
      c: Conversion[I, IconMagnet]
  ): Html[A] =
    numberedIconVar( ci.map( c( _ ).getIcon( env, icon.verticalAlign().withDropShadow() ) ), classes, attrs* )(
      _.show
    )

  def numberedIcon1[A, I]( env: Env, ci: Countable[Double, I], classes: Classes, attrs: Attr[A]* )( using
      c: Conversion[I, IconMagnet]
  ): Html[A] =
    numberedIconVar( ci.map( c( _ ).getIcon( env, icon.verticalAlign().withDropShadow() ) ), classes, attrs* )(
      Numbers.showDouble1M
    )

  def numberedIcon3[A, I]( env: Env, ci: Countable[Double, I], classes: Classes, attrs: Attr[A]* )( using
      c: Conversion[I, IconMagnet]
  ): Html[A] =
    numberedIconVar( ci.map( c( _ ).getIcon( env, icon.verticalAlign().withDropShadow() ) ), classes, attrs* )(
      Numbers.showDouble3
    )

  private def numberedIconVar[N, A, I](
      ci: Countable[N, Html[A]],
      classes: Classes,
      attrs: Attr[A]*
  )( showDouble: N => String ): Html[A] =
    Html.span(
      List[Attr[Nothing]](
        classes + b.hasTextWeightBold + b.iconText,
        Html.styles( vas(), CSS.minHeight( "24px" ), CSS.display( "inline-flex" ) )
      ) ++
        attrs
    )(
      Html.span( Html.styles( CSS.verticalAlign( "middle" ) ) )( showDouble( ci.amount ) ),
      Html.span( b.icon + b.ml1 )( ci.item )
    )

  private def ingredientIcons( env: Env )( recipe: Recipe ): List[Html[Nothing]] =
    val ingredients: List[Html[Nothing]] = recipe.ingredients.map( numberedIcon1( env, _, b.mr1 ) )
    recipe match
      case r: Recipe.PowerGeneration =>
        recipeDuration( recipe.duration ) :: ingredients
      case r =>
        recipeDuration( recipe.duration ) :: Html.span( b.mr1 )( showPower( r.power ) ) :: ingredients

  private def showPower( power: Power ): Html[Nothing] =
    val powerText: List[Elem[Nothing]] =
      power match
        case Power.Production( value ) =>
          Html.span( b.hasTextWeightBold )( Numbers.showDouble1M( -power.average ) + " MW" ) :: Nil
        case Power.Fixed( value ) =>
          Html.span( b.hasTextWeightBold )( Numbers.showDouble1M( power.average ) + " MW" ) :: Nil
        case Power.Variable( min, max ) =>
          Html.span( b.hasTextWeightBold )( Numbers.showDouble1M( power.average ) ) ::
            Html.text( s" (${Numbers.showDouble1M( power.min )}-${Numbers.showDouble1M( power.max )}) " ) ::
            Html.span( b.hasTextWeightBold )( "MW" ) ::
            Nil
    Html.span( va(), b.iconText + b.mr1 )(
      Html.span( va() )( powerText ),
      Html.span( va(), b.icon + b.hasTextWarning )( Html.i( p.fill.lightning )() )
    )

  private def recipeDuration( duration: FiniteDuration ): Html[Nothing] =
    val seconds: Long     = duration.toSeconds
    val millis: Long      = duration.toMillis % 1000
    val durationF: Double = seconds.toDouble + millis.toDouble / 1000
    Html.span( va(), b.iconText + b.mr1 )(
      Html.span( va(), b.hasTextWeightBold )( s"$durationF s" ),
      Html.span( va(), b.icon + b.hasTextInfo )( Html.i( p.regular.timer )() )
    )

  def productIcons( env: Env )( recipe: Recipe ): List[Html[Nothing]] =
    val products: List[Html[Nothing]] = recipe.productsList.map( numberedIcon1( env, _, b.mr1 ) )
    recipe match
      case r: Recipe.PowerGeneration => Html.span( b.mr1 )( showPower( r.power ) ) :: products
      case r                         => products

  def recipeIcons( env: Env )( recipe: Recipe ): List[Html[Nothing]] =
    ingredientIcons( env )( recipe ) ++ (
      Html.span( b.mr1 + b.icon, va() )( Html.i( p.fill.arrowFatRight )() ) ::
        productIcons( env )( recipe )
    )
