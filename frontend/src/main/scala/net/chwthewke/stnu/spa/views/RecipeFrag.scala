package net.chwthewke.stnu
package spa.views

import tyrian.Attr
import tyrian.CSS
import tyrian.Html

import data.Countable
import model.Item
import model.Power
import model.Recipe
import spa.Env
import spa.css.Bulma
import spa.css.CssClass
import spa.css.Phosphor

object RecipeFrag:
  val b: Bulma    = Bulma
  val p: Phosphor = Phosphor

  sealed trait IconMagnet:
    def getIcon[M]( env: Env, iconFactory: Icons.Icon[M] ): Html[M]

  object IconMagnet:
    given Conversion[Item, IconMagnet]:
      override def apply( x: Item ): IconMagnet = new IconMagnet:
        override def getIcon[M]( env: Env, iconFactory: Icons.Icon[M] ): Html[M] = iconFactory.item( env, x )

    given Conversion[ClassName[Item], IconMagnet]:
      override def apply( x: ClassName[Item] ): IconMagnet = new IconMagnet:
        override def getIcon[M]( env: Env, iconFactory: Icons.Icon[M] ): Html[M] = iconFactory.item( env, x )

  def numberedIconTag[A, I]( env: Env, ci: Countable[Double, I], attrs: Attr[A]* )( using
      c: Conversion[I, IconMagnet]
  ): Html[A] =
    Html.span( List[Attr[Nothing]]( b.tag + b.isMedium ) )(
      Html.strong( Numbers.showDouble1( ci.amount ) ),
      nbsp,
      c( ci.item ).getIcon( env, icon.verticalAlign().withDropShadow() )
    )

  def numberedIcon[A, I]( env: Env, ci: Countable[Double, I], padding: CssClass, attrs: Attr[A]* )( using
      c: Conversion[I, IconMagnet]
  ): Html[A] =
    Html.span(
      List[Attr[Nothing]](
        padding + b.hasTextWeightBold,
        Html.styles( CSS.verticalAlign( "middle" ), CSS.minHeight( "24px" ), CSS.display( "inline-flex" ) )
      ) ++
        attrs
    )(
      Html.div( Html.styles( CSS.verticalAlign( "middle" ) ) )(
        Numbers.showDouble1( ci.amount )
      ),
      nbsp,
      c( ci.item ).getIcon( env, icon.verticalAlign().withDropShadow() )
    )

  def ingredientIcons( env: Env )( recipe: Recipe, padding: CssClass = b.px1 ): List[Html[Nothing]] =
    recipe match
      case r: Recipe.PowerGeneration =>
        recipe.ingredients.map( numberedIcon( env, _, padding ) )
      case r =>
        Html.span( padding )( showPower( r.power ) ) :: recipe.ingredients.map( numberedIcon( env, _, padding ) )

  private def showPower( power: Power ): Html[Nothing] =
    Html.span( Html.style( CSS.verticalAlign( "middle" ) ) ):
      power match
        case Power.Production( value ) =>
          Html.span( b.hasTextWeightBold )( Numbers.showDouble1( -power.average ) + " MW" ) :: Nil
        case Power.Fixed( value ) =>
          Html.span( b.hasTextWeightBold )( Numbers.showDouble1( power.average ) + " MW" ) :: Nil
        case Power.Variable( min, max ) =>
          Html.span( b.hasTextWeightBold )( Numbers.showDouble1( power.average ) ) ::
            Html.text( s" (${Numbers.showDouble1( power.min )}-${Numbers.showDouble1( power.max )}) " ) ::
            Html.span( b.hasTextWeightBold )( "MW" ) ::
            Nil

  def productIcons( env: Env )( recipe: Recipe, padding: CssClass = b.px1 ): List[Html[Nothing]] =
    recipe match
      case r: Recipe.PowerGeneration =>
        Html.span( padding )( showPower( r.power ) ) :: r.products.map( numberedIcon( env, _, padding ) )
      case r =>
        r.productsList.map( numberedIcon( env, _, padding ) )

  def recipeIcons( env: Env )( recipe: Recipe, padding: CssClass = b.px1 ): List[Html[Nothing]] =
    ingredientIcons( env )( recipe, padding ) ++ (
      Html.span( padding + b.icon )( Html.i( p.fill.arrowFatRight )() ) ::
        productIcons( env )( recipe, padding )
    )
