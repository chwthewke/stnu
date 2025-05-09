package net.chwthewke.stnu
package spa
package views

import cats.syntax.all.*
import org.http4s.Uri
import org.http4s.syntax.literals.*
import tyrian.Elem
import tyrian.Html

import model.ModelIndex
import spa.css.Bulma
import spa.css.Phosphor

object MainView:
  val b: Bulma    = Bulma
  val p: Phosphor = Phosphor

  private def internalLocationNavItem(
      current: Option[LocationModel],
      target: LocationModel
  ): Html[Msg] =
    Html.a(
      b.navbarItem + Option.when( current.contains( target ) )( b.isActive ),
      Html.href := target.toInternalLocation
    )( target.toString )

  private def externalLocationNavItem( name: String, uri: Uri ): Html[Nothing] =
    Html.a(
      b.navbarItem,
      Html.href := uri.renderString
    )( name )

  private def gameVersionSelectorItem[F]( current: ModelVersionId, version: ModelVersion ): Html[Msg] = {
    val isSelected = version.version == current
    Html.a(
      b.navbarItem + Option.when( isSelected )( b.isSelected ),
      Option.when( !isSelected )( Html.onClick( Msg.FetchGameModel( version.version ) ) )
    )(
      Html.text( version.name ),
      Option.when( isSelected )(
        Html.span( b.icon + b.hasTextSuccess )(
          Html.i( p.fill.checkFat )()
        )
      )
    )
  }

  def nav( locationModel: Option[LocationModel], models: Option[( ModelIndex, ModelVersionId )] ): Html[Msg] =
    Html.nav( b.navbar, Html.role := "navigation" )(
      Html.div( b.navbarBrand + b.px1 )(
        Html.div(
          Html.h1( b.title + b.is3 )( "Satisfactory Planner" ),
          Html.div( b.subtitle + b.isSize7 + b.hasTextGrey )(
            s"version ${StnuBuildInfo.version} built on ${StnuBuildInfo.builtAt}"
          )
        )
      ),
      Html.div( b.navbarMenu )(
        Html.div( b.navbarStart )(
          internalLocationNavItem( locationModel, LocationModel.Browse ),
          internalLocationNavItem( locationModel, LocationModel.Plan ),
          externalLocationNavItem( "Wiki", uri"https://satisfactory.wiki.gg/" ),
          externalLocationNavItem( "Map", uri"https://satisfactory-calculator.com/en/interactive-map" )
        ),
        Html.div( b.navbarEnd )(
          models.foldMap:
            case ( modelList, selectedModel ) =>
              List(
                Html.div( b.navbarItem, Html.disabled )( Html.span( Html.text( "Current version:" ) ) ),
                nbsp,
                Html.div( b.navbarItem + b.hasDropdown + b.isHoverable )(
                  Html.div( b.navbarItem )(
                    Html.text( modelList.versions.find( _.version == selectedModel ).fold( "-" )( _.name ) ),
                    Html.span( b.icon + b.hasTextLink )(
                      Html.i( p.regular.caretDown )()
                    )
                  ),
                  Html.div( b.navbarDropdown )(
                    modelList.versions.toList.map( ( m: ModelVersion ) => gameVersionSelectorItem( selectedModel, m ) )
                  )
                )
              )
        )
      )
    )

  def withNav( location: Option[LocationModel], content: Option[ContentModel] )( contents: Elem[Msg]* ): Html[Msg] =
    Html.div( b.themeDark )(
      ( nav( location, content.map( cm => ( cm.modelIndex, cm.model.game.version.version ) ) ) +: contents ).toList
    )

  def view[F[_]]( model: MainModel[F] ): Html[Msg] =
    model match
      case MainModel.Error( message ) =>
        withNav( none, none )(
          Html.article( b.message + b.isDanger )(
            Html.div( b.messageHeader )( Html.p( Html.strong( "Error" ) ) ),
            Html.div( b.messageBody )( Html.p( message ) )
          )
        )
      case MainModel.Loading( _, location ) =>
        withNav( location.some, none )()
      case MainModel.Loaded( _, location, content, browsePage, planPage ) =>
        withNav( location.some, content.some ):
          location match
            case LocationModel.Browse => BrowseView( content.env, browsePage ).map( Msg.BrowseMessage( _ ) )
            case LocationModel.Plan   => PlanView( content.env, planPage ).map( Msg.PlanMessage( _ ) )
