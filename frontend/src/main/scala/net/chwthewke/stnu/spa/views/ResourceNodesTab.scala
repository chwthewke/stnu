package net.chwthewke.stnu
package spa
package views

import cats.syntax.all.*
import tyrian.CSS
import tyrian.Html

import model.ExtractorType
import model.Item
import model.ResourceDistrib
import model.ResourcePurity
import spa.css.Bulma
import spa.css.Phosphor
import spa.plan.PlanMsg
import spa.plan.ResourceOptionsInputModel

object ResourceNodesTab:
  val b: Bulma    = Bulma
  val p: Phosphor = Phosphor

  def apply( env: Env, model: ResourceOptionsInputModel ): List[Html[PlanMsg]] =
    List(
      Html.div( b.panelBlock )( Html.h3( b.subtitle )( "Resource nodes" ) ),
      Html.div( b.panelBlock )(
        Html.table( b.table + b.isResponsive )(
          Html.thead(
            Html.tr(
              Html.th( "Item" ),
              Html.th( Html.colspan := "2" )( "Impure" ),
              Html.th( Html.colspan := "2" )( "Normal" ),
              Html.th( Html.colspan := "2" )( "Pure" )
            )
          ),
          Html.tbody(
            env.game.defaultResourceOptions.resourceNodes.toList
              .sortBy( _._1 )
              .flatMap:
                case ( extractor, resources ) =>
                  resourceNodesExtractorRows( env, model )( extractor, resources )
          )
        )
      )
    )

  private def resourceNodesExtractorRows( env: Env, model: ResourceOptionsInputModel )(
      extractor: ExtractorType,
      resources: Map[ClassName[Item], ResourceDistrib]
  ): List[Html[PlanMsg]] =
    Html.tr( Html.td( Html.colspan := "7" )( Html.strong( extractor.description ) ) ) ::
      resources.toList
        .mapFilter:
          case ( itemClass, distrib ) => env.getItem( itemClass ).tupleRight( distrib )
        .map:
          case ( item, distrib ) =>
            resourceNodesItemRow( env, model )( extractor, item, distrib )

  private def resourceNodesItemRow( env: Env, model: ResourceOptionsInputModel )(
      extractor: ExtractorType,
      item: Item,
      distrib: ResourceDistrib
  ): Html[PlanMsg] =
    Html.tr(
      Html.td( Html.style( CSS.verticalAlign( "middle" ) ) )(
        icon.verticalAlign().withDropShadow().item( env, item ),
        nbsp,
        Html.text( item.displayName )
      ) ::
        resourceNodeInputs( extractor, item.className, distrib, model )
    )

  private def resourceNodeInputs(
      extractor: ExtractorType,
      item: ClassName[Item],
      max: ResourceDistrib,
      current: ResourceOptionsInputModel
  ): List[Html[PlanMsg]] =
    ResourcePurity.cases.toList
      .map: purity =>
        ( purity, max.get( purity ), current.inputs.get( ( extractor, item, purity ) ).flatMap( _.output ) )
      .flatMap:
        case ( purity, max, current ) =>
          List(
            Html.td(
              Html.input(
                Html.`type` := "number",
                Html.min    := "0",
                Html.max    := max.toString,
                current.map( v => Html.value := v ),
                Html.style( CSS.width( "3em" ) ),
                Html.onInput( PlanMsg.SetResourceDistribution( extractor, item, purity, _ ) )
              )
            ),
            Html.td( s"($max)" )
          )
