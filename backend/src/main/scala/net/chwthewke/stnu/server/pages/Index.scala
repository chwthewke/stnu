package net.chwthewke.stnu
package server
package pages

import io.circe.syntax.*
import scalatags.Text.all.*
import scalatags.Text.tags2

object Index:
  val bp: Bulma = Bulma.Prefixed

  def page: doctype =
    doctype( "html" )(
      html(
        lang := "en",
        head(
          link( rel := "stylesheet", href := "/static/css/bulma-prefixed.min.css" ),
          tags2.title( "Satisfactory Planner" )
        ),
        body(
          bp.themeDark,
          h1( bp.title, bp.is1, "Satisfactory Planner" ),
          p( s"Version ${StnuBuildInfo.version} built on ${StnuBuildInfo.builtAt}" ),
          div( id        := "app" ),
          script( `type` := "module", src := "js/launcher.js" )
        )
      )
    )

  def launcherScript( flags: Map[String, String] ): String =
    s"""import { TyrianApp } from '/static/js/main.js';
       |
       |TyrianApp.launch( "app", ${flags.asJson.spaces2} );
       |""".stripMargin.linesIterator.mkString( "\n" )
