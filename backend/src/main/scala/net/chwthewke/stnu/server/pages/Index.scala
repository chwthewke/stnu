package net.chwthewke.stnu
package server
package pages

import io.circe.syntax.*
import scalatags.Text.tags2

object Index:
  import scalatags.Text.all.*

  def page: doctype =
    doctype( "html" )(
      html(
        lang := "en",
        head(
          link( rel := "stylesheet", `type` := "text/css", href := "/static/css/bulma-prefixed.min.css" ),
          link( rel := "stylesheet", `type` := "text/css", href := "/static/css/bulma-theme-overrides.css" ),
          link( rel := "stylesheet", `type` := "text/css", href := "/static/fonts/regular/style.css" ),
          link( rel := "stylesheet", `type` := "text/css", href := "/static/fonts/fill/style.css" ),
          link( rel := "stylesheet", `type` := "text/css", href := "/static/fonts/bold/style.css" ),
          tags2.title( "Satisfactory Planner" )
        ),
        body(
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
