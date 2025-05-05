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
          link( rel := "stylesheet", `type` := "text/css", href := "/static/fonts/regular/styles.css" ),
          link( rel := "stylesheet", `type` := "text/css", href := "/static/fonts/fill/styles.css" ),
          link( rel := "stylesheet", `type` := "text/css", href := "/static/fonts/bold/styles.css" ),
          tags2.title( "Satisfactory Planner" )
        ),
        body(
          h1( "Satisfactory Planner" ),
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
