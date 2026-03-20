package net.chwthewke.stnu
package server
package pages

import cats.syntax.all.*
import io.circe.syntax.*
import scalatags.Text.tags2

object Index:
  import scalatags.Text.all.*

  private val ver: String =
    import StnuBuildInfo.*
    version + Option.when( version.endsWith( "-SNAPSHOT" ) )( s"-${builtAt.toEpochMilli / 1000}" ).orEmpty

  val page: doctype =
    doctype( "html" )(
      html(
        lang := "en",
        head(
          link( rel := "stylesheet", `type` := "text/css", href := s"/static/css/bulma-prefixed.min.css?v=$ver" ),
          link( rel := "stylesheet", `type` := "text/css", href := s"/static/css/bulma-theme-overrides.css?v=$ver" ),
          link( rel := "stylesheet", `type` := "text/css", href := s"/static/fonts/regular/style.css?v=$ver" ),
          link( rel := "stylesheet", `type` := "text/css", href := s"/static/fonts/fill/style.css?v=$ver" ),
          link( rel := "stylesheet", `type` := "text/css", href := s"/static/fonts/bold/style.css?v=$ver" ),
          tags2.title( "Satisfactory Planner" )
        ),
        body(
          div( id        := "app" ),
          script( `type` := "module", src := "js/launcher.js" ) // not cached, no ?v=$ver
        )
      )
    )

  def launcherScript( flags: Map[String, String] ): String =
    s"""import { TyrianApp } from '/static/js/main.js?v=$ver';
       |
       |TyrianApp.launch( "app", ${flags.asJson.spaces2} );
       |""".stripMargin.linesIterator.mkString( "\n" )
