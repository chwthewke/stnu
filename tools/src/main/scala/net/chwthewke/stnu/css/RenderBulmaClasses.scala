package net.chwthewke.stnu
package css

object RenderBulmaClasses:
  def camelCase( className: String ): String =
    className.split( '-' ).toList match
      case Nil       => className
      case _ :: Nil  => className
      case h :: list => ( h :: list.map( _.capitalize ) ).mkString

  def renderClasses(
      packageDecls: Seq[String],
      traitName: String,
      classes: Seq[Classes] = RenderBulmaClasses.classes
  ): String =

    def classLine( className: String ): String =
      s"""val ${camelCase( className )}: A = cls( "$className" )"""

    def commentLine( level: Int, content: String ): Seq[String] =
      level match
        case 0 =>
          s"""//////////////////////
             |// ${content.toUpperCase}
             |//////////////////////
             |""".stripMargin.linesIterator.toSeq
        case 1 =>
          Seq( s"// ${content.capitalize}" )
        case n =>
          Seq( "//" + ( " " * n ) + content )

    def classesLines: Seq[String] =
      def go( level: Int, clss: Seq[Classes] ): Seq[String] =
        if ( clss.isEmpty ) Vector.empty
        else
          clss.flatMap( cls =>
            if ( cls.children.isEmpty ) Seq( classLine( cls.name ) )
            else commentLine( level, cls.name ) ++ go( level + 1, cls.children )
          )
      go( 0, classes )

    s"""////////////////////////////////////////
       |// GENERATED, DO NOT EDIT
       |//  see GenBulmaClasses
       |
       |${packageDecls.map( pkg => s"package $pkg" ).mkString( "\n" )}
       |
       |trait $traitName[A]:
       |  def cls( name: String ): A
       |  
       |  ${classesLines.mkString( "\n  " )}
       |""".stripMargin

  class Classes( val name: String, val children: Seq[Classes] )
  object Classes:
    def apply( name: String )( children: Classes* ): Classes = new Classes( name, children )

    given Conversion[String, Classes]:
      override def apply( name: String ): Classes = Classes( name )()

  val classes: Seq[Classes] = Seq(
    Classes( "Elements" )(
      Classes( "Block" )(
        "block"
      ),
      Classes( "Box" )(
        "box"
      ),
      Classes( "Button" )(
        "button",
        "buttons"
      ),
      Classes( "Content" )(
        "content"
      ),
      Classes( "Delete" )(
        "delete"
      ),
      Classes( "Icon" )(
        "icon",
        "icon-text"
      ),
      Classes( "Image" )(
        "image",
        Classes( "sizes" )(
          Seq( 16, 24, 32, 48, 64, 96, 128 ).map( s => s"is-${s}x$s": Classes )*
        ),
        Classes( "aspect ratios" )(
          "is-square",
          "is-1by1",
          "is-5by4",
          "is-4by3",
          "is-3by2",
          "is-5by3",
          "is-16by9",
          "is-2by1",
          "is-3by1",
          "is-4by5",
          "is-3by4",
          "is-2by3",
          "is-3by5",
          "is-9by16",
          "is-1by2",
          "is-1by3"
        )
      ),
      Classes( "Notification" )(
        "notification"
      ),
      Classes( "Progress Bar" )(
        "progress"
      ),
      Classes( "Table" )(
        "table",
        "table-container",
        "is-bordered",
        "is-striped",
        "is-narrow",
        "is-hoverable"
      ),
      Classes( "Tag" )(
        "tag",
        "tags",
        "is-delete"
      ),
      Classes( "Title" )(
        "title",
        "subtitle"
      ),
      Classes( "Element colors" )(
        "is-white",
        "is-light",
        "is-dark",
        "is-black",
        "is-text",
        "is-ghost",
        "is-primary",
        "is-link",
        "is-info",
        "is-success",
        "is-warning",
        "is-danger"
      ),
      Classes( "Element sizes" )(
        "is-small",
        "is-normal",
        "is-medium",
        "is-large",
        "are-small",
        "are-medium",
        "are-large"
      ),
      Classes( "Element styles" )(
        "is-outlined",
        "is-inverted",
        "is-rounded"
      ),
      Classes( "Element states" )(
        "is-active",
        "is-focused",
        "is-hovered",
        "is-loading",
        "is-selected",
        "is-static"
      )
    ),
    Classes( "Components" )(
      Classes( "Breadcrumb" )(
        "breadcrumb",
        "has-arrow-separator",
        "has-bullet-separator",
        "has-dot-separator",
        "has-succeeds-separator"
      ),
      Classes( "Card" )(
        "card",
        "card-header",
        "card-header-title",
        "card-header-icon",
        "card-image",
        "card-content",
        "card-footer",
        "card-footer-item"
      ),
      Classes( "Dropdown" )(
        "dropdown",
        "dropdown-trigger",
        "dropdown-menu",
        "dropdown-content",
        "dropdown-item",
        "dropdown-divider"
      ),
      Classes( "Menu" )(
        "menu",
        "menu-label",
        "menu-list"
      ),
      Classes( "Message" )(
        "message",
        "message-header",
        "message-body"
      ),
      Classes( "Modal" )(
        "modal",
        "modal-background",
        "modal-content",
        "modal-close",
        "modal-card",
        "modal-card-head",
        "modal-card-title",
        "modal-card-body",
        "modal-card-foot"
      ),
      Classes( "Navbar" )(
        "navbar",
        "navbar-brand",
        "navbar-burger",
        "navbar-menu",
        "navbar-start",
        "navbar-end",
        "navbar-item",
        "navbar-link",
        "navbar-dropdown",
        "navbar-divider",
        "has-dropdown",
        "is-transparent",
        "is-fixed-top",
        "is-fixed-bottom",
        "has-navbar-fixed-top",
        "has-navbar-fixed-bottom"
      ),
      Classes( "Pagination" )(
        "pagination-previous",
        "pagination-next",
        "pagination-list",
        "pagination-link",
        "pagination-ellipsis"
      ),
      Classes( "Panel" )(
        "panel",
        "panel-heading",
        "panel-tabs",
        "panel-block"
      ),
      Classes( "Tabs" )(
        "tabs",
        "is-boxed",
        "is-toggle",
        "is-toggle-rounded"
      ),
      Classes( "Form" )(
        Classes( "control" )(
          "control",
          "has-icons-left",
          "has-icons-right"
        ),
        Classes( "field" )(
          "field",
          "field-label",
          "field-body"
        ),
        "label",
        "input",
        Classes( "textarea" )(
          "textarea",
          "has-fixed-size"
        ),
        Classes( "select" )(
          "select",
          "is-multiple"
        ),
        Classes( "checkbox" )(
          "checkbox",
          "checkboxes"
        ),
        Classes( "radio" )(
          "radio",
          "radios"
        ),
        Classes( "file" )(
          "file",
          "file-label",
          "file-input",
          "file-cta",
          "file-icon",
          "file-name",
          "has-name"
        ),
        "help"
      ), {
        val columnFracSizes: Seq[String] = Seq(
          "three-quarters",
          "two-thirds",
          "half",
          "one-third",
          "one-quarter",
          "full",
          "four-fifths",
          "three-fifths",
          "two-fifths",
          "one-fifth"
        )

        Classes( "Columns" )(
          "columns",
          "column",
          "is-mobile",
          "is-desktop",
          "is-gapless",
          "is-multiline",
          "is-vcentered",
          Classes( "sizes" )(
            columnFracSizes.map( s => s"is-$s": Classes )*
          ),
          Classes( "offsets" )(
            ( columnFracSizes ++ ( 1 to 12 ).map( _.toString ) ).map( s => s"is-offset-$s": Classes )*
          )
        )
      },
      Classes( "Grid" )(
        "grid",
        Classes( "layout" )(
          ( 1 to 32 ).map( n => s"is-col-min-$n": Classes )*
        ),
        Classes( "gaps" )(
          ( 0 to 8 ).flatMap( n => Seq[Classes]( s"is-gap-$n", s"is-column-gap-$n", s"is-row-gap-$n" ) )*
        ),
        Classes( "Fixed Grid" )(
          "fixed-grid",
          "has-auto-count",
          Classes( "column count" )(
            ( 0 to 12 ).map( n => s"has-$n-cols": Classes )*
          )
        ),
        Classes( "Grid cells" )(
          "cell", {
            val prefixes: Seq[String] =
              Seq(
                "is-col-start",
                "is-col-from-end",
                "is-col-span",
                "is-row-start",
                "is-row-from-end",
                "is-row-span"
              )
            Classes( "positioning" )(
              prefixes.flatMap( p => ( 1 to 4 ).map( n => s"$p-$n": Classes ) )*
            )
          }
        )
      ),
      Classes( "Layout" )(
        Classes( "Container" )(
          "container",
          "is-fluid"
        ),
        Classes( "Hero" )(
          "hero",
          "hero-head",
          "hero-body",
          "hero-foot"
        ),
        Classes( "Section" )(
          "section"
        ),
        Classes( "Level" )(
          "level",
          "level-left",
          "level-right",
          "level-item"
        ),
        Classes( "Media" )(
          "media",
          "media-left",
          "media-right",
          "media-content"
        ),
        Classes( "Footer" )(
          "footer"
        )
      )
    ),
    Classes( "Other" )(
      Classes( "Theme" )(
        "theme-dark",
        "theme-light"
      ),
      Classes( "Layout" )(
        "is-horizontal",
        "is-responsive",
        "is-tab",
        "is-expanded",
        "is-fullwidth",
        "is-grouped",
        "is-grouped-centered",
        "is-grouped-right",
        "is-grouped-multiline",
        "is-spaced",
        "is-centered",
        "is-left",
        "is-right",
        "is-up",
        "has-addons",
        "has-addons-centered",
        "has-addons-right",
        "has-ratio"
      ),
      Classes( "Sizes" )(
        ( 1 to 12 ).map( n => s"is-$n": Classes )*
      ),
      Classes( "Style" )(
        "has-shadow"
      ),
      Classes( "Text colors" )(
        "has-text-white",
        "has-text-black",
        "has-text-light",
        "has-text-dark",
        "has-text-primary",
        "has-text-link",
        "has-text-info",
        "has-text-success",
        "has-text-warning",
        "has-text-danger",
        "has-text-black-bis",
        "has-text-black-ter",
        "has-text-grey-darker",
        "has-text-grey-dark",
        "has-text-grey",
        "has-text-grey-light",
        "has-text-grey-lighter",
        "has-text-white-ter",
        "has-text-white-bis",
        "has-text-primary-light",
        "has-text-link-light",
        "has-text-info-light",
        "has-text-success-light",
        "has-text-warning-light",
        "has-text-danger-light",
        "has-text-primary-dark",
        "has-text-link-dark",
        "has-text-info-dark",
        "has-text-success-dark",
        "has-text-warning-dark",
        "has-text-danger-dark",
        "has-text-current",
        "has-text-inherit"
      ),
      Classes( "Background colors" )(
        "has-background-white",
        "has-background-black",
        "has-background-light",
        "has-background-dark",
        "has-background-primary",
        "has-background-link",
        "has-background-info",
        "has-background-success",
        "has-background-warning",
        "has-background-danger",
        "has-background-black-bis",
        "has-background-black-ter",
        "has-background-grey-darker",
        "has-background-grey-dark",
        "has-background-grey",
        "has-background-grey-light",
        "has-background-grey-lighter",
        "has-background-white-ter",
        "has-background-white-bis",
        "has-background-primary-light",
        "has-background-link-light",
        "has-background-info-light",
        "has-background-success-light",
        "has-background-warning-light",
        "has-background-danger-light",
        "has-background-primary-dark",
        "has-background-link-dark",
        "has-background-info-dark",
        "has-background-success-dark",
        "has-background-warning-dark",
        "has-background-danger-dark",
        "has-background-current",
        "has-background-inherit"
      ), {
        val prefixes: Seq[String] =
          Seq( "m", "mt", "mr", "mb", "ml", "mx", "my", "p", "pt", "pr", "pb", "pl", "px", "py" )
        val suffixes: Seq[String] =
          ( 0 to 6 ).map( _.toString ) :+ "auto"

        Classes( "Spacing" )(
          prefixes.flatMap( p => suffixes.map( s => s"$p-$s": Classes ) )*
        )
      },
      Classes( "Typography" )(
        Classes( "sizes" )(
          ( 1 to 7 ).map( n => s"is-size-$n": Classes )*
        ),
        Classes( "layout" )(
          "has-text-centered",
          "has-text-justified",
          "has-text-left",
          "has-text-right"
        ),
        Classes( "text transformation" )(
          "is-capitalized",
          "is-lowercase",
          "is-uppercase",
          "is-italic",
          "is-underlined"
        ),
        Classes( "text weight" )(
          "has-text-weight-light",
          "has-text-weight-normal",
          "has-text-weight-medium",
          "has-text-weight-semibold",
          "has-text-weight-bold",
          "has-text-weight-extrabold"
        ),
        Classes( "font family" )(
          "is-family-sans-serif",
          "is-family-monospace",
          "is-family-primary",
          "is-family-secondary",
          "is-family-code"
        )
      ),
      Classes( "Visibility" )(
        "is-block",
        "is-flex",
        "is-inline",
        "is-inline-block",
        "is-inline-flex"
      ),
      Classes( "Flexbox" )(
        Classes( "flex-direction" )(
          "is-flex-direction-row",
          "is-flex-direction-row-reverse",
          "is-flex-direction-column",
          "is-flex-direction-column-reverse"
        ),
        Classes( "flex-wrap" )(
          "is-flex-wrap-nowrap",
          "is-flex-wrap-wrap",
          "is-flex-wrap-wrap-reverse"
        ),
        Classes( "justify-content" )(
          "is-justify-content-flex-start",
          "is-justify-content-flex-end",
          "is-justify-content-center",
          "is-justify-content-space-between",
          "is-justify-content-space-around",
          "is-justify-content-space-evenly",
          "is-justify-content-start",
          "is-justify-content-end",
          "is-justify-content-left",
          "is-justify-content-right"
        ),
        Classes( "align-content" )(
          "is-align-content-flex-start",
          "is-align-content-flex-end",
          "is-align-content-center",
          "is-align-content-space-between",
          "is-align-content-space-around",
          "is-align-content-space-evenly",
          "is-align-content-stretch",
          "is-align-content-start",
          "is-align-content-end",
          "is-align-content-baseline"
        ),
        Classes( "align-items" )(
          "is-align-items-stretch",
          "is-align-items-flex-start",
          "is-align-items-flex-end",
          "is-align-items-center",
          "is-align-items-baseline",
          "is-align-items-start",
          "is-align-items-end",
          "is-align-items-self-start",
          "is-align-items-self-end"
        ),
        Classes( "align-self" )(
          "is-align-self-auto",
          "is-align-self-flex-start",
          "is-align-self-flex-end",
          "is-align-self-flex-center",
          "is-align-self-flex-baseline",
          "is-align-self-flex-stretch"
        ),
        Classes( "flex-grow" )(
          ( 0 to 5 ).map( n => s"is-flex-grow-$n": Classes )*
        ),
        Classes( "flex-shrink" )(
          ( 0 to 5 ).map( n => s"is-flex-shrink-$n": Classes )*
        )
      )
    )
  )
