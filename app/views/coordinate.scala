package views.html

import play.api.libs.json.Json

import lila.api.Context
import lila.app.templating.Environment._
import lila.app.ui.ScalatagsTemplate._
import lila.common.String.html.safeJsonValue
import lila.pref.Pref.Color
import play.api.i18n.Lang

import controllers.routes

object CoordinateMode {
  val FIND_SQUARE     = 1
  val GIVE_COORDINATE = 2

  val choices = Seq(
    FIND_SQUARE     -> "Find Square",
    GIVE_COORDINATE -> "Give Coordinate"
  )
  // val asString = Map(
  //   1 -> "findSquare",
  //   2 -> "giveCoordinate"
  // )
}

object coordinate {

  def home(scoreOption: Option[lila.coordinate.Score])(implicit ctx: Context) =
    views.html.base.layout(
      title = trans.coordinates.coordinateTraining.txt(),
      moreCss = cssTag("coordinate"),
      moreJs = jsModule("coordinate"),
      openGraph = lila.app.ui
        .OpenGraph(
          title = "Chess board coordinates trainer",
          url = s"$netBaseUrl${routes.Coordinate.home.url}",
          description =
            "Knowing the chessboard coordinates is a very important chess skill. A square name appears on the board and you must click on the correct square."
        )
        .some,
      zoomable = true,
      playing = true
    )(
      main(
        id := "trainer",
        cls := "coord-trainer training init",
        attr("data-color-pref") := ctx.pref.coordColorName,
        attr("data-resize-pref") := ctx.pref.resizeHandle,
        attr("data-score-url") := ctx.isAuth.option(routes.Coordinate.score.url)
      )(
        div(cls := "coord-trainer__side")(
          div(cls := "box charts")(
            h1("Find Square"),
            if (ctx.isAuth) scoreOption.map { score =>
              div(cls := "scores")(scoreCharts(score))
            }
          ),
          form(
            cls := "mode buttons",
            method := "post",
            autocomplete := "off"
          )(
            st.group(cls := "radio")(
              CoordinateMode.choices.map { case (id, labelText) =>
                div(cls := "mode-choice")(
                  input(
                    tpe := "radio",
                    st.id := s"coord_mode_$id",
                    name := "mode",
                    value := id,
                    (id == CoordinateMode.FIND_SQUARE) option checked
                  ),
                  label(`for` := s"coord_mode_$id", cls := s"mode mode_$id")(labelText)
                )
              }
            )
          ),
          form(
            cls := "color buttons",
            action := routes.Coordinate.color,
            method := "post",
            autocomplete := "off"
          )(
            st.group(cls := "radio")(
              List(Color.BLACK, Color.RANDOM, Color.WHITE).map { id =>
                div(
                  input(
                    tpe := "radio",
                    st.id := s"coord_color_$id",
                    name := "color",
                    value := id,
                    (id == ctx.pref.coordColor) option checked
                  ),
                  label(`for` := s"coord_color_$id", cls := s"color color_$id")(i)
                )
              }
            )
          ),
          div(cls := "box current-status")(
            h1(trans.storm.score()),
            div(cls := "coord-trainer__score")(0)
          ),
          div(cls := "box current-status")(
            h1(trans.time()),
            div(cls := "coord-trainer__timer")(30.0)
          )
        ),
        div(cls := "coord-trainer__board main-board")(
          svgTag(cls := "coords-svg", viewBoxAttr := "0 0 100 100")(
            svgGroupTag(cls := "coord coord--resolved")(svgTextTag),
            svgGroupTag(cls := "coord coord--current")(svgTextTag),
            svgGroupTag(cls := "coord coord--next")(svgTextTag),
            svgGroupTag(cls := "coord coord--new")(svgTextTag)
          ),
          chessgroundBoard
        ),
        div(cls := "coord-trainer__table")(
          div(cls := "explanation")(
            p(trans.coordinates.knowingTheChessBoard()),
            ul(
              li(trans.coordinates.mostChessCourses()),
              li(trans.coordinates.talkToYourChessFriends()),
              li(trans.coordinates.youCanAnalyseAGameMoreEffectively())
            ),
            p(trans.coordinates.aSquareNameAppears())
          ),
          button(cls := "start button button-fat")(trans.coordinates.startTraining())
        ),
        div(cls := "coord-trainer__progress")(div(cls := "progress_bar")),
        div(cls := "coord-trainer__kb-input")(
          input(cls := "keyboard"),
          strong("Press <enter> to focus")
        )
      )
    )

  def scoreCharts(score: lila.coordinate.Score)(implicit ctx: Context) =
    frag(
      List(
        (trans.coordinates.averageScoreAsWhiteX, score.white),
        (trans.coordinates.averageScoreAsBlackX, score.black)
      ).map { case (averageScoreX, s) =>
        div(cls := "chart_container")(
          s.nonEmpty option frag(
            p(averageScoreX(raw(s"""<strong>${"%.2f".format(s.sum.toDouble / s.size)}</strong>"""))),
            div(cls := "user_chart", attr("data-points") := safeJsonValue(Json toJson s))
          )
        )
      }
    )
}
