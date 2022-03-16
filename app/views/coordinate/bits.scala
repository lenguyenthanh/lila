package views.html.coordinate

import controllers.routes
import play.api.libs.json.Json

import lila.api.Context
import lila.app.templating.Environment._
import lila.app.ui.ScalatagsTemplate._

object bits {

  def coordinateConfig(scoreOption: Option[lila.coordinate.Score])(implicit ctx: Context) = Json.obj(
    "i18n"       -> i18nJsObject(i18nKeys),
    "colorPref"  -> ctx.pref.coordColorName,
    "resizePref" -> ctx.pref.resizeHandle,
    "is3d"       -> ctx.pref.is3d,
    "scores" -> Json.obj(
      "white" -> scoreOption.fold(List[Int]())(_.white),
      "black" -> scoreOption.fold(List[Int]())(_.black)
    )
  )

  private val i18nKeys = List(
    trans.coordinates.aSquareNameAppears,
    trans.coordinates.averageScoreAsBlackX,
    trans.coordinates.averageScoreAsWhiteX,
    trans.coordinates.coordinates,
    trans.coordinates.knowingTheChessBoard,
    trans.coordinates.mostChessCourses,
    trans.coordinates.startTraining,
    trans.coordinates.talkToYourChessFriends,
    trans.coordinates.youCanAnalyseAGameMoreEffectively,
    trans.storm.score,
    trans.time
  ).map(_.key)
}