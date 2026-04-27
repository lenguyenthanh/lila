package lila.studySearch

import chess.opening.Eco
import chess.variant.Variant
import play.api.data.*
import play.api.data.Forms.*

import lila.common.Form.{ stringIn, username }
import lila.core.study.StudyOrder
import lila.search.spec.{ ChapterMode, Query, StudySorting, TagFilter }
import lila.search.spec.{ Order as SpecOrder, StudySortField }

extension (x: StudyOrder)
  private[studySearch] def toSpec: Option[StudySorting] =
    x match
      case StudyOrder.alphabetical => StudySorting(StudySortField.Name, SpecOrder.Asc).some
      case StudyOrder.hot => StudySorting(StudySortField.Hot, SpecOrder.Desc).some
      case StudyOrder.newest => StudySorting(StudySortField.CreatedAt, SpecOrder.Desc).some
      case StudyOrder.oldest => StudySorting(StudySortField.CreatedAt, SpecOrder.Asc).some
      case StudyOrder.popular => StudySorting(StudySortField.Likes, SpecOrder.Desc).some
      case StudyOrder.updated => StudySorting(StudySortField.UpdatedAt, SpecOrder.Desc).some
      case StudyOrder.mine => StudySorting(StudySortField.Likes, SpecOrder.Asc).some
      case StudyOrder.relevant => none

case class StudySearchData(
    q: String = "",
    order: Option[StudyOrder] = None,
    mode: Option[ChapterSearchMode] = None,
    owner: Option[UserStr] = None,
    member: Option[UserStr] = None,
    variant: Option[Variant] = None,
    eco: Option[Eco] = None,
    opening: Option[String] = None,
    player1: Option[String] = None,
    player2: Option[String] = None,
    fideId1: Option[String] = None,
    fideId2: Option[String] = None,
    event: Option[String] = None
):

  // Decision #1: owner/member fold into the text field via QueryParser prefix syntax,
  // until the lila-search Smithy spec gains explicit fields (Option B follow-up).
  def composedText: String =
    val parts = q.trim ::
      owner.map(u => s"owner:${u.value}").toList :::
      member.map(u => s"member:${u.value}").toList
    parts.filter(_.nonEmpty).mkString(" ")

  // Decision #2 + #6c. ChapterFilters with all blank tag fields → None.
  def chapter: Option[ChapterMode] =
    mode match
      case None => None
      case Some(ChapterSearchMode.ChapterText) => ChapterMode.searchText().some
      case Some(ChapterSearchMode.ChapterFilters) =>
        val tf = TagFilter(
          // Match what the lila-search ingestor stores (raw PGN Variant tag).
          // lila's PgnDump emits variant.name.capitalize, so the ES keyword
          // field is "Standard", "Chess960", "King of the Hill", …
          variant = variant.map(_.name.capitalize),
          eco = eco.map(_.value),
          opening = opening,
          player1 = player1,
          player2 = player2,
          fideId1 = fideId1,
          fideId2 = fideId2,
          event = event
        )
        Option.when(tf.productIterator.exists { case Some(_) => true; case _ => false }):
          ChapterMode.filters(tf)

  def query(using me: Option[Me]): Query.Study =
    Query.Study(
      text = composedText,
      sorting = (order | StudyOrder.relevant).toSpec,
      userId = me.map(_.userId.value),
      chapter = chapter
    )

enum ChapterSearchMode(val key: String):
  case ChapterText extends ChapterSearchMode("chapterText")
  case ChapterFilters extends ChapterSearchMode("chapterFilters")

object ChapterSearchMode:
  val byKey: Map[String, ChapterSearchMode] = values.toList.mapBy(_.key)

final class StudySearchForm:

  val search: Form[StudySearchData] = Form(
    mapping(
      "q" -> default(text(maxLength = 100), ""),
      "order" -> optional(stringIn(StudyOrder.all)(_.key)),
      "mode" -> optional(stringIn(ChapterSearchMode.values.toList)(_.key)),
      "owner" -> optional(username.historicalField),
      "member" -> optional(username.historicalField),
      "variant" -> optional(stringIn(Variant.list.all)(_.key.value)),
      "eco" -> optional(text.verifying("invalid ECO code", _.matches("^[A-Ea-e][0-9]{2}$")))
        .transform(_.map(s => Eco(s.toUpperCase)), _.map(_.value)),
      "opening" -> optional(nonEmptyText),
      "player1" -> optional(nonEmptyText),
      "player2" -> optional(nonEmptyText),
      "fideId1" -> optional(text.verifying("only digits", _.forall(_.isDigit))),
      "fideId2" -> optional(text.verifying("only digits", _.forall(_.isDigit))),
      "event" -> optional(nonEmptyText)
    )(StudySearchData.apply)(unapply)
  ).fill(StudySearchData())
