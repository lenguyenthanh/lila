package lila.study
package ui

import play.api.data.Form
import scalalib.paginator.Paginator

import lila.core.study.StudyOrder
import lila.study.Study.WithChaptersAndLiked
import lila.ui.*

import ScalatagsTemplate.{ *, given }

final class StudyListUi(helpers: Helpers, bits: StudyBits, emptySearchForm: () => Form[?]):
  import helpers.{ *, given }
  import trans.study as trs

  def all(pag: Paginator[WithChaptersAndLiked], order: StudyOrder)(using Context) =
    page(
      title = trs.allStudies.txt(),
      active = StudyGroup.all,
      order = order,
      pag = pag,
      searchFilter = "",
      url = routes.Study.all(_)
    )
      .hrefLangs(lila.ui.LangPath(routes.Study.allDefault()))

  def byOwner(pag: Paginator[WithChaptersAndLiked], order: StudyOrder, owner: User)(using Context) =
    page(
      title = trs.studiesCreatedByX.txt(owner.titleUsername),
      active = StudyGroup.byOwner,
      order = order,
      pag = pag,
      searchFilter = s"owner:${owner.username}",
      url = routes.Study.byOwner(owner.username, _)
    )

  def mine(pag: Paginator[WithChaptersAndLiked], order: StudyOrder, topics: StudyTopics)(using
      ctx: Context,
      me: Me
  ) =
    page(
      title = trs.myStudies.txt(),
      active = StudyGroup.mine,
      order = order,
      pag = pag,
      searchFilter = s"owner:${me.username}",
      url = routes.Study.mine(_),
      topics = topics.some
    )

  def mineLikes(
      pag: Paginator[WithChaptersAndLiked],
      order: StudyOrder
  )(using Context) =
    page(
      title = trs.myFavoriteStudies.txt(),
      active = StudyGroup.mineLikes,
      order = order,
      pag = pag,
      searchFilter = "",
      url = routes.Study.mineLikes(_)
    )

  def mineMember(pag: Paginator[WithChaptersAndLiked], order: StudyOrder, topics: StudyTopics)(using
      ctx: Context,
      me: Me
  ) =
    page(
      title = trs.studiesIContributeTo.txt(),
      active = StudyGroup.mineMember,
      order = order,
      pag = pag,
      searchFilter = s"member:${me.username}",
      url = routes.Study.mineMember(_),
      topics = topics.some
    )

  def minePublic(pag: Paginator[WithChaptersAndLiked], order: StudyOrder)(using Context)(using me: Me) =
    page(
      title = trs.myPublicStudies.txt(),
      active = StudyGroup.minePublic,
      order = order,
      pag = pag,
      searchFilter = s"owner:${me.username}",
      url = routes.Study.minePublic(_)
    )

  def minePrivate(pag: Paginator[WithChaptersAndLiked], order: StudyOrder)(using Context)(using me: Me) =
    page(
      title = trs.myPrivateStudies.txt(),
      active = StudyGroup.minePrivate,
      order = order,
      pag = pag,
      searchFilter = s"owner:${me.username}",
      url = routes.Study.minePrivate(_)
    )

  def search(
      pag: Option[Paginator[WithChaptersAndLiked]],
      form: Form[?],
      order: StudyOrder
  )(using Context) =
    val text = ~form("q").value
    Page(text)
      .css("analyse.study.index")
      .js(Esm("bits.studyAdvancedSearch"))
      .js(infiniteScrollEsmInit):
        main(cls := "page-menu")(
          menu(StudyGroup.search, Some(order)),
          main(cls := "page-menu__content study-index box")(
            div(cls := "box__top")(
              searchForm(trans.search.search.txt(), text, order, panelForm = form.some),
              bits.orderSelect(order, StudyGroup.search, url = o => searchUrlFor(form, o.some)),
              bits.newForm()
            ),
            pag.fold(emptyFrag)(p => paginate(p, searchUrlFor(form, order.some)))
          )
        )

  private def searchUrlFor(form: Form[?], order: Option[StudyOrder]): Call =
    val base = routes.Study.search()
    val params: Map[String, String] = form.data.collect:
      case (k, v) if v.nonEmpty && k != "order" && k != "page" => k -> v
    val withOrder = order.fold(params)(o => params + ("order" -> o.key))
    Call(base.method, addQueryParams(base.url, withOrder), base.fragment)

  private def page(
      title: String,
      active: StudyGroup,
      order: StudyOrder,
      pag: Paginator[WithChaptersAndLiked],
      url: StudyOrder => Call,
      searchFilter: String,
      topics: Option[StudyTopics] = None
  )(using Context): Page =
    Page(title)
      .css("analyse.study.index")
      .js(Esm("bits.studyAdvancedSearch"))
      .js(infiniteScrollEsmInit):
        main(cls := "page-menu")(
          menu(active, Some(order), topics.so(_.value)),
          main(cls := "page-menu__content study-index box")(
            div(cls := "box__top")(
              searchForm(title, s"$searchFilter${searchFilter.nonEmpty.so(" ")}", order),
              bits.orderSelect(order, active, url),
              bits.newForm()
            ),
            topics.map: ts =>
              div(cls := "box__pad")(topic.topicsList(ts, StudyOrder.mine)),
            paginate(pag, url(order))
          )
        )

  private def paginate(pager: Paginator[WithChaptersAndLiked], url: Call)(using Context) =
    if pager.currentPageResults.isEmpty then
      div(cls := "nostudies")(
        iconTag(Icon.StudyBoard),
        p(trs.noneYet())
      )
    else
      div(cls := "studies list infinite-scroll")(
        pager.currentPageResults.map { s =>
          div(cls := "study paginated")(bits.widget(s))
        },
        pagerNext(pager, np => addQueryParam(url.url, "page", np.toString))
      )

  def menu(active: StudyGroup, order: Option[StudyOrder], topics: List[StudyTopic] = Nil)(using
      ctx: Context
  ) =
    def defaultOrder(group: StudyGroup): Option[StudyOrder] =
      if group == StudyGroup.search || group == StudyGroup.staffPicks then None
      else if group.isTopic then Some(StudyOrder.mine)
      else if group.isPersonal then Some(StudyOrder.updated)
      else Some(StudyOrder.hot)
    def newOrder(newGroup: StudyGroup): StudyOrder =
      (if defaultOrder(active).forall(order.contains) then defaultOrder(newGroup) else order)
        .getOrElse(Orders.default)
    def activeCls(group: StudyGroup) = cls := (
      group match
        case StudyGroup.topic(None) => active.isTopic
        case _ => group == active
    ).option("active")
    lila.ui.bits.pageMenuSubnav(
      a(activeCls(StudyGroup.all), href := routes.Study.all(newOrder(StudyGroup.all)))(trs.allStudies()),
      ctx.isAuth.option(bits.authLinks(activeCls, newOrder)),
      a(activeCls(StudyGroup.topic(None)), href := routes.Study.topics)(trs.topics()),
      topics.map: topic =>
        val group = StudyGroup.topic(topic.some)
        a(activeCls(group), href := routes.Study.byTopic(topic.value, newOrder(group)))(
          topic.value
        )
      ,
      a(activeCls(StudyGroup.staffPicks), href := routes.Study.staffPicks)("Staff picks"),
      a(
        dataIcon := Icon.InfoCircle,
        href := "/@/lichess/blog/study-chess-the-lichess-way/V0KrLSkA"
      )(trs.whatAreStudies())
    )

  def searchForm(
      placeholder: String,
      value: String,
      order: StudyOrder,
      panelForm: Option[Form[?]] = None
  ) =
    val panel = advancedPanel(panelForm | emptySearchForm())
    form(cls := "search", action := routes.Study.search(), method := "get")(
      form3.hidden("order", order.key),
      input(name := "q", st.placeholder := placeholder, st.value := value, enterkeyhint := "search"),
      submitButton(cls := "button", dataIcon := Icon.Search),
      button(
        cls := "button button-empty search__advanced-toggle",
        tpe := "button",
        attr("aria-controls") := "search-advanced",
        attr("aria-expanded") := "false"
      )("Advanced"),
      panel
    )

  def advancedPanel(form: Form[?]): Frag =
    val mode = ~form("mode").value
    val isFiltersMode = mode == "chapterFilters"
    val isTextMode = mode == "chapterText"
    val isStudyOnly = mode.isEmpty
    val refining = ~form("q").value
    div(cls := "search__advanced", id := "search-advanced")(
      div(cls := "search__advanced-head")(
        div(cls := "search__advanced-title")(
          h2("Advanced search"),
          div(cls := "search__advanced-refining")(
            "Refining: ",
            span(cls := "search__advanced-q")(if refining.nonEmpty then s""""$refining"""" else "all studies"),
            span(cls := "search__advanced-edit")(" — edit above to change query")
          )
        ),
        div(cls := "search__advanced-modes", role := "radiogroup", attr("aria-label") := "Search mode")(
          modeRadio(form("mode"), "", "Studies only", isStudyOnly),
          modeRadio(form("mode"), "chapterText", "+ Chapter text", isTextMode),
          modeRadio(form("mode"), "chapterFilters", "+ Chapter filters", isFiltersMode)
        )
      ),
      div(
        cls := List(
          "search__advanced-hint" -> true,
          "is-hidden" -> !isStudyOnly
        ),
        attr("data-mode") := ""
      )("Text will match study name, description, and topics — but no chapter data."),
      div(
        cls := List(
          "search__advanced-hint" -> true,
          "is-hidden" -> !isTextMode
        ),
        attr("data-mode") := "chapterText"
      )("Same text will also match chapter names, descriptions, and PGN tags."),
      table(cls := "search__advanced-fields")(
        tr(
          th(label("Owner")),
          td(form3.input(form("owner"))(st.placeholder := "Username"))
        ),
        tr(
          th(label("Member")),
          td(form3.input(form("member"))(st.placeholder := "Username"))
        )
      ),
      div(
        cls := List("search__advanced-divider" -> true, "is-hidden" -> !isFiltersMode),
        attr("data-mode") := "chapterFilters"
      )(
        span(cls := "search__advanced-divider-line"),
        span(cls := "search__advanced-divider-label")("Chapter filters"),
        span(cls := "search__advanced-divider-line")
      ),
      table(
        cls := List("search__advanced-fields" -> true, "filters" -> true, "is-hidden" -> !isFiltersMode)
      )(
        tr(
          th(label("Variant")),
          td(
            form3.select(
              form("variant"),
              chess.variant.Variant.list.all.map(v => v.key.value -> v.name),
              default = "Any variant".some
            )
          )
        ),
        tr(
          th(label("ECO")),
          td(
            form3.input(form("eco"))(
              st.placeholder := "e.g. B90",
              pattern := "[A-Ea-e][0-9]{2}",
              maxlength := 3
            )
          )
        ),
        tr(
          th(label("Opening")),
          td(form3.input(form("opening"))(st.placeholder := "e.g. Sicilian Najdorf"))
        ),
        tr(
          th(label("Player(s)")),
          td(cls := "two-columns")(
            form3.input(form("player1"))(st.placeholder := "Player name"),
            form3.input(form("player2"))(st.placeholder := "(optional)")
          )
        ),
        tr(
          th,
          td(cls := "search__advanced-help"):
            "One name matches either color. Two names match the pair (any side)."
        ),
        tr(
          th(label("FIDE ID(s)")),
          td(cls := "two-columns")(
            form3.input(form("fideId1"))(st.placeholder := "1503014", pattern := "[0-9]+"),
            form3.input(form("fideId2"))(st.placeholder := "(optional)", pattern := "[0-9]+")
          )
        ),
        tr(
          th(label("Event")),
          td(form3.input(form("event"))(st.placeholder := "e.g. Norway Chess 2025"))
        )
      ),
      div(cls := "search__advanced-footer")(
        a(href := routes.Study.search(), cls := "button button-empty")("Reset"),
        submitButton(cls := "button button-green")("Search")
      )
    )

  private def modeRadio(field: play.api.data.Field, value: String, lbl: String, checked: Boolean): Frag =
    label(cls := List("search__advanced-mode" -> true, "is-active" -> checked))(
      input(tpe := "radio", name := field.name, st.value := value, checked.option(st.checked)),
      span(lbl)
    )

  object topic:

    def topicsList(topics: StudyTopics, order: StudyOrder = Orders.default) =
      div(cls := "topic-list")(
        topics.value.map: t =>
          a(href := routes.Study.byTopic(t.value, order))(t.value)
      )

    def index(popular: StudyTopics, mine: Option[StudyTopics], myForm: Option[Form[?]])(using Context) =
      Page(trans.study.topics.txt())
        .css("analyse.study.index", "bits.form3", "bits.tagify")
        .js(Esm("analyse.study.topic.form")):
          main(cls := "page-menu")(
            menu(StudyGroup.topic(None), Some(StudyOrder.mine), mine.so(_.value)),
            main(cls := "page-menu__content study-topics box box-pad")(
              h1(cls := "box__top")(trans.study.topics()),
              myForm.map { form =>
                frag(
                  h2(trans.study.myTopics()),
                  postForm(cls := "form3", action := routes.Study.topics)(
                    form3.textarea(form("topics"))(rows := 10, attrData("max") := StudyTopics.userMax),
                    form3.submit(trans.site.save())
                  )
                )
              },
              h2(trans.study.popularTopics()),
              topicsList(popular)
            )
          )

    def show(
        topic: StudyTopic,
        pag: Paginator[WithChaptersAndLiked],
        order: StudyOrder,
        myTopics: Option[StudyTopics]
    )(using Context) =
      val active = StudyGroup.topic(topic.some)
      val url = (o: StudyOrder) => routes.Study.byTopic(topic.value, o)
      Page(topic.value)
        .css("analyse.study.index")
        .js(infiniteScrollEsmInit):
          main(cls := "page-menu")(
            menu(active, Some(order), myTopics.so(_.value)),
            main(cls := "page-menu__content study-index box")(
              boxTop(
                h1(topic.value),
                bits.orderSelect(order, active, url),
                bits.newForm()
              ),
              myTopics.ifTrue(order == StudyOrder.mine).map { ts =>
                div(cls := "box__pad")(
                  topicsList(ts, StudyOrder.mine)
                )
              },
              paginate(pag, url(order))
            )
          )
