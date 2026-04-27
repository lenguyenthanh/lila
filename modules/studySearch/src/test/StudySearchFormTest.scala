package lila.studySearch

import chess.opening.Eco

import lila.core.study.StudyOrder
import lila.search.spec.{ ChapterMode, TagFilter }

class StudySearchFormTest extends munit.FunSuite:

  // ── composedText ──────────────────────────────────────────────────────

  test("composedText: q only"):
    val data = StudySearchData(q = "najdorf carlsen")
    assertEquals(data.composedText, "najdorf carlsen")

  test("composedText: q trimmed"):
    val data = StudySearchData(q = "  najdorf  ")
    assertEquals(data.composedText, "najdorf")

  test("composedText: owner appended"):
    val data = StudySearchData(q = "najdorf", owner = UserStr("thibault").some)
    assertEquals(data.composedText, "najdorf owner:thibault")

  test("composedText: member appended"):
    val data = StudySearchData(q = "najdorf", member = UserStr("foo").some)
    assertEquals(data.composedText, "najdorf member:foo")

  test("composedText: owner + member appended in order"):
    val data = StudySearchData(
      q = "najdorf",
      owner = UserStr("thibault").some,
      member = UserStr("foo").some
    )
    assertEquals(data.composedText, "najdorf owner:thibault member:foo")

  test("composedText: empty q with owner"):
    val data = StudySearchData(owner = UserStr("thibault").some)
    assertEquals(data.composedText, "owner:thibault")

  test("composedText: all empty"):
    assertEquals(StudySearchData().composedText, "")

  // ── chapter builder ───────────────────────────────────────────────────

  test("chapter: mode None → None"):
    assertEquals(StudySearchData().chapter, None)

  test("chapter: ChapterText → SearchText"):
    val data = StudySearchData(mode = ChapterSearchMode.ChapterText.some)
    assertEquals(data.chapter, ChapterMode.searchText().some)

  test("chapter: ChapterFilters with one field set → Filters"):
    val data = StudySearchData(
      mode = ChapterSearchMode.ChapterFilters.some,
      eco = Eco("B90").some
    )
    assertEquals(
      data.chapter,
      ChapterMode.filters(TagFilter(eco = "B90".some)).some
    )

  test("chapter: ChapterFilters with all fields set → Filters carries every field"):
    val data = StudySearchData(
      mode = ChapterSearchMode.ChapterFilters.some,
      variant = chess.variant.Standard.some,
      eco = Eco("B90").some,
      opening = "Sicilian Najdorf".some,
      player1 = "Carlsen".some,
      player2 = "Caruana".some,
      fideId1 = "1503014".some,
      fideId2 = "2020009".some,
      event = "Norway Chess".some
    )
    val expected = ChapterMode.filters(
      TagFilter(
        variant = "Standard".some,
        eco = "B90".some,
        opening = "Sicilian Najdorf".some,
        player1 = "Carlsen".some,
        player2 = "Caruana".some,
        fideId1 = "1503014".some,
        fideId2 = "2020009".some,
        event = "Norway Chess".some
      )
    )
    assertEquals(data.chapter, expected.some)

  test("chapter: ChapterFilters with no fields set → None (decision 6c)"):
    val data = StudySearchData(mode = ChapterSearchMode.ChapterFilters.some)
    assertEquals(data.chapter, None)

  // ── form binding ──────────────────────────────────────────────────────

  private val form = StudySearchForm().search

  test("form: binds plain q"):
    val bound = form.bind(Map("q" -> "najdorf"))
    assert(bound.errors.isEmpty, bound.errors.toString)
    assertEquals(bound.value.map(_.q), "najdorf".some)

  test("form: defaults q to empty when missing"):
    val bound = form.bind(Map.empty[String, String])
    assert(bound.errors.isEmpty, bound.errors.toString)
    assertEquals(bound.value.map(_.q), "".some)

  test("form: rejects q over 100 chars"):
    val long = "a" * 101
    val bound = form.bind(Map("q" -> long))
    assert(bound.errors.exists(_.key == "q"), bound.errors.toString)

  test("form: binds known order"):
    val bound = form.bind(Map("q" -> "x", "order" -> "hot"))
    assertEquals(bound.value.flatMap(_.order), StudyOrder.hot.some)

  test("form: rejects unknown order"):
    val bound = form.bind(Map("q" -> "x", "order" -> "bogus"))
    assert(bound.errors.exists(_.key == "order"))

  test("form: binds known mode"):
    val bound = form.bind(Map("q" -> "x", "mode" -> "chapterFilters"))
    assertEquals(bound.value.flatMap(_.mode), ChapterSearchMode.ChapterFilters.some)

  test("form: rejects unknown mode"):
    val bound = form.bind(Map("q" -> "x", "mode" -> "chapterBogus"))
    assert(bound.errors.exists(_.key == "mode"))

  test("form: binds known variant"):
    val bound = form.bind(Map("q" -> "x", "variant" -> "standard"))
    assertEquals(bound.value.flatMap(_.variant), chess.variant.Standard.some)

  test("form: rejects unknown variant"):
    val bound = form.bind(Map("q" -> "x", "variant" -> "bogus"))
    assert(bound.errors.exists(_.key == "variant"))

  test("form: accepts and uppercases ECO"):
    val bound = form.bind(Map("q" -> "x", "eco" -> "b90"))
    assertEquals(bound.value.flatMap(_.eco.map(_.value)), "B90".some)

  test("form: rejects malformed ECO"):
    val bound = form.bind(Map("q" -> "x", "eco" -> "ZZ"))
    assert(bound.errors.exists(_.key == "eco"))

  test("form: rejects non-digit FIDE id"):
    val bound = form.bind(Map("q" -> "x", "fideId1" -> "12a3"))
    assert(bound.errors.exists(_.key == "fideId1"))

  test("form: accepts numeric FIDE id"):
    val bound = form.bind(Map("q" -> "x", "fideId1" -> "1503014"))
    assertEquals(bound.value.flatMap(_.fideId1), "1503014".some)
