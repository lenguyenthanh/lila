package lila.studySearch

import com.softwaremill.macwire.*
import scalalib.paginator.*

import lila.search.*
import lila.search.client.SearchClient
import lila.study.Study

final class Env(
    studyRepo: lila.study.StudyRepo,
    pager: lila.study.StudyPager,
    client: SearchClient
)(using Executor):

  val api: StudySearchApi = wire[StudySearchApi]

  lazy val form: StudySearchForm = wire[StudySearchForm]

  def apply(data: StudySearchData, page: Int)(using me: Option[Me]) =
    Paginator[Study.WithChaptersAndLiked](
      adapter = new AdapterLike[Study]:
        def query = data.query
        def nbResults = api.count(query).dmap(_.toInt)
        def slice(offset: Int, length: Int) = api.search(query, From(offset), Size(length))
      .mapFutureList(pager.withChaptersAndLiking()),
      currentPage = page,
      maxPerPage = pager.maxPerPage
    )
