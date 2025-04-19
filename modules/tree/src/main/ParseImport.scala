package lila.tree

import chess.format.Fen
import chess.format.pgn.{ ParsedPgn, Parser, PgnStr, Reader, Sans, Tags }
import chess.variant.*
import chess.{ Game as ChessGame, * }

case class TagResult(status: Status, points: Outcome.GamePoints):
  // duplicated from Game.finish
  def finished              = status >= Status.Mate
  def winner: Option[Color] = Outcome.fromPoints(points).flatMap(_.winner)

case class ImportResult(
    game: ChessGame,
    result: Option[TagResult],
    replay: Replay,
    initialFen: Option[Fen.Full],
    parsed: ParsedPgn,
    replayError: Option[ErrorStr]
)

private val maxPlies = 600

def parseImport(pgn: PgnStr): Either[ErrorStr, ImportResult] =
  catchOverflow: () =>
    Parser.full(pgn).map { parsed =>
      Reader
        .fullWithSans(parsed, _.map(_.take(maxPlies)))
        .pipe:
          case Reader.Result.Complete(replay)          => (replay, none[ErrorStr])
          case Reader.Result.Incomplete(replay, error) => (replay, error.some)
        .pipe { (replay, relayError) =>
          val variant    = extractVariant(replay, parsed.tags)
          val initialFen = parsed.tags.fen.flatMap(Fen.readWithMoveNumber(variant, _)).map(Fen.write)
          val game       = replay.state.copy(situation = replay.state.situation.withVariant(variant))
          val result     = extractResult(game, parsed.tags)
          ImportResult(game, result, replay.copy(state = game), initialFen, parsed, relayError)
        }
    }

def extractVariant(replay: Replay, tags: Tags): Variant =
  inline def initBoard  = tags.fen.flatMap(Fen.read).map(_.board)
  lazy val fromPosition = initBoard.nonEmpty && !tags.fen.exists(_.isInitial)

  tags.variant | {
    if fromPosition then FromPosition
    else Standard
  } match
    case Chess960 if !isChess960StartPosition(replay.setup.situation) =>
      FromPosition
    case FromPosition if tags.fen.isEmpty => Standard
    case Standard if fromPosition         => FromPosition
    case v                                => v

def extractResult(game: ChessGame, tags: Tags): Option[TagResult] =
  val status = tags(_.Termination).map(_.toLowerCase) match
    case Some("normal") =>
      game.situation.status | (if tags.outcome.exists(_.winner.isEmpty) then Status.Draw else Status.Resign)
    case Some("abandoned")                        => Status.Aborted
    case Some("time forfeit")                     => Status.Outoftime
    case Some("rules infraction")                 => Status.Cheat
    case Some(txt) if txt.contains("won on time") => Status.Outoftime
    case _                                        => Status.UnknownFinish

  tags.points
    .map(points => TagResult(status, points))
    .filter(_.status > Status.Started)
    .orElse {
      game.situation.status.flatMap: status =>
        Outcome
          .guessPointsFromStatusAndPosition(status, game.situation.winner)
          .map(TagResult(status, _))
    }

private def isChess960StartPosition(sit: Situation) =
  import chess.*
  val strict =
    def rankMatches(f: Option[Piece] => Boolean)(rank: Rank) =
      File.all.forall: file =>
        f(sit.board(file, rank))
    rankMatches {
      case Some(Piece(White, King | Queen | Rook | Knight | Bishop)) => true
      case _                                                         => false
    }(Rank.First) &&
    rankMatches {
      case Some(Piece(White, Pawn)) => true
      case _                        => false
    }(Rank.Second) &&
    List(Rank.Third, Rank.Fourth, Rank.Fifth, Rank.Sixth).forall(rankMatches(_.isEmpty)) &&
    rankMatches {
      case Some(Piece(Black, Pawn)) => true
      case _                        => false
    }(Rank.Seventh) &&
    rankMatches {
      case Some(Piece(Black, King | Queen | Rook | Knight | Bishop)) => true
      case _                                                         => false
    }(Rank.Eighth)

  Chess960.valid(sit, strict)

private def catchOverflow[A](f: () => Either[ErrorStr, A]): Either[ErrorStr, A] =
  try f()
  catch
    case e: RuntimeException if e.getMessage.contains("StackOverflowError") =>
      ErrorStr("This PGN seems too long or too complex!").asLeft
