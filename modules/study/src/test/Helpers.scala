package lila.study

import chess.format.pgn.{ PgnStr, Tags }
import chess.{ Node as PgnNode, Tree }
import monocle.syntax.all.*
import alleycats.Zero

import lila.tree.Node.{ Comment, Comments }
import lila.tree.{ Branch, Branches, Metas, NewBranch, NewRoot, NewTree, Node, Root }

trait LilaTest extends munit.FunSuite with EitherAssertions:

  def assertMatch[A](a: A)(f: PartialFunction[A, Boolean])(using munit.Location) =
    assert(f.lift(a) | false, s"$a does not match expectations")

  def assertCloseTo[T](a: T, b: T, delta: Double)(using n: Numeric[T])(using munit.Location) =
    assert(scalalib.Maths.isCloseTo(a, b, delta), s"$a is not close to $b by $delta")

  extension [A](a: A)
    def matchZero[B: Zero](f: PartialFunction[A, B])(using munit.Location): B =
      f.lift(a) | Zero[B].zero

trait EitherAssertions extends munit.Assertions:

  extension [E, A](v: Either[E, A])
    def assertRight(f: A => Any)(using munit.Location): Any = v match
      case Right(r) => f(r)
      case Left(e)  => fail(s"Expected Right but received $v")

object Helpers:
  import lila.tree.NewTree.*

  def rootToPgn(root: Root): PgnStr = PgnDump
    .rootToPgn(root, Tags.empty)(using PgnDump.WithFlags(true, true, true, true, false, none))
    .render

  def rootToPgn(root: NewRoot): PgnStr = PgnDump
    .rootToPgn(root, Tags.empty)(using PgnDump.WithFlags(true, true, true, true, false, none))
    .render

  extension (root: Root)
    def toNewRoot: NewRoot = NewRoot(root)

    def withoutClockTrust: Root =
      toNewRoot.withoutClockTrust.toRoot

    def debug = root.ppAs(rootToPgn)

  extension (root: NewRoot)

    def cleanup: NewRoot =
      root
        .focus(_.tree.some)
        .modify(_.map(_.cleanup))
        .focus(_.metas.comments)
        .modify(_.cleanup)

    def withoutClockTrust: NewRoot =
      root
        .focus(_.metas.clock.some.trust)
        .replace(none)
        .focus(_.tree.some)
        .modify(_.map(_.focus(_.metas.clock.some.trust).replace(none)))

    def debug = root.ppAs(rootToPgn)

  extension (comments: Comments)
    def cleanup: Comments =
      Comments(comments.value.map(_.copy(id = Comment.Id("i"))))

  extension (node: NewBranch)
    def cleanup: NewBranch =
      node
        .focus(_.metas.clock)
        .replace(none)
        .focus(_.metas.comments)
        .modify(_.cleanup)

  def sanStr(node: Tree[NewBranch]): String = node.value.move.san.value
