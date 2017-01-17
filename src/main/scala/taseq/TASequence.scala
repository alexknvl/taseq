package taseq

import cats._
import cats.arrow.{Compose}
import leibniz.{Leibniz, ===}
import simulacrum.{op, typeclass}

@typeclass trait TASequence[Q[_[_, _], _, _]] { self =>
  def empty[F[_, _], A]: Q[F, A, A]
  def singleton[F[_, _], A, B](ab: F[A, B]): Q[F, A, B]

  @op("><") def concat [F[_, _], A, B, C](ab: Q[F, A, B], bc: Q[F, B, C]): Q[F, A, C]
  @op("|>") def append [F[_, _], A, B, C](ab: Q[F, A, B], bc:    F[B, C]): Q[F, A, C]
  @op("<|") def prepend[F[_, _], A, B, C](ab:    F[A, B], bc: Q[F, B, C]): Q[F, A, C]

  def uncons[F[_, _], A, B](ab: Q[F, A, B]): ViewL[Q, F, A, B]
  def unsnoc[F[_, _], A, B](ab: Q[F, A, B]): ViewR[Q, F, A, B]

  def mapQ[F[_, _], G[_, _], A, B](fab: Q[F, A, B])(fg: F ~~> G): Q[G, A, B]
}

sealed abstract class ViewR[Q[_[_, _], _, _], F[_, _], A, B] {
  trait FoldCons[R] {
    def apply[X](init: Q[F, A, X], last: F[X, B]): R
  }
  object FoldCons {
    def const[R](value: R): FoldCons[R] = new FoldCons[R] {
      def apply[X](init: Q[F, A, X], last: F[X, B]): R = value
    }
  }
  def fold[R](
    empty: B === A => R,
    cons: FoldCons[R]
  ): R
}
object ViewR {
  final case class Empty[Q[_[_, _], _, _], F[_, _], A]() extends ViewR[Q, F, A, A] {
    def fold[R](
      empty: A === A => R,
      cons: FoldCons[R]
    ): R = empty(Leibniz.refl[A])
  }
  final case class Cons[Q[_[_, _], _, _], F[_, _], A, B, C]
  (init: Q[F, A, B], last: F[B, C]) extends ViewR[Q, F, A, C] {
    def fold[R](
      empty: C === A => R,
      cons: FoldCons[R]
    ): R = cons.apply[B](init, last)
  }

  def empty[Q[_[_, _], _, _], F[_, _], A]: ViewR[Q, F, A, A] =
    Empty()
  def cons[Q[_[_, _], _, _], F[_, _], A, B, C](init: Q[F, A, B], last: F[B, C]): ViewR[Q, F, A, C] =
    Cons[Q, F, A, B, C](init, last)
}

/**
  * A view of the left (cons) side of type-aligned sequences
  * of type `Q`. May be either empty, in which case we obtain
  * evidence that `A === B`, or A `Cons` which reveals
  * an (existential) intermediate type between `A` and `B`.
  */
sealed abstract class ViewL[Q[_[_, _], _, _], F[_, _], A, B] {
  type Fold[R] = λ[X => (F[A, X], Q[F, X, B])] ~> λ[X => R]

  def handle[R](h: Fold[R])(r: (B === A) => R): R

  def asEmpty: Option[B === A]
}
object ViewL {
  case class Empty[Q[_[_, _], _, _], F[_,_], A]() extends ViewL[Q, F, A, A] {
    def handle[R](h: Fold[R])(r: (A === A) => R) = r(Leibniz.refl[A])
    def asEmpty = Some(Leibniz.refl[A])
  }
  case class Cons[Q[_[_,_],_,_], F[_,_], A, B, C](head: F[A, B], tail: Q[F, B, C]) extends ViewL[Q, F, A, C] {
    def handle[R](h: Fold[R])(r: (C === A) => R) = h.apply[B](head, tail)
    def asEmpty = None
  }

  def empty[Q[_[_, _], _, _], F[_, _], A]: ViewL[Q, F, A, A] =
    Empty()
  def cons[Q[_[_, _], _, _], F[_, _], A, B, C](head: F[A, B], tail: Q[F, B, C]): ViewL[Q, F, A, C] =
    Cons[Q, F, A, B, C](head, tail)
}