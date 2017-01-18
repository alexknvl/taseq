package taseq

import cats.{Eval, Functor}
import leibniz.{Leibniz, ===}
import simulacrum.{op, typeclass}

/**
  * A type class for type aligned sequences.
  * Minimal complete definition:
  *   empty, singleton, (uncons or unsnoc), and
  *   (concat or append or prepend).
  *
  * Instances should satisfy the following laws:
  *
  * Category laws:
  * empty >< x == x
  * x >< empty == x
  * (x >< y) >< z = x >< (y >< z)
  *
  * Observation laws:
  * uncons (singleton e >< s) == e :< s
  * uncons empty == ViewL.empty
  */
@typeclass trait TASequence[Q[_[_, _], _, _]] { self =>
  /**
    * Create an empty type-aligned sequence.
    */
  def empty[F[_, _], A]: Q[F, A, A]

  /**
    * Create a type-aligned sequence consisting of a single element.
    */
  def singleton[F[_, _], A, B](ab: F[A, B]): Q[F, A, B]

  /**
    * Concatenate two type aligned sequences together.
    */
  @op("><") def concat [F[_, _], A, B, C](ab: Q[F, A, B], bc: Q[F, B, C]): Q[F, A, C] = {
    def go[α, β, γ](ab: Q[F, α, β], bc: Q[F, β, γ]): Eval[Q[F, α, γ]] = {
      type Z = Eval[Q[F, α, γ]]

      val u = uncons(ab)
      u.cata(new u.Cata[Z] {
        def empty(proof: α === β): Z =
          Eval.now(proof.flip.subst[Q[F, ?, γ]](bc))
        def cons[X](head: F[α, X], tail: Q[F, X, β]): Z =
          Eval.defer(go(tail, bc)).map(q => prepend[F, α, X, γ](head, q))
      })
    }

    go(ab, bc).value
  }

  /**
    * Append a single element to the right.
    */
  @op("|>") def append [F[_, _], A, B, C](ab: Q[F, A, B], bc: F[B, C]): Q[F, A, C] =
    concat(ab, singleton(bc))

  /**
    * Append a single element to the left.
    */
  @op("<|") def prepend[F[_, _], A, B, C](ab: F[A, B], bc: Q[F, B, C]): Q[F, A, C] =
    concat(singleton(ab), bc)

  /**
    * View a type aligned sequence from the left.
    */
  def uncons[F[_, _], A, B](ab: Q[F, A, B]): ViewL[Q, F, A, B] = {
    type Z = ViewL[Q, F, A, B]

    val u1 = unsnoc(ab)
    u1.cata(new u1.Cata[Z] {
      def empty(proof: B === A): Z =
        proof.subst[ViewL[Q, F, ?, B]](ViewL.empty[Q, F, B])

      def cons[X](init: Q[F, A, X], last: F[X, B]): Z = {
        val u2 = uncons(init)
        u2.cata(new u2.Cata[Z] {
          def empty(proof: A === X): Z =
            ViewL.cons[Q, F, A, B, B](proof.flip.subst[F[?, B]](last), self.empty)

          def cons[Y](head: F[A, Y], tail: Q[F, Y, X]): Z =
            ViewL.cons[Q, F, A, Y, B](head, append[F, Y, X, B](tail, last))
        })
      }
    })
  }

  /**
    * View a type aligned sequence from the right.
    */
  def unsnoc[F[_, _], A, B](ab: Q[F, A, B]): ViewR[Q, F, A, B] = {
    type Z = ViewR[Q, F, A, B]

    val u1 = uncons(ab)
    u1.cata(new u1.Cata[Z] {
      def empty(proof: A === B): Z =
        proof.subst[ViewR[Q, F, A, ?]](ViewR.empty[Q, F, A])

      def cons[X](head: F[A, X], tail: Q[F, X, B]): Z = {
        val u2 = unsnoc(tail)
        u2.cata(new u2.Cata[Z] {
          def empty(proof: B === X): Z =
            ViewR.cons[Q, F, A, A, B](self.empty, proof.flip.subst[F[A, ?]](head))

          def cons[Y](init: Q[F, X, Y], last: F[Y, B]): Z =
            ViewR.cons[Q, F, A, Y, B](prepend[F, A, X, Y](head, init), last)
        })
      }
    })
  }

  /**
    * Apply a function to all elements in a type aligned sequence.
    */
  def biffmap[F[_, _], G[_, _], A, B](fab: Q[F, A, B])(fg: F ~~> G): Q[G, A, B] = {
    val u1 = uncons(fab)
    u1.cata(new u1.Cata[Q[G, A, B]] {
      def empty(proof: A === B): Q[G, A, B] =
        proof.subst[Q[G, A, ?]](self.empty[G, A])

      def cons[X](head: F[A, X], tail: Q[F, X, B]): Q[G, A, B] =
        prepend[G, A, X, B](fg(head), biffmap(tail)(fg))
    })
  }
}

trait TAFoldRight[F[_, _], G[_, _]] {
  def empty[A]: G[A, A]
  def more[A, B, C](next: F[A, B], acc: G[B, C]): G[A, C]
}

trait TAFoldLeft[F[_, _], G[_, _]] {
  def empty[A]: G[A, A]
  def more[A, B, C](acc: G[A, B], next: F[B, C]): G[A, C]
}

sealed abstract class ViewR[Q[_[_, _], _, _], F[_, _], A, B] {
  type Cata[R] = ViewR.Cata[Q, F, A, B, R]
  def cata[R](fold: Cata[R]): R
}
object ViewR {
  trait Cata[Q[_[_, _], _, _], F[_, _], A, B, R] {
    def empty(proof: B === A): R
    def cons[X](init: Q[F, A, X], last: F[X, B]): R
  }

  final case class Empty[Q[_[_, _], _, _], F[_, _], A]() extends ViewR[Q, F, A, A] {
    def cata[R](fold: Cata[R]): R = fold.empty(Leibniz.refl[A])
  }
  final case class Cons[Q[_[_, _], _, _], F[_, _], A, B, C](init: Q[F, A, B], last: F[B, C]) extends ViewR[Q, F, A, C] {
    def cata[R](fold: Cata[R]): R = fold.cons[B](init, last)
  }

  def empty[Q[_[_, _], _, _], F[_, _], A]: ViewR[Q, F, A, A] =
    Empty()
  def cons[Q[_[_, _], _, _], F[_, _], A, B, C](init: Q[F, A, B], last: F[B, C]): ViewR[Q, F, A, C] =
    Cons[Q, F, A, B, C](init, last)
}

/**
  * A view of the left (cons) side of type-aligned sequences
  * of type `Q`. May be either empty, in which case we obtain
  * evidence that `A === B`, or a `Cons` which reveals
  * an (existential) intermediate type between `A` and `B`.
  */
sealed abstract class ViewL[Q[_[_, _], _, _], F[_, _], A, B] {
  type Cata[R] = ViewL.Cata[Q, F, A, B, R]
  def cata[R](fold: Cata[R]): R
}
object ViewL {
  trait Cata[Q[_[_, _], _, _], F[_, _], A, B, R] {
    def empty(proof: A === B): R
    def cons[X](head: F[A, X], tail: Q[F, X, B]): R
  }

  final case class Empty[Q[_[_, _], _, _], F[_, _], A]() extends ViewL[Q, F, A, A] {
    def cata[R](fold: Cata[R]): R = fold.empty(Leibniz.refl[A])
  }
  final case class Cons[Q[_[_, _], _, _], F[_, _], A, B, C](head: F[A, B], tail: Q[F, B, C]) extends ViewL[Q, F, A, C] {
    def cata[R](fold: Cata[R]): R = fold.cons[B](head, tail)
  }

  def empty[Q[_[_, _], _, _], F[_, _], A]: ViewL[Q, F, A, A] =
    Empty()
  def cons[Q[_[_, _], _, _], F[_, _], A, B, C](head: F[A, B], tail: Q[F, B, C]): ViewL[Q, F, A, C] =
    Cons[Q, F, A, B, C](head, tail)
}