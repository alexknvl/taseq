package taseq

import leibniz.{Leibniz, ===}

sealed abstract class TAList[F[_, _], A, B] {
  trait FoldCons[R] {
    def apply[X](head: F[A, X], tail: TAList[F, X, B]): R
  }
  object FoldCons {
    def const[R](value: R): FoldCons[R] = new FoldCons[R] {
      def apply[X](head: F[A, X], tail: TAList[F, X, B]): R = value
    }
  }

  def fold[R](
    empty: A === B => R,
    cons: FoldCons[R]
  ): R

  def fold[Z](empty: A === B => Z, cons: FoldCons[Z]): Z
}
object TAList {
  private[this] final case class Nil[F[_, _], A]
  () extends TAList[F, A, A] {
    def fold[Z](empty: A === A => Z, cons: FoldCons[Z]): Z =
      empty(Leibniz.refl)
    def asEmpty: Option[A === A] =
      Some(Leibniz.refl)
  }
  private[this] final case class Cons[F[_, _], A, B, C]
  (ab: F[A, B], bc: TAList[F, B, C]) extends TAList[F, A, C] {
    def fold[Z](empty: A === C => Z, cons: FoldCons[Z]): Z =
      cons.apply[B](ab, bc)
    def asEmpty: Option[A === C] =
      Option.empty
  }

  implicit val instance: TASequence[TAList] = new TASequence[TAList] {
    def empty[F[_, _], A]: TAList[F, A, A] =
      Nil[F, A]()
    def singleton[F[_, _], A, B](ab: F[A, B]): TAList[F, A, B] =
      Cons[F, A, B, B](ab, empty)

    def concat[F[_, _], A, B, C](fab: TAList[F, A, B], fbc: TAList[F, B, C]): TAList[F, A, C] = fab.fold(
      empty = ab => ab.flip.subst[TAList[F, ?, C]](fbc),
      cons = new fab.FoldCons[TAList[F, A, C]] {
        def apply[X](head: F[A, X], tail: TAList[F, X, B]): TAList[F, A, C] =
          prepend[F, A, X, C](head, concat(tail, fbc))
      }
    )
    def append[F[_, _], A, B, C](ab: TAList[F, A, B], bc: F[B, C]): TAList[F, A, C] =
      concat(ab, singleton(bc))
    def prepend[F[_, _], A, B, C](ab: F[A, B], bc: TAList[F, B, C]): TAList[F, A, C] =
      Cons[F, A, B, C](ab, bc)

    def uncons[F[_, _], A, B](ab: TAList[F, A, B]): ViewL[TAList, F, A, B] = ab.fold(
      empty = p => p.subst[ViewL[TAList, F, A, ?]](ViewL.empty[TAList, F, A]),
      cons = new ab.FoldCons[ViewL[TAList, F, A, B]] {
        def apply[X](head: F[A, X], tail: TAList[F, X, B]): ViewL[TAList, F, A, B] =
          ViewL.cons[TAList, F, A, X, B](head, tail)
      }
    )

    def unsnoc[F[_, _], A, B](fab: TAList[F, A, B]): ViewR[TAList, F, A, B] = fab.fold(
      empty = ab => ab.subst[ViewR[TAList, F, A, ?]](ViewR.empty),
      cons = new fab.FoldCons[ViewR[TAList, F, A, B]] {
        def apply[X](head: F[A, X], tail: TAList[F, X, B]): ViewR[TAList, F, A, B] = {
          val v = unsnoc[F, X, B](tail)
          v.fold(
            empty = bx =>
              ViewR.cons[TAList, F, A, A, B](
                empty[F, A], bx.flip.subst[F[A, ?]](head)),
            cons =
              new v.FoldCons[ViewR[TAList, F, A, B]] {
                def apply[Y](init: TAList[F, X, Y], last: F[Y, B]): ViewR[TAList, F, A, B] =
                  ViewR.cons[TAList, F, A, Y, B](
                    prepend[F, A, X, Y](head, init), last)
              }
          )
        }
      }
    )

    def mapQ[F[_, _], G[_, _], A, B](ab: TAList[F, A, B])(fg: F ~~> G): TAList[G, A, B] = ab.fold(
      empty = p => p.subst[TAList[G, A, ?]](empty[G, A]),
      cons = new ab.FoldCons[TAList[G, A, B]] {
        def apply[X](head: F[A, X], tail: TAList[F, X, B]): TAList[G, A, B] =
          prepend[G, A, X, B](fg.apply(head), mapQ(tail)(fg))
      }
    )
  }
}