package taseq

import leibniz.{Exists, Leibniz, ===}
import taseq.{ TAList => L }
import L.{ cons, singleton }

sealed abstract class TAList[F[_, _], A, B] { ab =>
  type Fold[Z] = L.Fold[F, A, B, Z]
  def fold[Z](fold: Fold[Z]): Z

  def asNil: Option[A === B]
  def asCons: Option[Exists[L.Cons[F, A, ?, B]]]

  def ++[C](fbc: L[F, B, C]): L[F, A, C]
  def :+[C](bc: F[B, C]): L[F, A, C] = ab ++ singleton(bc)
  def +:[Z](za: F[Z, A]): L[F, Z, B] = cons[F, Z, A, B](za, this)
  def uncons: ViewL[L, F, A, B]
  def unsnoc: ViewR[L, F, A, B]
  def mapQ[G[_, _]](fg: F ~~> G): L[G, A, B]

  def foldRight[G[_, _]](z: TAFoldRight[F, G]): G[A, B]
}
object TAList {
  trait Fold[F[_, _], A, B, R] {
    def nil(proof: A === B): R
    def cons[X](head: F[A, X], tail: L[F, X, B]): R
  }
  object Nil {
    def unapply[F[_, _], A, B](q: L[F, A, B]): Option[A === B] =
      q.asNil
  }
  object Cons {
    def unapply[F[_, _], A, B](q: L[F, A, B]): Option[Exists[Cons[F, A, ?, B]]] =
      q.asCons
  }

  final case class Nil[F[_, _], A]() extends L[F, A, A] {
    def proof: A === A = Leibniz.refl

    def asNil: Option[A === A] = Some(Leibniz.refl)
    def asCons: Option[Exists[L.Cons[F, A, ?, A]]] = None

    def fold[Z](fold: Fold[Z]): Z = fold.nil(Leibniz.refl)
    def ++[C](fbc: L[F, A, C]): L[F, A, C] = fbc
    def uncons: ViewL[L, F, A, A] = ViewL.empty[L, F, A]
    def unsnoc: ViewR[L, F, A, A] = ViewR.empty[L, F, A]
    def mapQ[G[_, _]](fg: ~~>[F, G]): L[G, A, A] = empty[G, A]

    def foldRight[G[_, _]](z: TAFoldRight[F, G]): G[A, A] = z.empty[A]
  }
  final case class Cons[F[_, _], A, B, C](head: F[A, B], tail: L[F, B, C]) extends L[F, A, C] {
    type Middle = B

    def asNil: Option[A === C] =
      None
    def asCons: Option[Exists[L.Cons[F, A, ?, C]]] =
      Some(Exists[L.Cons[F, A, ?, C]](this))

    def fold[Z](fold: Fold[Z]): Z = fold.cons[B](head, tail)
    def ++[D](fbc: L[F, C, D]): L[F, A, D] = head +: (tail ++ fbc)
    def uncons: ViewL[L, F, A, C] =
      ViewL.cons[L, F, A, B, C](head, tail)
    def unsnoc: ViewR[L, F, A, C] =
      tail.unsnoc.fold(new ViewR.Fold[L, F, B, C, ViewR[L, F, A, C]] {
        def empty(proof: C === B): ViewR[L, F, A, C] =
          ViewR.cons[L, F, A, A, C](L.empty[F, A], proof.flip.subst[F[A, ?]](head))

         def cons[X](init: L[F, B, X], last: F[X, C]): ViewR[L, F, A, C] =
           ViewR.cons[L, F, A, X, C](head +: init, last)
      })
    def mapQ[G[_, _]](fg: ~~>[F, G]): L[G, A, C] =
      fg.apply(head) +: tail.mapQ(fg)

    def foldRight[G[_, _]](z: TAFoldRight[F, G]): G[A, C] =
      z.more(head, tail.foldRight(z))
  }

  def empty[F[_, _], A]: L[F, A, A] = Nil[F, A]()
  def cons[F[_, _], A, B, C](head: F[A, B], tail: L[F, B, C]): L[F, A, C] = Cons[F, A, B, C](head, tail)
  def singleton[F[_, _], A, B](ab: F[A, B]): L[F, A, B] = cons[F, A, B, B](ab, empty)

  implicit val instance: TASequence[L] = new TASequence[L] {
    def empty[F[_, _], A]: L[F, A, A] = L.empty[F, A]
    def singleton[F[_, _], A, B](ab: F[A, B]): L[F, A, B] = L.singleton[F, A, B](ab)
    def concat[F[_, _], A, B, C](fab: L[F, A, B], fbc: L[F, B, C]): L[F, A, C] = fab ++ fbc
    def append[F[_, _], A, B, C](ab: L[F, A, B], bc: F[B, C]): L[F, A, C] = ab :+ bc
    def prepend[F[_, _], A, B, C](ab: F[A, B], bc: L[F, B, C]): L[F, A, C] = ab +: bc
    def uncons[F[_, _], A, B](ab: L[F, A, B]): ViewL[L, F, A, B] = ab.uncons
    def unsnoc[F[_, _], A, B](fab: L[F, A, B]): ViewR[L, F, A, B] = fab.unsnoc
    def mapQ[F[_, _], G[_, _], A, B](ab: L[F, A, B])(fg: F ~~> G): L[G, A, B] = ab.mapQ(fg)
  }
}