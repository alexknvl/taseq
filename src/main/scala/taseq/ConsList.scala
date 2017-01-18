package taseq

import leibniz.{Leibniz, ===}
import taseq.{ ConsList => L }
import L.{ cons, singleton }

sealed abstract class ConsList[F[_, _], A, B] { ab =>
  type Cata[Z] = L.Cata[F, A, B, Z]
  def cata[Z](fold: Cata[Z]): Z

  def ++[C](fbc: L[F, B, C]): L[F, A, C]
  def :+[C](bc: F[B, C]): L[F, A, C] = ab ++ singleton(bc)
  def +:[Z](za: F[Z, A]): L[F, Z, B] = cons[F, Z, A, B](za, this)
  def uncons: ViewL[L, F, A, B]
  def unsnoc: ViewR[L, F, A, B]
  def biffmap[G[_, _]](fg: F ~~> G): L[G, A, B]

  def foldRight[G[_, _]](z: TAFoldRight[F, G]): G[A, B]
}
object ConsList {
  trait Cata[F[_, _], A, B, R] {
    def nil(proof: A === B): R
    def cons[X](head: F[A, X], tail: L[F, X, B]): R
  }

  final case class Nil[F[_, _], A]() extends L[F, A, A] {
    def cata[Z](fold: Cata[Z]): Z = fold.nil(Leibniz.refl)
    def ++[C](fbc: L[F, A, C]): L[F, A, C] = fbc
    def uncons: ViewL[L, F, A, A] = ViewL.empty[L, F, A]
    def unsnoc: ViewR[L, F, A, A] = ViewR.empty[L, F, A]
    def biffmap[G[_, _]](fg: ~~>[F, G]): L[G, A, A] = empty[G, A]

    def foldRight[G[_, _]](z: TAFoldRight[F, G]): G[A, A] = z.empty[A]
  }
  final case class Cons[F[_, _], A, B, C](head: F[A, B], tail: L[F, B, C]) extends L[F, A, C] {
    def cata[Z](fold: Cata[Z]): Z = fold.cons[B](head, tail)
    def ++[D](fbc: L[F, C, D]): L[F, A, D] = head +: (tail ++ fbc)
    def uncons: ViewL[L, F, A, C] =
      ViewL.cons[L, F, A, B, C](head, tail)
    def unsnoc: ViewR[L, F, A, C] =
      tail.unsnoc.cata(new ViewR.Cata[L, F, B, C, ViewR[L, F, A, C]] {
        def empty(proof: C === B): ViewR[L, F, A, C] =
          ViewR.cons[L, F, A, A, C](L.empty[F, A], proof.flip.subst[F[A, ?]](head))

         def cons[X](init: L[F, B, X], last: F[X, C]): ViewR[L, F, A, C] =
           ViewR.cons[L, F, A, X, C](head +: init, last)
      })
    def biffmap[G[_, _]](fg: ~~>[F, G]): L[G, A, C] =
      fg.apply(head) +: tail.biffmap(fg)

    def foldRight[G[_, _]](z: TAFoldRight[F, G]): G[A, C] =
      z.more(head, tail.foldRight(z))
  }

  def empty[F[_, _], A]: L[F, A, A] = Nil[F, A]()
  def cons[F[_, _], A, B, C](head: F[A, B], tail: L[F, B, C]): L[F, A, C] = Cons[F, A, B, C](head, tail)
  def singleton[F[_, _], A, B](ab: F[A, B]): L[F, A, B] = cons[F, A, B, B](ab, empty)

  implicit val instance: TASequence[L] = new TASequence[L] {
    override def empty[F[_, _], A]: L[F, A, A] = L.empty[F, A]
    override def singleton[F[_, _], A, B](ab: F[A, B]): L[F, A, B] = L.singleton[F, A, B](ab)
    override def concat[F[_, _], A, B, C](fab: L[F, A, B], fbc: L[F, B, C]): L[F, A, C] = fab ++ fbc
    override def append[F[_, _], A, B, C](ab: L[F, A, B], bc: F[B, C]): L[F, A, C] = ab :+ bc
    override def prepend[F[_, _], A, B, C](ab: F[A, B], bc: L[F, B, C]): L[F, A, C] = ab +: bc
    override def uncons[F[_, _], A, B](ab: L[F, A, B]): ViewL[L, F, A, B] = ab.uncons
    override def unsnoc[F[_, _], A, B](fab: L[F, A, B]): ViewR[L, F, A, B] = fab.unsnoc
    override def biffmap[F[_, _], G[_, _], A, B](ab: L[F, A, B])(fg: F ~~> G): L[G, A, B] = ab.biffmap(fg)
  }
}