package taseq

import leibniz.{Leibniz, ===}
import taseq.{ SnocList => L }
import L.{ snoc, singleton }

sealed abstract class SnocList[F[_, _], A, B] { ab =>
  type Cata[Z] = L.Cata[F, A, B, Z]
  def cata[Z](fold: Cata[Z]): Z

  def :+[C](bc: F[B, C]): L[F, A, C] = snoc[F, A, B, C](this, bc)
  def +:[Z](za: F[Z, A]): L[F, Z, B] = singleton(za) ++ ab

  def :::[Z](fbc: L[F, Z, A]): L[F, Z, B]
  def ++[C](fbc: L[F, B, C]): L[F, A, C]

  def uncons: ViewL[L, F, A, B]
  def unsnoc: ViewR[L, F, A, B]

  def biffmap[G[_, _]](fg: F ~~> G): L[G, A, B]

  def foldLeft[G[_, _]](z: TAFoldLeft[F, G]): G[A, B]
}
object SnocList {
  trait Cata[F[_, _], A, B, R] {
    def lin(proof: A === B): R
    def snoc[X](init: L[F, A, X], last: F[X, B]): R
  }

  final case class Lin[F[_, _], A]() extends L[F, A, A] {
    def cata[Z](fold: Cata[Z]): Z = fold.lin(Leibniz.refl)

    def :::[Z](fbc: L[F, Z, A]): L[F, Z, A] = fbc
    def ++[C](fbc: L[F, A, C]): L[F, A, C] = fbc

    def uncons: ViewL[L, F, A, A] = ViewL.empty[L, F, A]
    def unsnoc: ViewR[L, F, A, A] = ViewR.empty[L, F, A]

    def biffmap[G[_, _]](fg: ~~>[F, G]): L[G, A, A] = empty[G, A]

    def foldLeft[G[_, _]](z: TAFoldLeft[F, G]): G[A, A] = z.empty[A]
  }
  final case class Snoc[F[_, _], A, B, C](init: L[F, A, B], last: F[B, C]) extends L[F, A, C] {
    def cata[Z](fold: Cata[Z]): Z = fold.snoc[B](init, last)

    def :::[Z](fbc: L[F, Z, A]): L[F, Z, C] = (fbc ::: init) :+ last
    def ++[D](fbc: L[F, C, D]): L[F, A, D] = this ::: fbc

    def uncons: ViewL[L, F, A, C] =
      init.uncons.cata(new ViewL.Cata[L, F, A, B, ViewL[L, F, A, C]] {
        def empty(proof: A === B): ViewL[L, F, A, C] =
          ViewL.cons[L, F, A, C, C](proof.flip.subst[F[?, C]](last), L.empty[F, C])
        def cons[X](head: F[A, X], tail: L[F, X, B]): ViewL[L, F, A, C] =
          ViewL.cons[L, F, A, X, C](head, tail :+ last)
      })
    def unsnoc: ViewR[L, F, A, C] =
      ViewR.cons[L, F, A, B, C](init, last)

    def biffmap[G[_, _]](fg: ~~>[F, G]): L[G, A, C] =
       init.biffmap(fg) :+ fg.apply(last)

    def foldLeft[G[_, _]](z: TAFoldLeft[F, G]): G[A, C] =
      z.more(init.foldLeft(z), last)
  }

  def empty[F[_, _], A]: L[F, A, A] = Lin[F, A]()
  def snoc[F[_, _], A, B, C](init: L[F, A, B], last: F[B, C]): L[F, A, C] = Snoc[F, A, B, C](init, last)
  def singleton[F[_, _], A, B](ab: F[A, B]): L[F, A, B] = snoc[F, A, A, B](empty, ab)

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