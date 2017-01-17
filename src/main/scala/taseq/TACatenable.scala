package taseq

import cats.Eval
import leibniz.{Leibniz, ===}

import taseq.{ TACatenable => Q, TAList => L }

sealed abstract class TACatenable[F[_, _], A, B] {
  type Fold[Z] = Q.Fold[F, A, B, Z]
  def fold[Z](fold: Fold[Z]): Z

  protected def concatL[Z](that: Q.NonEmpty[F, Z, A]): Q[F, Z, B]
  protected def concatR[C](that: Q.NonEmpty[F, B, C]): Q[F, A, C]

  def ++[C](fbc: Q[F, B, C]): Q[F, A, C]
  def :+[C](bc: F[B, C]): Q[F, A, C] = this ++ Q.singleton[F, B, C](bc)
  def +:[Z](za: F[Z, A]): Q[F, Z, B] = Q.singleton[F, Z, A](za) ++ this

  def uncons: ViewL[Q, F, A, B] = {
    def go[X](ax: Q[F, A, X], rights: L[Q[F, ?, ?], X, B]): Eval[ViewL[Q, F, A, B]] = {
      type Z = Eval[ViewL[Q, F, A, B]]

      ax.fold(new ax.Fold[Z] {
        def empty(pax: A === X): Eval[ViewL[Q, F, A, B]] =
          rights.fold(new rights.Fold[Z] {
            def nil(pxb: X === B): Eval[ViewL[Q, F, A, B]] =
              Eval.now((pax andThen pxb).flip.subst[ViewL[Q, F, ?, B]](
                ViewL.empty[Q, F, B]))
            def cons[Y](head: Q[F, X, Y], tail: L[Q[F, ?, ?], Y, B]): Z =
              Eval.defer(go[Y](pax.flip.subst[Q[F, ?, Y]](head), tail))
          })

        def append[Y](l: Q[F, A, Y], r: Q[F, Y, X]): Eval[ViewL[Q, F, A, B]] =
          Eval.defer(go(l, r +: rights))

        def single(fab: F[A, X]): Eval[ViewL[Q, F, A, B]] = {
          val rest = rights.foldRight(new TAFoldRight[Q[F, ?, ?], Q[F, ?, ?]] {
            def empty[Y]: Q[F, Y, Y] = Q.empty[F, Y]
            def more[I, J, K](next: Q[F, I, J], acc: Q[F, J, K]): Q[F, I, K] =
              Q.append(next, acc)
          })
          Eval.now(ViewL.cons[Q, F, A, X, B](fab, rest))
        }
      })
    }

    go[B](this, L.empty[Q[F, ?, ?], B]).value
  }

//  def unsnoc: ViewR[TAList, F, A, B]

  def mapQ[G[_, _]](fg: F ~~> G): Q[G, A, B]
}
object TACatenable {
  trait Fold[F[_, _], A, B, Z] {
    def empty(proof: A === B): Z
    def single(fab: F[A, B]): Z
    def append[X](left: Q[F, A, X], right: Q[F, X, B]): Z
  }

  final case class Empty[F[_, _], A]() extends Q[F, A, A] {
    def fold[Z](fold: Fold[Z]): Z = fold.empty(Leibniz.refl)

    protected def concatL[Z](that: Q.NonEmpty[F, Z, A]): Q[F, Z, A] = that
    protected def concatR[C](that: Q.NonEmpty[F, A, C]): Q[F, A, C] = that

    def ++[C](fbc: Q[F, A, C]): Q[F, A, C] = fbc
    def mapQ[G[_, _]](fg: ~~>[F, G]): Q[G, A, A] = empty[G, A]
  }
  sealed abstract class NonEmpty[F[_, _], A, B] extends Q[F, A, B]

  final case class Single[F[_, _], A, B](value: F[A, B]) extends NonEmpty[F, A, B] {
    def fold[Z](fold: Fold[Z]): Z = fold.single(value)

    protected def concatL[Z](that: Q.NonEmpty[F, Z, A]): Q[F, Z, B] =
      Q.append(that, this)
    protected def concatR[C](that: Q.NonEmpty[F, B, C]): Q[F, A, C] =
      Q.append(this, that)

    def ++[C](fbc: Q[F, B, C]): Q[F, A, C] = fbc match {
      case Empty() => fbc.concatL(this)
      case s@_ => Q.append(this, fbc)
    }
    def mapQ[G[_, _]](fg: ~~>[F, G]): Q[G, A, B] = singleton[G, A, B](fg.apply(value))
  }
  final case class Append[F[_, _], A, B, C](left: Q[F, A, B], right: Q[F, B, C]) extends NonEmpty[F, A, C] {
    def fold[Z](fold: Fold[Z]): Z = fold.append[B](left, right)

    protected def concatL[Z](that: Q.NonEmpty[F, Z, A]): Q[F, Z, C] =
      Q.append(that, this)
    protected def concatR[D](that: Q.NonEmpty[F, C, D]): Q[F, A, D] =
      Q.append(this, that)

    def ++[D](fbc: Q[F, C, D]): Q[F, A, D] = fbc match {
      case Empty() => fbc.concatL(this)
      case s@_ => Q.append(this, fbc)
    }
    def mapQ[G[_, _]](fg: ~~>[F, G]): Q[G, A, C] =
      append[G, A, B, C](left.mapQ(fg), right.mapQ(fg))
  }

  def empty[F[_, _], A]: Q[F, A, A] = new Empty[F, A]()
  def singleton[F[_, _], A, B](ab: F[A, B]): Q[F, A, B] = new Single[F, A, B](ab)
  private[TACatenable]
  def append[F[_, _], A, B, C](ab: Q[F, A, B], bc: Q[F, B, C]): Q[F, A, C] =
    new Append[F, A, B, C](ab, bc)

  implicit val instance: TASequence[Q] = new TASequence[Q] {
    def empty[F[_, _], A]: Q[F, A, A] = Q.empty[F, A]
    def singleton[F[_, _], A, B](ab: F[A, B]): Q[F, A, B] = Q.singleton[F, A, B](ab)
    def concat[F[_, _], A, B, C](ab: Q[F, A, B], bc: Q[F, B, C]): Q[F, A, C] = ab ++ bc
    def append[F[_, _], A, B, C](ab: Q[F, A, B], bc: F[B, C]): Q[F, A, C] = ab :+ bc
    def prepend[F[_, _], A, B, C](ab: F[A, B], bc: Q[F, B, C]): Q[F, A, C] = ab +: bc
    def unsnoc[F[_, _], A, B](ab: Q[F, A, B]): ViewR[Q, F, A, B] = ???
    def uncons[F[_, _], A, B](ab: Q[F, A, B]): ViewL[Q, F, A, B] = ab.uncons
    def mapQ[F[_, _], G[_, _], A, B](fab: Q[F, A, B])(fg: F ~~> G): Q[G, A, B] = fab.mapQ(fg)
  }
}