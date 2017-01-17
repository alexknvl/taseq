package taseq

//import TASteque._
import cats.~>
import leibniz.{Leibniz, ===}

import scala.annotation.tailrec

//sealed abstract class TASteque[F[_, _], A, B] {
//  def fold[Z](
//    empty: A === B => Z,
//    single: F[A, B] => Z,
//    append: FoldAppend[F, A, B, Z]): Z
//  def asEmpty: Option[A === B]
//}
//object TASteque {
//  private[this] final case class Empty[F[_, _], A]
//  () extends TASteque[F, A, A] {
//    def fold[Z](
//      empty: A === A => Z,
//      single: F[A, A] => Z,
//      append: FoldAppend[F, A, A, Z]
//    ): Z = empty(Leibniz.refl)
//
//    def asEmpty: Option[A === A] = Some(Leibniz.refl)
//  }
//  private[this] final case class Single[F[_, _], A, B]
//  (value: F[A, B]) extends TASteque[F, A, B] {
//    def fold[Z](
//      empty: A === B => Z,
//      single: F[A, B] => Z,
//      append: FoldAppend[F, A, B, Z]
//    ): Z = single(value)
//
//    def asEmpty: Option[A === B] = None
//  }
//  private[this] final case class Append[F[_, _], A, B, C]
//  (left: TASteque[F, A, B], right: TASteque[F, B, C]) extends TASteque[F, A, C] {
//    def fold[Z](
//      empty: A === C => Z,
//      single: F[A, C] => Z,
//      append: FoldAppend[F, A, C, Z]
//    ): Z = append.apply[B]((left, right))
//
//    def asEmpty: Option[A === C] = None
//  }
//
//  private type Pair[F[_, _], A, B, X] =
//    (TASteque[F, A, X], TASteque[F, X, B])
//  private type FoldAppend[F[_, _], A, B, Z] =
//    Pair[F, A, B, ?] ~> λ[X => Z]
//
//  implicit val instance: TASequence[TASteque] = new TASequence[TASteque] {
//    def empty[F[_, _], A]: TASteque[F, A, A] =
//      new Empty[F, A]()
//    def singleton[F[_, _], A, B](ab: F[A, B]): TASteque[F, A, B] =
//      new Single[F, A, B](ab)
//
//    def concat[F[_, _], A, B, C](ab: TASteque[F, A, B], bc: TASteque[F, B, C]): TASteque[F, A, C] =
//      ab.asEmpty match {
//        case Some(p) => p.flip.subst[TASteque[F, ?, C]](bc)
//        case _ => bc.asEmpty match {
//          case Some(p) => p.subst[TASteque[F, A, ?]](ab)
//          case _ => Append(ab, bc)
//        }
//      }
//    def append[F[_, _], A, B, C](ab: TASteque[F, A, B], bc: F[B, C]): TASteque[F, A, C] =
//      concat(ab, singleton(bc))
//    def prepend[F[_, _], A, B, C](ab: F[A, B], bc: TASteque[F, B, C]): TASteque[F, A, C] =
//      concat(singleton(ab), bc)
//
//    def unsnoc[F[_, _], A, B](ab: TASteque[F, A, B]): ViewR[TASteque, F, A, B] = {
//      @tailrec def go(c: TASteque[A], rights: List[TASteque[A]]): ViewR[TASteque, F, A, B] = c match {
//        case Empty() => rights match {
//          case Nil => None
//          case c :: rights => go(c, rights)
//        }
//        case Single(a) => Some(a -> (if (rights.isEmpty) empty else rights.reduceRight(Append(_, _))))
//        case Append(l,r) => go(l, r :: rights)
//      }
//      go(this, List())
//    }
//
//    def uncons[F[_, _], A, B](ab: TASteque[F, A, B]): ViewL[TASteque, F, A, B] = ???
//
//    def mapK[F[_, _], G[_, _], A, B](fab: TASteque[F, A, B])(fg: F ~~> G): TASteque[G, A, B] = ???
//  }
//}