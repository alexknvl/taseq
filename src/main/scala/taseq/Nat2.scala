package taseq

trait Nat2[F[_, _], G[_, _]] {
  def apply[A, B](f: F[A, B]): G[A, B]
}
