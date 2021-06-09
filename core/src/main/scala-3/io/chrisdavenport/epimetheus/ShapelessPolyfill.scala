package io.chrisdavenport.epimetheus

import scala.compiletime.ops.int.*
import scala.quoted.*

trait ShapelessPolyfill {

  type Represented[R] = R match {
    case IndexedSeq[a] => a
  }

  type TupleSized[R, A, N <: Int] <: Tuple = N match {
    case 0 => EmptyTuple
    case S[n] => A *: TupleSized[R, A, n]
  }

  extension [R, A, N <: Int] (s: TupleSized[R, A, N]) {
    def unsized: IndexedSeq[A] = s.productIterator.toIndexedSeq.asInstanceOf[IndexedSeq[A]]
  }

  type Nat = Int

  type Sized[Repr, L <: Nat] = TupleSized[Repr, Represented[Repr], L]

  object Sized {
    def apply[A](a1: A): Sized[IndexedSeq[A], 1] = Tuple1(a1)
    def apply[A](a1: A, a2: A): Sized[IndexedSeq[A], 2] = (a1, a2)
    def apply[A](a1: A, a2: A, a3: A): Sized[IndexedSeq[A], 3] = (a1, a2, a3)
    def apply[A](a1: A, a2: A, a3: A, a4: A): Sized[IndexedSeq[A], 4] = (a1, a2, a3, a4)
    def apply[A](a1: A, a2: A, a3: A, a4: A, a5: A): Sized[IndexedSeq[A], 5] = (a1, a2, a3, a4, a5)
    def apply[A](a1: A, a2: A, a3: A, a4: A, a5: A, a6: A): Sized[IndexedSeq[A], 6] = (a1, a2, a3, a4, a5, a6)
    def apply[A](a1: A, a2: A, a3: A, a4: A, a5: A, a6: A, a7: A): Sized[IndexedSeq[A], 7] = (a1, a2, a3, a4, a5, a6, a7)
    def apply[A](a1: A, a2: A, a3: A, a4: A, a5: A, a6: A, a7: A, a8: A): Sized[IndexedSeq[A], 8] = (a1, a2, a3, a4, a5, a6, a7, a8)
    def apply[A](a1: A, a2: A, a3: A, a4: A, a5: A, a6: A, a7: A, a8: A, a9: A): Sized[IndexedSeq[A], 9] = (a1, a2, a3, a4, a5, a6, a7, a8, a9)
    def apply[A](a1: A, a2: A, a3: A, a4: A, a5: A, a6: A, a7: A, a8: A, a9: A, a10: A): Sized[IndexedSeq[A], 10] = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
  }

}
