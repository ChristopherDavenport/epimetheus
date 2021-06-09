package io.chrisdavenport.epimetheus

trait ShapelessPolyfill {

  type Nat = shapeless.Nat

  type Sized[+Repr, L <: Nat] = shapeless.Sized[Repr, L]

  // For tests, user code on Scala 2 should just import shapeless.Sized
  private[epimetheus] val Sized = shapeless.Sized

}
