package typelevel101

import scala.language.higherKinds

sealed trait Nat {
  type Plus[That <: Nat] <: Nat
}

object Nat {
  type +[A <: Nat, B <: Nat] = A#Plus[B]
}

sealed trait Nat0 extends Nat {
  override type Plus[That <: Nat] = That
}

sealed trait NatN[Prev <: Nat] extends Nat {
  override type Plus[That <: Nat] = NatN[Prev#Plus[That]]
}

object NaturalNumbersTest {
  import typelevel101.Nat.+

  type Nat1 = NatN[Nat0]
  type Nat2 = NatN[Nat1]
  type Nat3 = NatN[Nat2]
  type Nat4 = NatN[Nat3]
  type Nat5 = NatN[Nat4]
  type Nat6 = NatN[Nat5]
  type Nat7 = NatN[Nat6]
  type Nat8 = NatN[Nat7]
  type Nat9 = NatN[Nat8]

  implicitly[Nat0 =:= Nat0]
  implicitly[Nat0 + Nat1 =:= Nat1]
  implicitly[Nat1 + Nat1 =:= Nat2]
  implicitly[Nat2 + Nat1 =:= Nat3]

  import shapeless.test.illTyped

  illTyped("implicitly[Nat0 =:= Nat1]")
}
