package typelevel101

import scala.language.higherKinds

import typelevel101.Nat.+

sealed trait Vector {
  def ::(head: Int): Vector = NEVector(head, this)
  def +(that: Vector): Vector

  def size: Int
}

case object VNil extends Vector {
  override def +(that: Vector) = {
    require(that == VNil)
    this
  }
  override val size = 0
}

case class NEVector(head: Int, tail: Vector) extends Vector {
  override def +(that: Vector) = {
    require(this.size == that.size)
    that match {
      case NEVector(h, t) => (head + h) :: (tail + t)
      case _ => this // compiler warning...
    }
  }
  override val size = 1 + tail.size
}

// types !!

sealed trait TVector[Size <: Nat] {
  def ::(head: Int): TVector[NatN[Size]] = TNEVector[Size](head, this)
  def +(that: TVector[Size]): TVector[Size]
  def ++[ThatSize <: Nat](that: TVector[ThatSize]): TVector[Size + ThatSize]
}

case object TVNil extends TVector[Nat0] {
  def +(that: TVector[Nat0]) = this
  def ++[ThatSize <: Nat](that: TVector[ThatSize]) = that
}

case class TNEVector[TailSize <: Nat](head: Int, tail: TVector[TailSize]) extends TVector[NatN[TailSize]] {
  type Size = NatN[TailSize]
  def +(that: TVector[Size]) = {
    that match {
      case TNEVector(h, t) => TNEVector(head + h, tail + t) // pattern match is now exhaustive
    }
  }
  def ++[ThatSize <: Nat](that: TVector[ThatSize]) = TNEVector(head, tail ++ that)  // arrghh
}

object VectorTest extends App {
  val sum1 = (1 :: 2 :: VNil) + (1 :: 1 :: VNil)

  println(sum1)

  val sum2 = (1 :: 2 :: TVNil) ++ (3 :: TVNil) + (1 :: 1 :: 1 :: TVNil)

  println(sum2)
}
