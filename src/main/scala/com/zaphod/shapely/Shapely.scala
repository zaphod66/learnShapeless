package com.zaphod.shapely

// Daniel Spiewak's talk 'Roll Your Own Shapeless'
// see https://www.youtube.com/watch?v=GKIfu1WtSz4

import scala.language.higherKinds

sealed trait HList {
  type Append[L <: HList] <: HList

  def ++[L <: HList](xs: L): Append[L]
}

final case class HCons[H, T <: HList](head: H, tail: T) extends HList {
  type Append[L <: HList] = HCons[H, T#Append[L]]

  def ++[L <: HList](xs: L) = HCons(head, tail ++ xs)
}

case object HNil0 extends HList {
  type Append[L <: HList] = L

  def ++[L <: HList](xs: L) = xs
}

// Removing

trait Remover[A, L <: HList] {
  type Out <: HList

  def apply(xs: L): Out
}

private [shapely] trait RemoverLowPriorityImplicits {
  implicit def base[A, L <: HList]: Remover.Aux[A, A :: L, L] = new Remover[A, A :: L] {
    type Out = L

    def apply(xs: A :: L) = xs.tail
  }
}

object Remover extends RemoverLowPriorityImplicits {
  type Aux[A, L <: HList, Out0 <: HList] = Remover[A, L] { type Out = Out0 }

  implicit def corecurseRemove[A, L <: HList](implicit R: Remover[A, L]): Remover.Aux[A, A :: L, R.Out] =
    new Remover[A, A :: L] {
      type Out = R.Out

      def apply(xs: A :: L) = R(xs.tail)
    }

  implicit def corecurseRebuild[A, B, L <: HList](implicit R: Remover[A, L]): Remover.Aux[A, B :: L, B :: R.Out] =
    new Remover[A, B :: L] {
      type Out = B :: R.Out

      def apply(xs: B :: L) = xs.head :: R(xs.tail)
    }
}

// Mapping

trait Poly {
  final def at[A] = new Caser[A]

  def apply[A, B](a: A)(implicit C: this.Case[A, B]): B = C(a)

  final class Caser[A] {
    def apply[B](f: A => B): Case[A, B] = new Case[A, B] {
      def apply(a: A) = f(a)
    }
  }

  sealed trait Case[A, B] {
    def apply(a: A): B
  }
}

trait Mapper[L <: HList, P <: Poly] {
  type Out <: HList

  def apply(xs: L): Out
}

object Mapper {
  type Aux[L <: HList, P <: Poly, Out0 <: HList] = Mapper[L, P] { type Out = Out0 }

  implicit def base[P <: Poly]: Mapper.Aux[HNil, P, HNil] = new Mapper[HNil, P] {
    type Out = HNil

    def apply(xs: HNil) = xs
  }

  implicit def corecurse[A, B, L <: HList, P <: Poly](implicit C: P#Case[A, B], M: Mapper[L, P]): Mapper.Aux[A :: L, P, B :: M.Out] = new Mapper[A :: L, P] {
    type Out = B :: M.Out

    def apply(xs: A :: L) = C(xs.head) :: M(xs.tail)
  }
}

// Nth


object Shapely extends App {

  val hl0 = 0 :: 3.14 :: "HList" :: 1 :: HNil
  val hl1 = hl0.remove[String]
  val hl2 = hl0.remove[Double]

  println(s"hl0: $hl0")
  println(s"hl1: $hl1")
  println(s"hl2: $hl2")

  object square extends Poly {
    implicit val is = at[Int]    { i => i * i }
    implicit val fs = at[Float]  { f => f * f }
    implicit val ds = at[Double] { d => d * d }
  }

  trait AddOneLowPriorityImplicit extends Poly {
    implicit def default[A] = at[A] { _.toString + "1" }
  }

  object addOne extends AddOneLowPriorityImplicit {
    implicit val is         = at[Int] { _ + 1 }
  }

  val si = square(4)
  val sd = square(3.14d)

  val ai = addOne(1)
  val as = addOne("1")
  val ad = addOne(3.14d)

  println(s"ai: $ai")
  println(s"as: $as")
  println(s"ad: $ad")

  val xs = 1 :: false :: "Hey" :: HNil

  object transform extends Poly {
    implicit val is = at[Int]     { _ + 1 }
    implicit val bs = at[Boolean] { !_ }
    implicit val ss = at[String]  { _ + " #Yolo" }
  }

  val ys = xs map transform

  println(s"xs: $xs")
  println(s"ys: $ys")
}
