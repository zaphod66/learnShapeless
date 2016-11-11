package com.zaphod

package object shapely {
  type ::[H, T <: HList] = HCons[H, T]

  type HNil = HNil0.type
  val  HNil = HNil0

  implicit class HListSyntax[L <: HList](self: L) {
    def ::[H](head: H): H :: L = HCons(head, self)

    def remove[A](implicit R: Remover[A, L]): R.Out = R(self)

    def map[P <: Poly](p: P)(implicit M: Mapper[L, P]): M.Out = M(self)
  }
}
