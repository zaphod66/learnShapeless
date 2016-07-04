package com.zaphod.shapely

// Daniel Spiewak's talk 'Roll Your Own Shapeless'
// see https://www.youtube.com/watch?v=GKIfu1WtSz4

trait HList {
  type Append[L <: HList] <: HList

  def ++[L <: HList](xs: L): Append[L]
}

object Shapely extends App {

}
