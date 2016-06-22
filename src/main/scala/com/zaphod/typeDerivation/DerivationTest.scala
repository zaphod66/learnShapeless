package com.zaphod.typeDerivation

object equalManual {
  trait Eq[T] {
    def eqv(x: T, y: T): Boolean
  }

  object Eq {
    implicit val eqInt: Eq[Int] = new Eq[Int] {
      def eqv(x: Int, y: Int): Boolean = x == y
    }
    implicit val eqStr: Eq[String] = new Eq[String] {
      def eqv(x: String, y: String): Boolean = x == y
    }
  }

  implicit class EqOps[T](x: T)(implicit eqT: Eq[T]) {
    def =?=(y: T): Boolean = eqT.eqv(x, y)
  }

  sealed trait Animal
  case class Dog(name: String, bones: Int) extends Animal
  case class Cat(name: String, fishes: Int) extends Animal

  object Animal {
    implicit val eqAnimal: Eq[Animal] = new Eq[Animal] {
      def eqv(x: Animal, y: Animal): Boolean =
        (x, y) match {
          case (x: Cat, y: Cat) => x =?= y
          case (x: Dog, y: Dog) => x =?= y
          case _ => false
        }
    }
  }

  object Cat {
    implicit val eqCat: Eq[Cat] = new Eq[Cat] {
      def eqv(x: Cat, y: Cat): Boolean = x.name =?= y.name && x.fishes =?= y.fishes
    }
  }

  object Dog {
    implicit val eqCat: Eq[Dog] = new Eq[Dog] {
      def eqv(x: Dog, y: Dog): Boolean = x.name =?= y.name && x.bones =?= y.bones
    }
  }

  val dog1 = Dog("dog1", 1)
  val dog2 = Dog("dog1", 1)
  val cat1 = Cat("cat1", 1)
  val cat2 = Cat("cat2", 1)
}


object DerivationTest extends App {
  object Manual {
    import equalManual._

    println("ImplicitTest")

    println(s"dog1 =?= dog2 = ${dog1 =?= dog2}")
    println(s"cat1 =?= cat2 = ${cat1 =?= cat2}")
  }

  Manual
}
