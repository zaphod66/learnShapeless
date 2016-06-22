package com.zaphod.typeDerivation

// Scala Days 2015 San Francisco
// Miles Sabin:
//   The swiss army knife of generic programming:
//   shapeless's TypeClass type class in action
// https://www.youtube.com/watch?v=NL88I0DAS7g

// import shapeless.{Generic, HList, HNil, Lazy}
import shapeless._

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
  val ani1: Animal = new Dog("dog3", 2)
  val ani2: Animal = new Cat("cat3", 1)
}

object equalDerived {
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

    implicit def eqGeneric[T, R](implicit gen: Generic.Aux[T, R], eqRepr: Lazy[Eq[R]]): Eq[T] = new Eq[T] {
      def eqv(x: T, y: T): Boolean = eqRepr.value.eqv(gen.to(x), gen.to(y))
    }
  }

  // Induction over products

  // Base case for products
  implicit val eqHNil: Eq[HNil] = new Eq[HNil] {
    def eqv(x: HNil, y: HNil): Boolean = true
  }

  // Induction step for products
  implicit def eqHCons[H, T <: HList](implicit eqH: Lazy[Eq[H]], eqT: Lazy[Eq[T]]): Eq[H :: T] = new Eq[H :: T] {
    def eqv(x: H :: T, y: H :: T): Boolean = eqH.value.eqv(x.head, y.head) && eqT.value.eqv(x.tail, y.tail)
  }

  // Induction over sums (coproducts)

  // Base case for sums
  implicit val eqCNil: Eq[CNil] = new Eq[CNil] {
    def eqv(x: CNil, y: CNil): Boolean = true
  }

  // Induction step over sums
  implicit def eqCCons[H, T <: Coproduct](implicit eqH: Lazy[Eq[H]], eqT: Lazy[Eq[T]]): Eq[H :+: T] = new Eq[H :+: T] {
    def eqv(x: H :+: T, y: H :+: T): Boolean = (x, y) match {
      case (Inl(xh), Inl(yh)) => eqH.value.eqv(xh, yh)
      case (Inr(xt), Inr(yt)) => eqT.value.eqv(xt, yt)
      case _ => false
    }
  }

  implicit class EqOps[T](x: T)(implicit eqT: Eq[T]) {
    def =?=(y: T): Boolean = eqT.eqv(x, y)
  }

  sealed trait Animal
  case class Dog(name: String, bones: Int) extends Animal
  case class Cat(name: String, fishes: Int) extends Animal

  val dog1 = Dog("dog1", 1)
  val dog2 = Dog("dog1", 1)
  val cat1 = Cat("cat1", 1)
  val cat2 = Cat("cat2", 1)
  val ani1: Animal = new Dog("dog3", 2)
  val ani2: Animal = new Cat("cat3", 1)

  sealed trait Tree[T]
  case class Leaf[T](t: T) extends Tree[T]
  case class Doub[T](t1: T, t2: T) extends Tree[T]
  case class Node[T](l: Tree[T], r: Tree[T]) extends Tree[T]

  val tre1: Tree[Int] = Node(Node(Leaf(1), Node(Leaf(2), Leaf(3))), Leaf(4))
  val tre2: Tree[Int] = Node(Node(Leaf(1), Node(Leaf(2), Leaf(5))), Leaf(4))
  val tre3: Tree[Int] = Node(Node(Leaf(1), Node(Leaf(2), Leaf(3))), Leaf(4))
  val tre4: Tree[Int] = Node(Node(Leaf(1), Node(Leaf(2), Doub(3, 6))), Leaf(4))
}

object ADTs {
  // Sum: Leaf[T] :+: Node[T] :+: CNil
  sealed trait Tree[T]

  // Product: Leaf[T] :: HNil
  case class Leaf[T](t: T) extends Tree[T]
  // Product: Tree[T] :: Tree[T] :: HNil
  case class Node[T](l: Tree[T], r: Tree[T]) extends Tree[T]
}

object DerivationTest extends App {
  object Manual {
    import equalManual._

    println("ImplicitTest")

    println(s"dog1 =?= dog2 = ${dog1 =?= dog2}")
    println(s"cat1 =?= cat2 = ${cat1 =?= cat2}")
    //    println(s"cat1 =?= dog1 = ${cat1 =?= dog1}")    // will not typecheck
    println(s"ani1 =?= ani2 = ${ani1 =?= ani2}")
  }

  object Derived {
    import equalDerived._

    println("ImplicitTest")

    println(s"dog1 =?= dog2 = ${dog1 =?= dog2}")
    println(s"cat1 =?= cat2 = ${cat1 =?= cat2}")
    //    println(s"cat1 =?= dog1 = ${cat1 =?= dog1}")    // will not typecheck
    println(s"ani1 =?= ani2 = ${ani1 =?= ani2}")
    println(s"tre1 =?= tre2 = ${tre1 =?= tre2}")
    println(s"tre1 =?= tre3 = ${tre1 =?= tre3}")
    println(s"tre1 =?= tre3 = ${tre1 =?= tre4}")

    val genA = Generic[Animal]
    val genT = Generic[Tree[Int]]

    println(s"genA.to(dog1) = ${genA.to(dog1)}")
    println(s"genA.to(cat1) = ${genA.to(cat1)}")
    println(s"genA.to(ani1) = ${genA.to(ani1)}")
    println(s"genA.to(ani2) = ${genA.to(ani2)}")
    println(s"genT.to(tre1) = ${genT.to(tre1)}")
    println(s"genT.to(tre4) = ${genT.to(tre4)}")
  }

  Manual
  Derived
}
