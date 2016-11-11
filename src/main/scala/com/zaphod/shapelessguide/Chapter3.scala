package com.zaphod.shapelessguide

// Turn a value of type A into a row of cells in a CSV file:
trait CsvEncoder[A] {
  def encode(value: A): List[String]
}

object CsvEncoder {
  // "Summoner" method
  def apply[A](implicit enc: CsvEncoder[A]): CsvEncoder[A] = enc

  // "Constructor" method
  def instance[A](func: A => List[String]): CsvEncoder[A] = {
    new CsvEncoder[A] {
      def encode(value: A): List[String] = func(value)
    }
  }
}

// Product data types
case class Employee(name: String, number: Int, isManager: Boolean)
case class IceCream(name: String, numCherries: Int, inCone: Boolean)

// Coproduct data types
sealed trait Shape
final case class Rectangle(width: Double, height: Double) extends Shape
final case class Circle(radius: Double) extends Shape

object Chapter3 extends App {

  val employees = List(
    Employee("Bill", 1, isManager = true),
    Employee("John", 2, isManager = false),
    Employee("Mart", 3, isManager = false)
  )

  val iceCreams = List(
    IceCream("Sundae", 1, inCone = false),
    IceCream("Cornetto", 0, inCone = true),
    IceCream("Banana", 0, inCone = false)
  )

  val shapes: List[Shape] = List(
    Rectangle(3.0, 4.0),
    Circle(2.0)
  )

  def writeCsv[A](values: List[A])(implicit enc: CsvEncoder[A]): String =
    values.map(value => enc.encode(value).mkString(",")).mkString("\n")

  object PlainStyle {
    implicit val employeeEncoder: CsvEncoder[Employee] = new CsvEncoder[Employee] {
      override def encode(e: Employee): List[String] = List(
        e.name,
        e.number.toString,
        if (e.isManager) "yes" else "no"
      )
    }

    implicit val iceCreamEncoder: CsvEncoder[IceCream] = new CsvEncoder[IceCream] {
      override def encode(i: IceCream): List[String] = List(
        i.name,
        i.numCherries.toString,
        if (i.inCone) "yes" else "no"
      )
    }

    implicit def pairEncoder[A, B](implicit aEncoder: CsvEncoder[A], bEncoder: CsvEncoder[B]): CsvEncoder[(A, B)] =
      new CsvEncoder[(A, B)] {
        override def encode(pair: (A, B)): List[String] = {
          val (a, b) = pair
          aEncoder.encode(a) ++ bEncoder.encode(b)
        }
      }

    import shapeless._

    val theIceCreamEncoder = the[CsvEncoder[IceCream]]

    println(writeCsv(employees))
    println(writeCsv(iceCreams))
    println(writeCsv(employees zip iceCreams))


    println(iceCreams flatMap theIceCreamEncoder.encode)
  }

  object Encoders {
    implicit val stringEncoder: CsvEncoder[String]   = CsvEncoder.instance(List(_))
    implicit val intEncoder: CsvEncoder[Int]         = CsvEncoder.instance(n => List(n.toString))
    implicit val booleanEncoder: CsvEncoder[Boolean] = CsvEncoder.instance(b => if (b) List("yes") else List("no"))

    import shapeless.{HList, ::, HNil}

    // product types
    implicit val hnilEncoder:  CsvEncoder[HNil] = CsvEncoder.instance(hnil => Nil)

    import shapeless.Lazy

    implicit def hlistEncoder[H, T <: HList](
                                              implicit
                                              hEncoder: Lazy[CsvEncoder[H]], // wrap in Lazy
                                              tEncoder: CsvEncoder[T]
                                            ): CsvEncoder[H :: T] = CsvEncoder.instance {
      case h :: t =>
        hEncoder.value.encode(h) ++ tEncoder.encode(t)
    }

    // coproduct types
    import shapeless.{Coproduct, :+:, CNil, Inl, Inr}

    implicit val cnilEncoder: CsvEncoder[CNil] = CsvEncoder.instance(cnil => Nil)

    implicit def coproductEncoder[H, T <: Coproduct](
      implicit
      hEncoder: Lazy[CsvEncoder[H]],
      tEncoder: CsvEncoder[T]
    ): CsvEncoder[H :+: T] = CsvEncoder.instance {
      case Inl(h) => hEncoder.value.encode(h)
      case Inr(t) => tEncoder.encode(t)
    }

    import shapeless.Generic

    implicit def genericEncoder[A, R](
      implicit
      gen: Generic.Aux[A, R],
      rEncoder: Lazy[CsvEncoder[R]]
    ): CsvEncoder[A] = CsvEncoder.instance(v => rEncoder.value.encode(gen.to(v)))

  }

  object HalfGenericStyle {
    import Encoders._
    import shapeless.{::, HNil}

    val reprEncoder: CsvEncoder[String :: Int :: Boolean :: HNil] = implicitly

    println(reprEncoder.encode("abc" :: 3 :: false :: HNil))

    import shapeless.Generic

    implicit val iceCreamEncoder: CsvEncoder[IceCream] = {
      val gen = Generic[IceCream]
      val enc = CsvEncoder[gen.Repr]

      CsvEncoder.instance(iceCream => enc.encode(gen.to(iceCream)))
    }

    println(writeCsv(iceCreams))
  }

  object FullGenericStyle {
    import Encoders._

    implicit val floatEncoder:  CsvEncoder[Float]  = CsvEncoder.instance(f => List(f.toString))
    implicit val doubleEncoder: CsvEncoder[Double] = CsvEncoder.instance(d => List(d.toString))

    case class Car(brand: String, model: String, doors: Int, kw: Float)

    val cars = List(
      Car("Alfa Romeo", "Guilietta", 4, 120.0f),
      Car("Porsche", "Targa 911", 2, 250.0f)
    )

    println(writeCsv(employees))
    println(writeCsv(iceCreams))
    println(writeCsv(cars))
    println(writeCsv(shapes))
  }

  object TreeGenericStyle {

    sealed trait Tree[A]
    case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
    case class Leaf[A](value: A) extends Tree[A]

    import Encoders._

    implicit def branchEncoder: CsvEncoder[Branch[Int]] = CsvEncoder.instance((br: Branch[Int]) => List(br.left.toString, br.right.toString))
    val leafEncoder = CsvEncoder[Leaf[Int]]
    val brEncoder   = CsvEncoder[Branch[Int]]

    import shapeless.{::, HNil, :+:, CNil}

    val poEnc = CsvEncoder[Int :: Int :: HNil]
    val coEnc = CsvEncoder[Int :+: String :+: CNil]
//  val treeEncoder = CsvEncoder[Tree[Int]]
  }

  println("=== PlainStyle")
  PlainStyle
  println("=== HalfGenericStyle")
  HalfGenericStyle
  println("=== FullGenericStyle")
  FullGenericStyle
}
