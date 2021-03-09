package co.s4n.inmutable.m6

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EjerciciosReviewSpec extends AnyFlatSpec with Matchers  {

  "esDivisibleTres(5) " should " false" in {
    EjerciciosReview.esDivisibleTres(5) shouldEqual false
  }

  "esPar(5) " should " false" in {
    EjerciciosReview.esPar(5) shouldEqual false
  }

  "esPar(2) " should " true" in {
    EjerciciosReview.esPar(2) shouldEqual true
  }

  "max3(2,5,3) " should " 5" in {
    EjerciciosReview.max3(2,5,3) shouldEqual 5
  }

  "factorial(5) " should " 120" in {
    EjerciciosReview.factorial(5) shouldEqual 120
  }

  "apply((_+_),5,2)" should " 7" in {
    EjerciciosReview.apply((_+_),5,2) shouldEqual 7
  }

  "filtro1((_>_), 12,10)" should " true" in {
    EjerciciosReview.filtro1((_>_), 12,10) shouldEqual true
  }

  "filtro2((_>_), 12,10)" should " 12" in {
    EjerciciosReview.filtro2((_>_), 12,10) shouldEqual 12
  }

  "temp(Autumn)" should " Cold" in {
    EjerciciosReview.temp(Autumn) shouldEqual Cold
  }

  "area(circ) radio 5 " should " 78.53982" in {
    val circ = new Circle(5)
    EjerciciosReview.area(circ) shouldEqual (78.53982).toFloat
  }

  "eval(Add(Lit(3),Lit(4)),Lit(5)) " should " 35" in {
    val expr = Mul(Add(Lit(3),Lit(4)),Lit(5))
    EjerciciosReview.eval(expr) shouldEqual 35
  }

  "size(Add(Lit(3),Lit(4)),Lit(5)) " should " 2" in {
    val expr = Mul(Add(Lit(3),Lit(4)),Lit(5))
    EjerciciosReview.size(expr) shouldEqual 2
  }

  "sum(List(1,2,3))" should " 6" in {
    EjerciciosReview.sum(List(1,2,3)) shouldEqual 6
  }

  "concat(List(\"z\",\"d\",\"e\"))" should " zde" in {
    EjerciciosReview.concat(List("z","d","e")) shouldEqual "zde"
  }

  "makeInt(\"5\").map(_+1)" should " Some(6)" in {
    EjerciciosReview.makeInt("5").map(_+1) shouldEqual Some(6)
  }

  "makeInt(\"Hola mundo!\").map(_+1)" should " None" in {
    EjerciciosReview.makeInt("Hola mundo!").map(_+1) shouldEqual None
  }

  "make using for" should " 3" in {
    val sum = for {
      a <- EjerciciosReview.makeInt("1")
      b <- EjerciciosReview.makeInt("2")
    } yield (a + b)

    sum.get shouldEqual 3
  }

  "makeInt(\"Cualquier cosa\") using for" should " None" in {
    val sum = for {
      a <- EjerciciosReview.makeInt("1")
      b <- EjerciciosReview.makeInt("Cualquier cosa")
    } yield (a + b)

    sum shouldEqual None
  }
}
