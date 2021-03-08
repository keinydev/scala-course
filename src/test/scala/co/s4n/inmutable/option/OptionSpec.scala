package co.s4n.inmutable.option

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OptionSpec extends AnyFlatSpec with Matchers{

  "media(List())" should " None" in {
    Option.media(List()) shouldEqual None
  }

  "media(List())" should " 0.0" in {
    Option.media(List()).getOrElse(0.0) shouldEqual 0.0
  }

  "mapa(List(1,2,3,4,5,6,7))(_*2)" should " List(2, 4, 6, 8, 10, 12, 14)" in {
    Option.mapa(List(1,2,3,4,5,6,7))(_*2) shouldEqual List(2, 4, 6, 8, 10, 12, 14)
  }

  "filtro(List(1,2,3,4,5,6,7))(_>3)" should " List(4,5,6,7)" in {
    Option.filtro(List(1,2,3,4,5,6,7))(_>3) shouldEqual  List(4,5,6,7)
  }

  "divideA(3,5)" should " false" in {
    Option.divideA(3,5) shouldEqual false
  }

  "divideA(3,9)" should " true" in {
    Option.divideA(3,9) shouldEqual true
  }

  "divisoresDe(10)" should " List(1, 2, 5, 10)" in {
    Option.divisoresDe(10) shouldEqual List(1, 2, 5, 10)
  }

  "esPrimo(5)" should " true" in {
    Option.esPrimo(5) shouldEqual true
  }

  "esPrimo(1)" should " false" in {
    Option.esPrimo(1) shouldEqual false
  }
}
