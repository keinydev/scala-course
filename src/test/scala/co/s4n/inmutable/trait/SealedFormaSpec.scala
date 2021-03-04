package co.s4n.inmutable.`trait`

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SealedFormaSpec extends AnyFlatSpec with Matchers {

  "Draw(Circulo_(10)) " should " Un circulo de radio 10.0 cm " in {
    Draw(Circulo_(10)) shouldEqual "Un circulo de radio 10.0 cm"
  }

  "Draw(Rectangulo_(4,3)) " should " Un rectangulo de ancho 4.0 y largo 3.0 cm" in {
    Draw(Rectangulo_(4,3)) shouldEqual "Un rectangulo de ancho 4.0 y largo 3.0 cm"
  }

  "Draw(Cuadrado_(3)) " should " Un cuadrado de lado 3.0 cm " in {
    Draw(Cuadrado_(3)) shouldEqual "Un cuadrado de lado 3.0 cm"
  }
}
