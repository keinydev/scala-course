package co.s4n.inmutable.`trait`

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FormaSpec extends AnyFlatSpec with Matchers {

  "Area de circulo radio = 5 " should " 78.53981633974483 " in {
    val circ:Forma = new Circulo(5)
    circ.area shouldEqual 78.53981633974483
  }

  "Area de cuadrado lado = 3 " should " 9 " in {
    val cuad:Forma = new Cuadrado(3)
    cuad.area shouldEqual 9
  }

  "Perímetro de rectangulo base = 4 y altura = 7 " should " 22.0 " in {
    val rec:Forma = new Rectangulo(4,7)
    rec.perimetro shouldEqual 22.0
  }
}
