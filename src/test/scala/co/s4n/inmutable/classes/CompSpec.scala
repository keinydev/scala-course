package co.s4n.inmutable.classes

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CompSpec extends AnyFlatSpec with Matchers {

  "Metodo tipo Float: Cuadrado de 2 " should " 4.0 " in {
    Comp.cuadrado(2) shouldEqual 4.0
  }

  "Metodo tipo Float: Cubo de 2" should " 8.0" in {
    Comp.cubo(2) shouldEqual 8.0
  }

  "Metodo tipo Long: Cuadrado de 10 " should " 100" in {
    Comp2.cuadrado(10L) shouldEqual 100
  }

  "Metodo tipo Long: Cubo de 10 " should " 1000" in {
    Comp2.cubo(10L) shouldEqual 1000
  }

  "Usando REPL el objeto debe calcular " should " x valor" in {
    object prueba {
      def x = {
        println("x")
        1
      }
      val y = {
        println("y")
        x + 2
      }
      def z = {
        println("z")
        x
        x + "c"
      }
    }

    prueba.x + prueba.y + prueba.z shouldEqual "41c"
  }

}