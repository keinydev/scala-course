package co.s4n.inmutable.classes

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CompSpec extends AnyFlatSpec with Matchers {

  "Metodo cuadrado " should " computar el cubo " in {
    Comp.cuadrado(2) shouldEqual 8
  }

  "Metodo cuadrado " should " computar el cubo usando tipo long" in {
    Comp2.cuadrado(10L) shouldEqual 1000
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