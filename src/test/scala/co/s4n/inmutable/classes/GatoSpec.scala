package co.s4n.inmutable.classes

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GatoSpec extends AnyFlatSpec with Matchers{

  "A Io le gusta el Churrus, entonces despachar " should " be true " in {
    val Io = new Gato("Io","Fawn","Churrus")
    VentaDeChurrus.despachar(Io) shouldEqual true
  }

  "A Make le gusta la Leche, entonces despachar" should " be false " in {
    val Make = new Gato("Make","Fawn","Leche")
    VentaDeChurrus.despachar(Make) shouldEqual false
  }
}
