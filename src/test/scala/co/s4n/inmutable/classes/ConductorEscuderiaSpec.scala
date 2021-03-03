package co.s4n.inmutable.classes

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ConductorEscuderiaSpec extends AnyFlatSpec with Matchers {

    "Escudería " should " retornar conductor asociado " in {
      val conductor = new Conductor("Keiny","Pacheco",4,1)
      val escuderia = new Escuderia("Esc 1",conductor)
      escuderia.getConductor().getNombre() shouldEqual "Keiny"
    }
}
