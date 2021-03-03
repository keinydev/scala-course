package co.s4n.inmutable.companionObjects

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PersonaSpec extends AnyFlatSpec with Matchers  {

  "Keiny Pacheco" should " retornar nombre completo " in {
    val c:Persona = Persona("Keiny Pacheco")
    c.nombres shouldEqual "Keiny Pacheco"
  }

  "John Doe" should " retornar nombre John" in {
    val c:Persona = Persona("John Doe")
    c.nombre shouldEqual "John"
  }

  "John Doe" should " retornar apellido Doe" in {
    val c:Persona = Persona("John Doe")
    c.apellido shouldEqual "Doe"
  }
}
