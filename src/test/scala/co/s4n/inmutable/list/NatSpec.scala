package co.s4n.inmutable.list

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NatSpec extends AnyFlatSpec with Matchers {

  "El ejercicio 9 " should " Cero" in {
    val cero =  Cero
    cero shouldEqual Cero
  }

  "El ejercicio 9 " should " uno " in {
    val uno =  Suc(Cero)
    uno shouldEqual Suc(Cero)
  }

  "El ejercicio 9 " should " dos " in {
    val dos = Suc(Suc(Cero))
    dos shouldEqual Suc(Suc(Cero))
  }

  "El ejercicio 10: Cero" should " be 0 " in {
    val cero = Nat.fromNatToInt(Cero)
    cero shouldEqual 0
  }

  "El ejercicio 10: Suc(Suc(Suc(Cero))) " should " be 3 " in {
    val tres = Nat.fromNatToInt(Suc(Suc(Suc(Cero))))
    tres shouldEqual 3
  }

  "El ejercicio 10: Suc(Suc(Suc(Suc(Suc(Cero))))) " should " be 5 " in {
    val cinco = Nat.fromNatToInt(Suc(Suc(Suc(Suc(Suc(Cero))))))
    cinco shouldEqual 5
  }

  "El ejercicio 11: 0" should " be Cero " in {
    val cero = Nat.fromIntToNat(0)
    val ceroNat = Cero
    cero shouldEqual ceroNat
  }

  "El ejercicio 11: 4" should " be Suc(Suc(Suc(Suc(Cero)))) " in {
    val cuatro = Nat.fromIntToNat(4)
    val cuatroNat = Suc(Suc(Suc(Suc(Cero))))
    cuatro shouldEqual cuatroNat
  }

  "El ejercicio 11: 6" should " be Suc(Suc(Suc(Suc(Suc(Suc(Cero)))))) " in {
    val seis = Nat.fromIntToNat(6)
    val seisNat = Suc(Suc(Suc(Suc(Suc(Suc(Cero))))))
    seis shouldEqual seisNat
  }
}
