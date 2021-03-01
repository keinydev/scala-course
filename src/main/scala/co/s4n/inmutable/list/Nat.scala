package co.s4n.inmutable.list
import scala.annotation.tailrec
sealed trait Nat
case object Cero extends Nat
case class Suc(nat: Nat) extends Nat

object Nat {
  /**
   * Esta función toma un número natural Nat y lo transforma a su valor Int
   * @param Nat n
   * @return Int Nuevo número entero
   */
  def fromNatToInt(n:Nat):Int = n match {
    case Cero => 0
    case Suc(n) => 1 + fromNatToInt(n)
  }

  /**
   * Esta función toma valores enteros positivo (inclusive el cero) y produce el correspondiente número natural.
   * @param Int n
   * @return Nat Nuevo número natural
   */
  def fromIntToNat(n:Int):Nat = n match {
    case 0 => Cero
    case n => Suc(fromIntToNat(n-1))
  }

  /**
   * Esta función recibe dos naturales y se encarga de sumarlo
   * @param nat1 Num1
   * @param nat2 Num2
   * @return Resultado de suma
   */
  def addNat(nat1: Nat, nat2: Nat): Nat = (nat1, nat2) match {
    case (Cero, n2) => n2
    case (Suc(n1), n2) => Suc(addNat(n1, n2))
  }

  /**
   * Esta función recibe dos naturales y se encarga de multiplicarlos
   * @param nat1 Num1
   * @param nat2 Num2
   * @return Resultado de multiplicación
   */
  def prodNat(nat1:Nat,nat2:Nat): Nat = {
    /**
     * Esta función realiza el calculo a base de sumatorias, y devuelve el acumulador
     * @param nat1 Num1
     * @param nat2 Num2
     * @param acum Número auxliar
     * @return Resultado de sumatorias
     */
    @tailrec
    def prodNatInterno(nat1:Nat,nat2:Nat,acum:Nat):Nat = (nat1,nat2) match {
      case (Cero,_) => Cero
      case (_,Cero) => Cero
      case (Suc(Cero),n2) => addNat(n2,acum)
      case (Suc(n1), n2) => prodNatInterno(n1, n2, addNat(n2, acum))
    }
    prodNatInterno(nat1,nat2,Cero)
  }
}
