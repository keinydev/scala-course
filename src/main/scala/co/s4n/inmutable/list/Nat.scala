package co.s4n.inmutable.list

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
}
