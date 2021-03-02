package co.s4n.inmutable.classes

object Comp {

  def cuadrado(valor:Float) = cubo(valor)

  def cubo(valor:Float) = valor * valor * valor
}

object Comp2 {

  def cuadrado(valor:Long) = cubo(valor)

  def cubo(valor:Long) = valor * valor * valor
}