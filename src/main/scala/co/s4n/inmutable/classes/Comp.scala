package co.s4n.inmutable.classes

object Comp {
  /**
   * Calcula el cuadrado de un número flotante
   * @param valor
   * @return Valor calculado
   */
  def cuadrado(valor:Float):Float = valor * valor

  /**
   * Calcula el cubo de un número flotante
   * @param valor
   * @return Valor calculado
   */
  def cubo(valor:Float):Float = valor * cuadrado(valor)
}

object Comp2 {
  /**
   * Calcula el cuadrado de un número tipo Long
   * @param valor
   * @return Valor calculado
   */
  def cuadrado(valor:Long):Long = valor * valor
  /**
   * Calcula el cubo de un número tipo Long
   * @param valor
   * @return Valor calculado
   */
  def cubo(valor:Long):Long = valor * cuadrado(valor)
}