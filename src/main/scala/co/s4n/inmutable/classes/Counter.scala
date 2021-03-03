package co.s4n.inmutable.classes

/**
 * Clase contador que permite incrementar y decrementar un valor
 * @param Int valor
 */
class Counter(private val valor:Int = 0) {
  /**
   * Incrementa un valor por unidad
   * @return Nuevo objeto con valor incrementado
   */
  def incr():Counter = new Counter(valor + 1)

  /**
   * Incrementa un valor con la cantidad indicada por parámetro
   * @param Int incremento
   * @return Nuevo objeto con valor incrementado
   */
  def incr(incremento:Int = 1):Counter =	new Counter(valor + incremento)

  /**
   * Decrementa un valor por unidad
   * @return Nuevo objeto con valor decrementado
   */
  def decr():Counter = new Counter(if(valor <= 0) 0 else valor - 1)

  /**
   * Decrementa un valor con la cantidad indicada por parámetro
   * @param Int decremento
   * @return Nuevo objeto con valor decrementado
   */
  def decr(decremento:Int = 1):Counter =	new Counter(if(valor - decremento <= 0) 0 else valor - decremento)

  /**
   * Función que devuelve el contador
   * @return Valor operado
   */
  def contador:Int = valor
}
