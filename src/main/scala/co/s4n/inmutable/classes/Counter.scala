package co.s4n.inmutable.classes

/**
 * Clase contador que permite incrementar y decrementar un valor
 * @param valorIncremento
 */
class Counter(private val valorIncremento:Int = 1) {
  /**
   * Incrementa un valor
   * @return Nuevo objeto con valor incrementado
   */
  def incr():Counter = new Counter(valorIncremento + 1)

  /**
   * Decrementa un valor
   * @return Nuevo objeto con valor decrementado
   */
  def decr():Counter = new Counter(if(valorIncremento <= 0) 0 else valorIncremento - 1)

  /**
   * Función que devuelve el contador
   * @return Valor operado
   */
  def contador:Int = valorIncremento
}
