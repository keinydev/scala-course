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
  def incr(incremento:Int = 1):Counter = new Counter(valor + incremento)

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
  def decr(decremento:Int = 1):Counter = new Counter(if(valor - decremento <= 0) 0 else valor - decremento)

  /**
   * Este método acepta un Sumador y retorna un nuevo Contador con el resultado de aplicar el Sumador a el conteo
   * @param sumador
   * @return Nuevo resultado
   */
  def ajuste(sdr: Sumador):Counter = new Counter(sdr.adicionar(valor))

  /**
   * Función que devuelve el contador
   * @return Valor operado
   */
  def contador:Int = valor
}

class Sumador(monto: Int) {
  def adicionar(valor: Int) = valor + monto
}