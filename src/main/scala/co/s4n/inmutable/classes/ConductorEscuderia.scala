package co.s4n.inmutable.classes

/**
 * Clase conductor que permite devolver el nombre, apellido, total de carreras, carreras terminadas y las carreras no terminadas
 * @param nombre
 * @param apellido
 * @param totalCarreras
 * @param carrerasTerminadas
 */
class Conductor(private val nombre:String, private val apellido:String, private val totalCarreras:Int, private val carrerasTerminadas:Int) {
  /**
   * Devuelve el nombre
   * @return Nombre
   */
  def getNombre():String = nombre

  /**
   * Devuelve el apellido
   * @return Apellido
   */
  def getApellido():String = apellido

  /**
   * Devuelve el total de carreras
   * @return Total carreras
   */
  def getTotalCarreras():Int = totalCarreras

  /**
   * Devuelve las carreras terminadas
   * @return Carreras terminadas
   */
  def getCarrerasTerminadas():Int = carrerasTerminadas

  /**
   * Devuelve las carreras no terminadas
   * @return Carreras no terminadas
   */
  def getCarrerasNoTerminadas():Int = if(totalCarreras > carrerasTerminadas) totalCarreras - carrerasTerminadas else 0
}

/**
 * Almacena un nombre y el conductor asociado
 * @param nombre
 * @param conductor
 */
class Escuderia(private val nombre:String, private val conductor: Conductor) {

  /**
   * Devuelve nombre de la escudería
   * @return Nombre
   */
  def getNombre():String = nombre

  /**
   * Devuelve los datos del objeto instanciado a la clase conductor
   * @return Conductor
   */
  def getConductor():Conductor = conductor
}
