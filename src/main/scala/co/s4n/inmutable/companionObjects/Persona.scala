package co.s4n.inmutable.companionObjects

/**
 * Clase Persona que recibe el nombre y los apellidos
 * @param nombres
 * @param apellidos
 */
class Persona(val nombre:String, val apellido:String) {
  /**
   * Devuelve el nombre y apellido
   * @return Nombre Completo
   */
  def nombres = s"$nombre $apellido"
}

object Persona {
  /**
   * El objeto de compañía recibirá solo un nombre, pero al instanciar la clase, se le envían los dos parámetros
   * @param nombreCompleto
   * @return Nueva instancia a Persona
   */
  def apply(nombreCompleto:String) = new Persona(nombreCompleto.split(" ")(0),nombreCompleto.split(" ")(1))

  def main(args:Array[String]):Unit = {
    val c:Persona = Persona("Anna Smith")
    println(c.nombres)
  }
}
