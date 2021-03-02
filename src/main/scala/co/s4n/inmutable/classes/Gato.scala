package co.s4n.inmutable.classes

/**
 * Clase Gato
 * @param nombre
 * @param color
 * @param comida
 */
class Gato(val nombre:String, val color:String, val comida:String) {
  def descripcion = "A "+ nombre + " le gusta comer "+comida+ " y su color favorito es "+color
}

object VentaDeChurrus {
  /**
   * Valida si se despacha o no según la comida del gato
   * @param b Objeto que instancia a la clase Gato
   * @return Valor booleano de validación
   */
  def despachar(b:Gato):Boolean = if(b.comida == "Churrus") true else false
}

//val Io = new Gato("Io","Fawn","Churrus")
//val Make = new Gato("Make","Fawn","Leche")
//val Docker = new Gato("Io","Fawn","Churrus")