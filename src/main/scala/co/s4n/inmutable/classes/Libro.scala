package co.s4n.inmutable.classes

/**
 * Ejemplo de clase Libro desarrollado en la mentoría
 * @param titulo
 * @param autor
 * @param ref
 */
class Libro(val titulo:String, val autor:String, val ref:String) {

  def nombre = titulo + " " + autor + " " + ref
}

object editorial {
  def presentar(b:Libro) = "Presentamos a "+ b.titulo + " de " + b.autor + " un ejemplar de " + b.ref
}
