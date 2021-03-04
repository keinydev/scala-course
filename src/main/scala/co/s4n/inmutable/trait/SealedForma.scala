package co.s4n.inmutable.`trait`

/**
 * Para este ejercicio se ha reescrito los nombres de las clases y los traits para que no hayan errores de compilación,
 * ya que existe otro archivo que tiene la misma estructura llamado Forma.scala
 */
sealed trait Formas {
  /**
   * Retorna el número de lados de la forma
   * @return valor
   */
  def tamano:Double

  /**
   * Devuelve longitud total de sus lados
   * @return valor
   */
  def perimetro:Double

  /**
   * Devuelve el área de la forma
   * @return
   */
  def area:Double
}

sealed trait Rectangulares extends Formas {
  /**
   * Los cuadrados y los rectangulos tienen 4 lados
   *  @return valor
   */
  def tamano:Double = 4
}

case class Circulo_(val radio:Double) extends Formas {

  def tamano:Double = 0.0

  def perimetro:Double = 2 * math.Pi * radio

  def area:Double = math.Pi * radio * radio

  override def toString: String = s"Radio: $radio "
}

case class Rectangulo_(val base:Double, val altura:Double) extends Rectangulares {

  def perimetro: Double = 2 * (base + altura)

  def area: Double = base * altura

  override def toString: String = s"Base: $base y Altura: $altura"
}

case class Cuadrado_(val lado:Double) extends Rectangulares{

  def perimetro: Double = 4 * lado

  def area: Double = lado * lado

  override def toString: String = s"Lado: $lado"
}

object Draw {
  def apply(form:Formas):String = form match {
    case Circulo_(rad)               =>  s"Un circulo de radio $rad cm"
    case Rectangulo_(ancho, largo)   =>  s"Un rectangulo de ancho $ancho y largo $largo cm"
    case Cuadrado_(lado)             =>  s"Un cuadrado de lado $lado cm"
  }
}
