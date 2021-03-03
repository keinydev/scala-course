package co.s4n.inmutable.`trait`

trait Forma {
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

trait Rectangular extends Forma {
  /**
   * Los cuadrados y los rectangulos tienen 4 lados
   *  @return valor
   */
  def tamano:Double = 4
}

class Circulo(val radio:Double) extends Forma {

  override def tamano:Double = 0.0

  override def perimetro:Double = 2 * math.Pi * radio

  override def area:Double = math.Pi * radio * radio

  override def toString: String = s"Radio: $radio "
}

class Rectangulo(val base:Double, val altura:Double) extends Rectangular {

  override def perimetro: Double = 2 * (base + altura)

  override def area: Double = base * altura

  override def toString: String = s"Base: $base y Altura: $altura"
}

class Cuadrado(val lado:Double) extends Rectangular{

  override def perimetro: Double = 4 * lado

  override def area: Double = lado * lado

  override def toString: String = s"Lado: $lado"
}
