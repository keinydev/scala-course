package co.s4n.inmutable.classes

/**
 * Ejemplo de clases desarrollado en la mentoría
 */
class Forma {
  def area: Double = 0.0
}

class Rectangulo(val ancho:Double, val altura:Double) extends Forma {
  override def area: Double = ancho * altura
}

class Circulo(val radio:Double) extends Forma {
  override def area: Double = math.Pi * radio * radio
}

//val forma:Forma = new Rectangulo(2.0,4.0)
//val forma2:Forma = new Circulo(2.3)
//forma.area
//forma2.area