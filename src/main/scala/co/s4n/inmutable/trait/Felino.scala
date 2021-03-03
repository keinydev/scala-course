package co.s4n.inmutable.`trait`

trait Felino {
  val color:String
  val sonido:String
}

class Gato(val color:String, val sonido:String, val comidaFavorita:String) extends Felino

class Leon(val color:String, val sonido:String, val tamanoMelena:Double) extends Felino

class Jaguar(val color:String, val sonido:String) extends Felino

class Tigre(val color:String, val sonido:String) extends Felino



