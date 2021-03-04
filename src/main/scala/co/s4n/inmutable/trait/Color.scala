package co.s4n.inmutable.`trait`

/**
 * Clase color
 * @param red
 * @param green
 * @param blue
 */
class Color(val red:Int,val green:Int, val blue:Int) {
  override def toString:String = s"R:$red G:$green B:$blue"
}

object Rojo extends Color(255,0,0)
object Amarillo extends Color(255,255,0)
object Rosa extends Color(255,192,203)

object Color {
  /**
   * Permite crear un nuevo color
   * @param red
   * @param green
   * @param blue
   * @return Nuevo objeto
   */
  def apply(red: Int, green:Int, blue:Int) = new Color (red, green, blue)
}