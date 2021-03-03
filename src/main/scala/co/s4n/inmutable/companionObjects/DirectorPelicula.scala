package co.s4n.inmutable.companionObjects

class Director(val nombre:String,  val apellido:String, val nacimiento:Int){
  /**
   * Devuelve el nombre y el apellido del director
   * @return Nombre completos
   */
  def nombres:String = s"$nombre $apellido"

  /**
   * Hace una copia del objeto
   * @param nombre
   * @param apellido
   * @param nacimiento
   * @return Copia del mismo objeto
   */
  def copy(nombre:String = this.nombre,
           apellido:String = this.apellido,
           nacimiento:Int = this.nacimiento) = new Director(nombre, apellido, nacimiento)
}

object Director {
  /**
   * Acepta los mismos parámetros del constructor de la clase y retorna un nuevo Director
   * @param nombre
   * @param apellido
   * @param nacimiento
   * @return Nuevo Director
   */
  def apply(nombre:String, apellido:String, nacimiento:Int) = new Director(nombre, apellido, nacimiento)

  /**
   * Acepta dos directores y retorna el mayor de ellos
   * @param dir1
   * @param dir2
   * @return Director mayor
   */
  def esMayor(dir1:Director, dir2:Director) = if(dir1.nacimiento < dir2.nacimiento) dir1 else dir2
}

class Pelicula (val nombre:String, val presentacion:Int, val rangoIMDB:Double, val director:Director){

  /**
   * Calcula la edad del director
   * @return cálculo
   */
  def directorEdad:Int = presentacion - director.nacimiento

  /**
   * Devuelve si el director dirigió la película
   * @param director
   * @return Validación
   */
  def esDirigidaPor(director:Director) = this.director == director

  /**
   * Hace una copia del objeto
   * @param String nombre
   * @param Int presentacion
   * @param Double rangoIMDB
   * @param director
   * @return Copia del mismo objeto
   */
  def copy(nombre:String = this.nombre,
           presentacion:Int = this.presentacion,
           rangoIMDB:Double = this.rangoIMDB,
           director:Director = this.director):Pelicula = new Pelicula(nombre, presentacion, rangoIMDB, director)
}

object Pelicula {
  /**
   * Acepta los mismos parámetros del constructor de la clase y retorna una nueva pelicula
   * @param nombre
   * @param presentacion
   * @param rangoIMDB
   * @param director
   * @return Nueva película
   */
  def apply(nombre:String, presentacion:Int, rangoIMDB:Double, director:Director) = new Pelicula(nombre, presentacion, rangoIMDB, director)

  /**
   * Acepta dos Pelicula(s) y retorna la que tiene mayor rangoIMDB entre las dos
   * @param peli1
   * @param peli2
   * @return Película mayor calificada
   */
  def mejorCalificada(peli1:Pelicula, peli2:Pelicula):Pelicula = if(peli1.rangoIMDB > peli2.rangoIMDB) peli1 else peli2

  /**
   * Acepta dos Pelicula(s) y retorna el Director que fue mayor en el momento de presentar la pelı́cula.
   * @param peli1
   * @param peli2
   * @return Director mayor
   */
  def mayorDirectorEnElTiempo(peli1:Pelicula, peli2:Pelicula):Director = if(peli1.directorEdad > peli2.directorEdad) peli1.director else peli2.director
}

