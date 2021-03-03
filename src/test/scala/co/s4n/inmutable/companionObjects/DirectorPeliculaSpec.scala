package co.s4n.inmutable.companionObjects

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DirectorPeliculaSpec extends AnyFlatSpec with Matchers {

  "Comparar edad dir1 y dir2 " should " edad mayor " in {
    val dir1:Director = Director("Juan","Lopez",1980)
    val dir2:Director = Director("Elena","Gomez",1978)
    Director.esMayor(dir1, dir2).nombre shouldEqual "Elena"
  }

  "Validar dir1 dirigió peli1Dir1 " should " true" in {
    val dir1:Director = Director("Jane","Rory",1982)
    val peli1Dir1 = Pelicula("Olvidados",2020,8.0,dir1)

    val dir2:Director = Director("Lorelai","Gilmore",1980)
    val peli1Dir2 = Pelicula("Primaveras",2015,5.0,dir2)

    peli1Dir1.esDirigidaPor(dir1) shouldEqual true
  }

  "Comparar calificación pelicula peli1 y peli2  " should " mejor calificada" in {
    val dir1:Director = Director("Jane","Rory",1982)
    val peli1 = Pelicula("Olvidados",2020,8.0,dir1)
    val peli2 = Pelicula("Primaveras",2015,5.0,dir1)
    Pelicula.mejorCalificada(peli1, peli2).nombre shouldEqual "Olvidados"
  }

  "Comparar mayor director con peli1Dir1 y peli1Dir2  " should " mayor director" in {
    val dir1:Director = Director("Jane","Rory",1982)
    val peli1Dir1 = Pelicula("Olvidados",2020,8.0,dir1)

    val dir2:Director = Director("Lorelai","Gilmore",1980)
    val peli1Dir2 = Pelicula("Primaveras",2015,5.0,dir2)

    Pelicula.mayorDirectorEnElTiempo(peli1Dir1, peli1Dir2).nombre shouldEqual "Jane"
  }
}
