package ar.edu.unq.o3.Pifyo

import ar.edu.unq.o3.Pifyo.Pifyo._

object Pifyo {


  case class Numero(i: Int)

  case class Suma(numeroA: Numero, numeroB: Numero) extends Instruccion
  case class Resta(numeroA: Numero, numeroB: Numero) extends Instruccion
  case class Multiplicacion(numeroA: Numero, numeroB: Numero) extends Instruccion
  case class Division(numeroA: Numero, numeroB: Numero) extends Instruccion
  case class Mayor(numeroA: Numero, numeroB: Numero) extends Instruccion
  case class Menor(numeroA: Numero, numeroB: Numero) extends Instruccion
  case class Igual(numeroA: Numero, numeroB: Numero) extends Instruccion
  case class Distinto(numeroA: Numero, numeroB: Numero) extends Instruccion
  case class MenorOIgual(numeroA: Numero, numeroB: Numero) extends Instruccion
  case class MayorOIgual(numeroA: Numero, numeroB: Numero) extends Instruccion


  def Chequeador : ( Programa,  Chequeo) => Problema = (prg, cheq) => cheq.chequear(prg)


  case class Problema(descripcion:String, gravedad: String, instruccion:String)
  case class Programa(instruccion: Instruccion)

  trait Instruccion

  trait Chequeo {
    def chequear : Programa => Problema

  }


  case class SumaCero() extends  Chequeo {

   override def chequear : Programa=> Problema = prg => prg match  {
      case Pifyo.Programa(Suma(Numero(0),_)) => Problema("Operacion redundante de suma 0","Advertencia","Suma")
      case Pifyo.Programa(Suma(_,Numero(0))) => Problema("Operacion redundante de suma 0","Advertencia","Suma")
      case  Pifyo.Programa(_) => Problema("","","")
   }

  }

  type OperacionEnteros = (Int, Int) => Int
  def _sumar4: OperacionEnteros = (a, b) => a + b

}


object pruebaSumaCero extends App {

  val programa = Programa(Suma((Numero(0)),(Numero(3))))
  val chequeo = SumaCero()
  val chequeador = Chequeador(programa, chequeo )
  println(chequeador)
}
