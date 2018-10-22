package ar.edu.unq.o3.Pifyo

import ar.edu.unq.o3.Pifyo.Pifyo._

import scala.collection.mutable.ArrayBuffer

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


  def Chequeador : ( Programa,  Chequeo) => Problema = (prg, cheq) => cheq.chequear(prg).getOrElse(Problema("","",""))

  def chequeadorMultiple : (Programa, List[Chequeo]) => ArrayBuffer[Problema] = {
    (prog, chequeos) =>
      var problemasEncontrados = ArrayBuffer[Problema]()
      chequeos.foldLeft(problemasEncontrados){
        (acc,f) => acc += Chequeador(prog,f)
      }
       problemasEncontrados
  }
  case class Problema(descripcion:String, gravedad: String, instruccion:String)
  case class Programa(instruccion: Instruccion)

  trait Instruccion

  trait Chequeo {
    def chequear : Programa =>Option[Problema]

  }


  case class SumaCero() extends  Chequeo {

   override def chequear : Programa=> Option[Problema] = prg => prg match  {
      case Pifyo.Programa(Suma(Numero(0),_)) => Some(Problema("Operacion redundante de suma 0","Advertencia","Suma"))
      case Pifyo.Programa(Suma(_,Numero(0))) => Some(Problema("Operacion redundante de suma 0","Advertencia","Suma"))
      case  Pifyo.Programa(_) => None
   }

  }

  case class RestarCero() extends  Chequeo {

    override def chequear : Programa=> Option[Problema] = prg => prg match  {
      case Pifyo.Programa(Resta(_,Numero(0))) => Some(Problema("Operacion redundante de resta 0","Advertencia","Resta"))
      case  Pifyo.Programa(_) => None
    }

  }

  case class DividirUno() extends  Chequeo {

    override def chequear : Programa=> Option[Problema] = prg => prg match  {
      case Pifyo.Programa(Division(_,Numero(1))) => Some(Problema("Operacion redundante de division 1","Advertencia","Division"))
      case  Pifyo.Programa(_) => None
    }

  }

  case class MultiplicarUno() extends  Chequeo {

    override def chequear : Programa=> Option[Problema] = prg => prg match  {
      case Pifyo.Programa(Multiplicacion(_,Numero(1))) => Some(Problema("Operacion redundante de Multiplicacion 1","Advertencia","Multiplicacion"))
      case  Pifyo.Programa(_) => None
    }

  }

  case class DivisionPorCero() extends  Chequeo {

    override def chequear : Programa=> Option[Problema] = prg => prg match  {
      case Pifyo.Programa(Division(_,Numero(0))) => Some(Problema("Division Por Cero","Error","Division"))
      case  Pifyo.Programa(_) => None
    }

  }

  case class ComparacionDosNumerosIguales() extends  Chequeo {

    override def chequear : Programa=> Option[Problema] = prg => prg match  {
      case Pifyo.Programa(Igual(Numero(_),Numero(_))) => Some(Problema("Comparacion de dos numeros iguales","Error","Igual"))
      case  Pifyo.Programa(_) => None //preguntar iguales
    }

  }

  case class ComparacionSiempreDaTrue() extends  Chequeo {

    override def chequear : Programa=> Option[Problema] = prg => prg match  {
      case Pifyo.Programa(Mayor(Numero(a:Int),Numero(b:Int)))  => Some(Problema("Comparacion que siempre da true","Error","Division"))
      case  Pifyo.Programa(_) => None
    }

  }

  case class ComparacionSiempreDaFalse() extends  Chequeo {

    override def chequear : Programa=> Option[Problema] = prg => prg match  {
      case Pifyo.Programa(Menor(_,Numero(0))) => Some(Problema("Comparacion que siempre da false","Error","Division"))
      case  Pifyo.Programa(_) => None
    }

  }
}


object pruebaSumaCero extends App {

  val programa = Programa(Suma((Numero(2)),(Numero(2))))
  val chequeo = SumaCero()
  val chequeo2 = ComparacionDosNumerosIguales()
  var chequeos = List[Chequeo](chequeo,chequeo2)
  val chequeador = chequeadorMultiple(programa, chequeos)
  println(chequeador)
}
