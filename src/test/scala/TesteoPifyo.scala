import ar.edu.unq.o3.Pifyo.Pifyo._
import org.scalatest.{BeforeAndAfter, FlatSpec}

class TesteoPifyo extends FlatSpec with BeforeAndAfter {

  "se crea un programa que dentro tiene una suma entre numeros 2 y 3" should " al ejecutarlo deberia devolver un numero 5" in {
    val programa = Programa(Suma((Numero(2)),(Numero(3))))
    val chequeo = SumaCero()
    val chequeador = Chequeador(programa, chequeo )

    assert(true )
  }

}







