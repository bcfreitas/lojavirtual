// IDS = {1,2,3,4}
// channel passaCartao, consultaServidor: IDS
// channel libera, barra
// Catraca = passaCartao?id -> consultaServidor!id -> (libera -> Catraca [] barra -> Catraca)
// Servidor = consultaServidor?id -> (libera -> Servidor |~| barra -> Servidor)
// Sistema = Catraca [|{|consultaServidor, libera, barra|}|] Servidor

import ox.CSO._
import ox.Format._
import java.util.Random

object Exemplo2 {
  def Catraca(consultaServidor: ![Int], libera: ?[Unit], barra: ?[Unit]) = proc {
    while(true) {
      print("Digite código do cartão: ")
      val id = Console.readInt() // passaCartao?id
      consultaServidor!id
      alt ((true &&& libera) =?=> { x => println("Portão liberado!") }
        | (true &&& barra) =?=> { x => println("Portão barrado!") }
      )
    }
  }
  def Servidor(consultaServidor: ?[Int], libera: ![Unit], barra: ![Unit]) = proc {
    var id: Int = 0
    val seed = new Random(System.currentTimeMillis())
    var expr = 0
    while(true) {
      id = consultaServidor? ;    
      expr = seed.nextInt(2)+1
      println("Sorteio resultou em " + expr)
      expr match {
        case 1 =>
          libera!()
        case 2 =>
          barra!()
      }
    }
  }
  def Sistema() = proc {
    val consultaServidor = OneOne[Int]
    val libera, barra = OneOne[Unit]
    (Catraca(consultaServidor, libera, barra) || Servidor(consultaServidor, libera, barra))()
  }
  def main(args: Array[String]) {
    (Sistema())()
    exit
  }
}