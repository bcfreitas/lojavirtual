// copy = left?x -> right!x -> copy

import ox.CSO._
import ox.Format._
import java.util.Random

object Exemplo1 {

  def sender(left: ![Int]) = proc {
    val seed = new Random(System.currentTimeMillis())
    var expr = 0
    while(true) {
      expr = seed.nextInt(100)+1
      left!expr
      println("Enviei o " + expr)
    }
  }
  def copy(left: ?[Int], right: ![Int]) = proc {
    var x: Int = 0
    while(true) {
      x = left? ;
      println("Recebi o " + x)
      right!x
    }
  }
  def receiver(right: ?[Int]) = proc {
    var x = 0
    while(true) {
      x = right? ;
      println("Receiver = " + x)
    }
  }
  def main(args: Array[String]) {
    val left, right = OneOne[Int]

    (sender(left) || copy(left, right) || receiver(right))()
    exit
  }
}