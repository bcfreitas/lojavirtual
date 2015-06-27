// channel pickup, putdown:{0..4}.{0..4}
// channel sitdown, getup:{0..4}
// inc(x) = (x + 1) % 5
// dec(x) = (x - 1) % 5
// PHIL(i) =  sitdown.i -> pickup.i.inc(i) -> pickup.i.i -> 
//           putdown.i.inc(i) -> putdown.i.i -> getup.i -> PHIL(i)
// FORK(i) = pickup.i.i -> putdown.i.i -> FORK(i)
//         [] pickup.dec(i).i -> putdown.dec(i).i -> FORK(i)
// PHILS = || i:{0..4} @ [{|pickup.i.i, pickup.i.inc(i), putdown.i.i, putdown.i.inc(i), sitdown.i, getup.i|}] PHIL(i)
// FORKS = || i:{0..4} @ [{|pickup.i.i, putdown.i.i, pickup.dec(i).i, putdown.dec(i).i|}] FORK(i)
// COLLEGE = PHILS [ {|pickup,putdown,sitdown,getup|} || {|pickup,putdown|} ] FORKS

import ox.CSO._
import ox.Format._
import java.util.Random

object Exemplo3 {
  val N = 5
  def dec(i: Int):Int = {
    if(i==0) N-1
    else i-1
  }
  def PHIL(i: Int, pickup: Seq[?[Unit]], putdown: Seq[![Unit]]) = proc {
    while(true) {
      println("PHIL(" + i + ") tentando pegar o FORK ("+ ((i+1)%N) + ")...")
      pickup((i+1)%N)? ;
      println("PHIL(" + i + ") pegou o FORK ("+ ((i+1)%N) + ")...")
      println("PHIL(" + i + ") tentando pegar o FORK ("+ i + ")...")
      pickup(i)? ;
      println("PHIL(" + i + ") pegou o FORK ("+ i + ")...")
      println("PHIL(" + i + ") tentando liberar o FORK ("+ ((i+1)%N) + ")...")
      putdown((i+1)%N)!()
      println("PHIL(" + i + ") liberou o FORK ("+ ((i+1)%N) + ")...")
      println("PHIL(" + i + ") tentando liberar o FORK ("+ i + ")...")
      putdown(i)!()
      println("PHIL(" + i + ") liberou o FORK ("+ i + ")...")
    }
  }
  def FORK(i: Int, pickup: Seq[![Unit]], putdown: Seq[?[Unit]]) = proc {
    while(true) {
      alt( (true &&& pickup(i)) =!=> { () } ==> { putdown(i)? }
        | (true &&& pickup(dec(i))) =!=> { () } ==> { putdown(dec(i))? })
    }
  }
  def PHILS(pickup: Seq[?[Unit]], putdown: Seq[![Unit]]) = proc {
    (|| (for (i <- 0 until 5) yield PHIL(i, pickup, putdown)))()
  }
  def FORKS(pickup: Seq[![Unit]], putdown: Seq[?[Unit]]) = proc {
    (|| (for (i <- 0 until 5) yield FORK(i, pickup, putdown)))()
  }
  def COLLEGE() = proc {
    val pickup, putdown = OneOne[Unit](N)
    (PHILS(pickup, putdown) || FORKS(pickup, putdown))()
  }
  def main(args: Array[String]) {
    (COLLEGE())()
    exit
  }  
}