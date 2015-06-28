import ox.CSO._
import ox.Format._
import java.util.Random

/**
 * @author bruno_e_elis
 */
object lojavirtual {
  
    val produtos = List("Smartphone", "TV")
    var produtosBD = List(("Smartphone", 100, 2), ("TV", 200, 1))
  
  //respostaProdutoBD dividido em respostaPrecoBD e respostaEstoqueBD.
  def ACESSOCLIENTE(i: Int, consultaProdutoBD: ![String], respostaPrecoBD: ?[Int], respostaEstoqueBD: ?[Int], separaEstoqueBD: ![String], respostaSeparaEstoqueBD: ?[Boolean] ) = proc {
    println("acessoCliente." + i)
    //o sleep abaixo é pra garantir seeds diferentes da randomização
    Thread.sleep(i * 300)
    
    while(true){
      //randomizacao para acesso a produto.
      var seed = new Random(System.currentTimeMillis())
      var randomInt = 0
      //randomizando até 10, para em seguida dividir por 2 e pegar o resto - so temos 2 produtos, 0 ou 1
      randomInt = seed.nextInt(10)
      Thread.sleep(randomInt * 200)
   
      println("pesquisaProduto." + i)
      Thread.sleep(randomInt * 200)
       
      var produtoAcessado = ""
       
      //dividindo o random até 10 por 2 e pegando resto para escolher entre um dos produtos
      randomInt%2 match {
        case 0 => produtoAcessado = produtos(0)
        case 1 => produtoAcessado = produtos(1)
      }
       
      println("acessaProduto." + i +"."+ produtoAcessado)
  
      println("consultaProdutoBD." + i + "." + produtoAcessado)
      consultaProdutoBD!produtoAcessado
      
      var preco = 0
      var estoque = 0
      
      preco = respostaPrecoBD? ;
      estoque = respostaEstoqueBD? ;
      
      println("respostaPrecoBD."+i+"."+preco)
      println("respostaEstoqueBD."+i+"."+estoque)
      
      //se houver estoque oferece adicionarCarrinho, senao mensagemEstoqueIndisponivel
      if(estoque>0){
        println("adicionaCarrinho." + i + "." + produtoAcessado)
        Thread.sleep(500)
        println("separaEstoque." + i + "." + produtoAcessado)
        Thread.sleep(500)
        separaEstoqueBD!produtoAcessado
        
        var retorno = respostaSeparaEstoqueBD? ;
        retorno match {
          case true => println("enviaProduto")
          case false => println("Estoque indisponível")
        }
      } else { 
        println("Estoque indisponivel")
      }
      
      
    }
  }
  
  def BDLOJA(consultaProdutoBD: ?[String], respostaPrecoBD: ![Int], respostaEstoqueBD: ![Int], separaEstoqueBD: ?[String], respostaSeparaEstoqueBD: ![Boolean]) = proc {
    var produtoAcessado = ""
    while(true){
      alt ((true &&& consultaProdutoBD) =?=> { produto => CONSULTABD(produto, respostaPrecoBD, respostaEstoqueBD)() }
        | (true &&& separaEstoqueBD) =?=> { produto => SEPARAESTOQUEBD(produto, respostaSeparaEstoqueBD)() }
      )
    } 
  }
  
  def CONSULTABD(produtoConsultado: String, respostaPrecoBD: ![Int], respostaEstoqueBD: ![Int]) = proc {
      var preco = 0
      var estoque = 0
      
      //iterar na lista de produtos tuplas
      for(x <- 0 until produtosBD.length){
        //pegar a tupla do produto acessado
        var produto = produtosBD(x)
        if(produtoConsultado == produto._1){
          //pegar o preco do produto acessado
          preco = produto._2
          //pegar o estoque do produto acessado
          estoque = produto._3
        }
      }

      respostaPrecoBD!preco
      respostaEstoqueBD!estoque
      
  }
  
  def SEPARAESTOQUEBD(produtoConsultado: String, respostaSeparaEstoqueBD: ![Boolean]) = proc {
    
      var preco = 0
      var estoque = 0
      //iterar na lista de produtos tuplas
      for(x <- 0 until produtosBD.length){
        //pegar a tupla do produto acessado
        var produto = produtosBD.head
        if(produtoConsultado == produto._1){
          //pegar o preco do produto acessado
          preco = produto._2
          //pegar o estoque do produto acessado
          estoque = produto._3

          if(estoque > 0){
            //atualizar estoque na tupla
            var itemAtualizado = (produto._1, produto._2, produto._3 - 1)
            //atualizar lista com tupla modificada
            produtosBD = itemAtualizado::produtosBD.tail
            //retornar separaEstoque OK!
            respostaSeparaEstoqueBD!true
            } else {
             //retornar separaEstoque False!
              respostaSeparaEstoqueBD!false
            }
        } else {
          produtosBD = produtosBD.reverse
        }
      }
  }
  
  def ACESSOSCLIENTES(consultaProdutoBD: ![String], respostaPrecoBD: ?[Int], respostaEstoqueBD: ?[Int], separaEstoqueBD: ![String], respostaSeparaEstoqueBD: ?[Boolean]) = proc {
    (|| (for (i <- 1 until 3) yield ACESSOCLIENTE(i,consultaProdutoBD, respostaPrecoBD, respostaEstoqueBD, separaEstoqueBD, respostaSeparaEstoqueBD)))()
  }
  
  def main(args: Array[String]) {
    val consultaProdutoBD = OneOne[String]
    val respostaPrecoBD = OneOne[Int]
    val respostaEstoqueBD = OneOne[Int]
    val separaEstoqueBD = OneOne[String]
    val respostaSeparaEstoqueBD = OneOne[Boolean]
    
    val produtos = List("Smartphone", "TV")
    val produtosBD = List(("Smartphone", 100, 2), ("TV", 200, 1))

    (ACESSOSCLIENTES(consultaProdutoBD, respostaPrecoBD, respostaEstoqueBD, separaEstoqueBD, respostaSeparaEstoqueBD) || BDLOJA(consultaProdutoBD, respostaPrecoBD, respostaEstoqueBD, separaEstoqueBD, respostaSeparaEstoqueBD))()
    exit
  }
  
}



