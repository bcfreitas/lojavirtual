import ox.CSO._
import ox.Format._
import java.util.Random

/**
 * @author bruno_e_elis
 */
object lojavirtual {
  
    val produtos = List("Smartphone", "TV")
    var produtosBD = List(("Smartphone", 100, 2), ("TV", 200, 1))
    val usuarios = List("Joao", "Maria")
    val usuariosBD = Map("Joao" -> "123", "Maria" -> "345")
  
  //respostaProdutoBD dividido em respostaPrecoBD e respostaEstoqueBD.
  def ACESSOCLIENTE(i: Int, consultaProdutoBD: ![String], respostaPrecoBD: ?[Int], respostaEstoqueBD: ?[Int], separaEstoqueBD: ![String], 
                      respostaSeparaEstoqueBD: ?[Boolean], reverteEstoqueBD: ![String], autenticaUsuario: ![String], enviaSenha: ![String], retornoLogin: ?[Boolean] ) = proc {
    println("acessoCliente." + i)
    //o sleep abaixo é pra garantir seeds diferentes da randomização
    Thread.sleep(i * 300)
    
    var carrinho = List[(String,Int,Int)]()
    
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
          case true => 
             {
               println("produto Adicionado no carrinho")
               carrinho = (produtoAcessado, preco, 1)::carrinho
              }
          case false => println("Estoque indisponível")
        }
      } else { 
        println("Estoque indisponivel")
      }
      
      if(carrinho.length > 0){
        //se houver itens no carrinho, o cliente podera remover, navegar ou concluir compra.
        var seed = new Random(System.currentTimeMillis())
        var randomInt = 0
        randomInt = seed.nextInt(9)
        
        randomInt%3 match {
          case 0 => {
            println("concluirCompra." + i)
            
            var logado = false;
            //convencionei acesso 1 = joao e acesso 2 = maria
            
            while(!logado){
              println("autenticaUsuario." + i + "." + usuarios(i-1))
              autenticaUsuario!usuarios(i-1)
              
              //randomizando para enviar senha
              var senha = ""
              var seed = new Random(System.currentTimeMillis())
              var randomInt = 0
              randomInt = seed.nextInt(10)
              
              randomInt%2 match {
                case 0 => {
                  senha = "123"
                }
                case 1 => {
                  senha = "345"
                }
              }
              
              enviaSenha!senha
              
              logado = retornoLogin? ;
              println("retornoLogin." + i + "."+logado)
            }
            
            for(x <- 0 until carrinho.length){
              println("========== CLIENTE " + i + " COMPROU " + carrinho(x)._1)
            }
            carrinho = List[(String,Int,Int)]()
          }
          case 1 => {
            println("removerItem." + i)
            reverteEstoqueBD!carrinho(0)._1
            carrinho = carrinho.tail
            if(carrinho.length == 0){
              println("carrinhoVazio." + i)
            }
          }
          case 2 => {
            println("continuarComprando." + i)
          }
          
        }
        
      } else {
        //se não houver itens no carrinho, só poderá continuar navegando.
      }
      
      
    }
  }
    
  def BDLOJA(consultaProdutoBD: ?[String], respostaPrecoBD: ![Int], respostaEstoqueBD: ![Int], separaEstoqueBD: ?[String], 
      respostaSeparaEstoqueBD: ![Boolean], reverteEstoqueBD: ?[String], autenticaUsuario: ?[String], enviaSenha: ?[String], retornoLogin: ![Boolean]) = proc {
    var produtoAcessado = ""
    while(true){
      alt ((true &&& consultaProdutoBD) =?=> { produto => CONSULTABD(produto, respostaPrecoBD, respostaEstoqueBD)() }
        | (true &&& separaEstoqueBD) =?=> { produto => SEPARAESTOQUEBD(produto, respostaSeparaEstoqueBD)() }
        | (true &&& reverteEstoqueBD) =?=> { produto => REVERTEESTOQUEBD(produto)() }
        | (true &&& autenticaUsuario) =?=> { usuario => AUTENTICAUSUARIO(usuario, enviaSenha, retornoLogin)() }
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
          var produto2 = produtosBD.tail.head
          //pegar o preco do produto acessado
          preco = produto2._2
          //pegar o estoque do produto acessado
          estoque = produto2._3

          if(estoque > 0){
            //atualizar estoque na tupla
            var itemAtualizado = (produto2._1, produto2._2, produto2._3 - 1)
            //atualizar lista com tupla modificada
            produtosBD = List(produto, itemAtualizado)
            
            //retornar separaEstoque OK!
            respostaSeparaEstoqueBD!true
            } else {
             //retornar separaEstoque False!
              respostaSeparaEstoqueBD!false
            }
        }
  }
  
    def REVERTEESTOQUEBD(produtoConsultado: String) = proc {
    
      var preco = 0
      var estoque = 0
        //pegar a tupla do produto acessado
        var produto = produtosBD.head
        if(produtoConsultado == produto._1){
          //pegar o preco do produto acessado
          preco = produto._2
          //pegar o estoque do produto acessado
          estoque = produto._3

            //atualizar estoque na tupla
            var itemAtualizado = (produto._1, produto._2, produto._3 + 1)
            //atualizar lista com tupla modificada
            produtosBD = itemAtualizado::produtosBD.tail
            
        } else {
          var produto2 = produtosBD.tail.head
          //pegar o preco do produto acessado
          preco = produto2._2
          //pegar o estoque do produto acessado
          estoque = produto2._3

            //atualizar estoque na tupla
            var itemAtualizado = (produto2._1, produto2._2, produto2._3 + 1)
            //atualizar lista com tupla modificada
            produtosBD = List(produto, itemAtualizado)
        }
  }
  
  def AUTENTICAUSUARIO(usuario: String, enviaSenha: ?[String], retornoLogin: ![Boolean]) = proc {
    var login = usuario 
    var senha = enviaSenha? ;
    
    //pega senha no mapa usuariosBD e compara com senha recebida
    if(senha == usuariosBD(usuario)){
      retornoLogin!true
    } else {
      retornoLogin!false
    }
  }  
    
  def ACESSOSCLIENTES(consultaProdutoBD: ![String], respostaPrecoBD: ?[Int], respostaEstoqueBD: ?[Int], separaEstoqueBD: ![String], 
      respostaSeparaEstoqueBD: ?[Boolean], reverteEstoqueBD: ![String], autenticaUsuario: ![String], enviaSenha: ![String], retornoLogin: ?[Boolean]) = proc {
    (|| (for (i <- 1 until 3) yield ACESSOCLIENTE(i,consultaProdutoBD, respostaPrecoBD, respostaEstoqueBD, separaEstoqueBD, respostaSeparaEstoqueBD, reverteEstoqueBD, autenticaUsuario, enviaSenha, retornoLogin)))()
  }
  
  def main(args: Array[String]) {
    val consultaProdutoBD = OneOne[String]
    val respostaPrecoBD = OneOne[Int]
    val respostaEstoqueBD = OneOne[Int]
    val separaEstoqueBD = OneOne[String]
    val respostaSeparaEstoqueBD = OneOne[Boolean]
    val reverteEstoqueBD = OneOne[String]
    val autenticaUsuario = OneOne[String]
    val enviaSenha = OneOne[String]
    val retornoLogin = OneOne[Boolean]
    
    val produtos = List("Smartphone", "TV")
    val produtosBD = List(("Smartphone", 100, 2), ("TV", 200, 1))

    (ACESSOSCLIENTES(consultaProdutoBD, respostaPrecoBD, respostaEstoqueBD, separaEstoqueBD, respostaSeparaEstoqueBD, reverteEstoqueBD, autenticaUsuario, enviaSenha, retornoLogin) || BDLOJA(consultaProdutoBD, respostaPrecoBD, respostaEstoqueBD, separaEstoqueBD, respostaSeparaEstoqueBD, reverteEstoqueBD, autenticaUsuario, enviaSenha, retornoLogin))()
    exit
  }
  
}



