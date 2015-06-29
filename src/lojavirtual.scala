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
    var repositorioCartoes= List((1111111, "Joao", 500), (2222222,"Maria", 100))

  
  //respostaProdutoBD dividido em respostaPrecoBD e respostaEstoqueBD.
  def ACESSOCLIENTE(i: Int, consultaProdutoBD: ![String], respostaPrecoBD: ?[Int], respostaEstoqueBD: ?[Int], separaEstoqueBD: ![String], 
                      respostaSeparaEstoqueBD: ?[Boolean], reverteEstoqueBD: ![String], autenticaUsuario: ![String], enviaSenha: ![String], 
                      retornoLogin: ?[Boolean], handshakeOperadora: ![(Int,String,Int)], confirmaTransacaoCredito: ?[Boolean] ) = proc {
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
            //calculando o total da compra para concluir
            var total = 0
            for(x <- 0 until carrinho.length){
              total = total + carrinho(x)._2
            }
            
            println("concluirCompra." + i + "." + total)
            
            var logado = false
            //convencionei acesso 1 = joao e acesso 2 = maria
            
            while(!logado){
              println("autenticaUsuario." + i + "." + usuarios(i-1))
              autenticaUsuario!usuarios(i-1)
              
              //randomizando para enviar senha
              var senha = ""
              var seed = new Random(System.currentTimeMillis())
              Thread.sleep(200)
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
            
            println("escolherFormaDePagamento." + i + "." + total)
            var pagamentoRealizado = false
            
            while(!pagamentoRealizado){
              //randomizando escolha de forma de pagamento
              var seed = new Random(System.currentTimeMillis())
              var randomInt = 0
              randomInt = seed.nextInt(10)
              
          
              randomInt%2 match {
                case 0 => {
                  println("pagamentoDebitoOnline")  
                }
                case 1 => {
                  println("pagamentoCartaoCredito." + i + "."+ total)
                  var cartao = 0
                  var nome = ""
                  i match {
                    case 1 => {
                      cartao = 1111111
                      nome = "Joao"
                    }
                    case 2 => {
                      cartao = 2222222
                      nome = "Maria"
                    }
                  }
                  println("handshakeOperadora." + i + "." + "(" + cartao + ", " + nome + ", " + total +")")
                  handshakeOperadora!(cartao, nome, total)
                  
                  var retorno = confirmaTransacaoCredito? ;
                  
                  if(retorno == true){ 
                    println("pagou com sucesso!!!")
                    pagamentoRealizado = true
                  } else {
                    println("nao pagou com sucesso")
                  }
                }
              }
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
      respostaSeparaEstoqueBD: ?[Boolean], reverteEstoqueBD: ![String], autenticaUsuario: ![String], enviaSenha: ![String],
      retornoLogin: ?[Boolean], handshakeOperadora: ![(Int, String, Int)], confirmaTransacaoCredito: ?[Boolean]) = proc {
    (|| (for (i <- 1 until 3) yield ACESSOCLIENTE(i,consultaProdutoBD, respostaPrecoBD, respostaEstoqueBD, separaEstoqueBD, 
                                      respostaSeparaEstoqueBD, reverteEstoqueBD, autenticaUsuario, enviaSenha, retornoLogin,
                                      handshakeOperadora, confirmaTransacaoCredito)))()
  }
  
  def OPERADORACARTAO(handshakeOperadora: ?[(Int, String, Int)], confirmaTransacaoCredito: ![Boolean]) = proc {
    while(true){
      alt ((true &&& handshakeOperadora) =?=> { dadosCartao => PAGAMENTOCREDITO(dadosCartao, confirmaTransacaoCredito)()  } )
    }
  }
  
  def PAGAMENTOCREDITO(dadosCartao: (Int, String, Int), confirmaTransacaoCredito: ![Boolean]) = proc {
    
      var numCartao = dadosCartao._1
      var nomeCartao = dadosCartao._2
      var total = dadosCartao._3
      
      //localiza o cartao no repositorio
        var registro = repositorioCartoes.head        
        if(registro._1 == numCartao){
          if(registro._2 == nomeCartao) {
            //checa saldo
            if(registro._3 >= total){
              //autoriza
               repositorioCartoes = (numCartao, nomeCartao, registro._3 - total)::repositorioCartoes.tail
               confirmaTransacaoCredito!true
            } else { 
              //nao autoriza
               confirmaTransacaoCredito!false
            }
          } 
        } else { 
          var registro2 = repositorioCartoes.tail.head
            if(registro2._2 == nomeCartao) {
            //checa saldo
              if(registro2._3 >= total){
              //autoriza
                 repositorioCartoes = List(registro, (numCartao, nomeCartao, registro2._3 - total))
                 confirmaTransacaoCredito!true
              } else { 
                //nao autoriza
                 confirmaTransacaoCredito!false
              }
            }
        }
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
    val handshakeOperadora = OneOne[(Int, String, Int)]
    val confirmaTransacaoCredito = OneOne[Boolean]
    
    val produtos = List("Smartphone", "TV")
    val produtosBD = List(("Smartphone", 100, 2), ("TV", 200, 1))

    (ACESSOSCLIENTES(consultaProdutoBD, respostaPrecoBD, respostaEstoqueBD, separaEstoqueBD, 
        respostaSeparaEstoqueBD, reverteEstoqueBD, autenticaUsuario, enviaSenha, retornoLogin, handshakeOperadora, confirmaTransacaoCredito) 
     || BDLOJA(consultaProdutoBD, respostaPrecoBD, respostaEstoqueBD, separaEstoqueBD, 
        respostaSeparaEstoqueBD, reverteEstoqueBD, autenticaUsuario, enviaSenha, retornoLogin)
     || OPERADORACARTAO(handshakeOperadora, confirmaTransacaoCredito))()
    exit
  }
  
}



