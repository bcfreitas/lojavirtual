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
    var repositorioContas= List((333333, "Joao", 321, 100), (444444, "Maria", 654, 300))

  
  //respostaProdutoBD dividido em respostaPrecoBD e respostaEstoqueBD.
  def ACESSOCLIENTE(i: Int, consultaProdutoBD: ![String], respostaPrecoBD: ?[Int], respostaEstoqueBD: ?[Int], separaEstoqueBD: ![String], 
                      respostaSeparaEstoqueBD: ?[Boolean], reverteEstoqueBD: ![String], autenticaUsuario: ![String], enviaSenha: ![String], 
                      retornoLogin: ?[Boolean], handshakeOperadora: ![(Int,String,Int)], confirmaTransacaoCredito: ?[Boolean],
                      handshakeBanco: ![(String,Int)], confirmaTransacaoDebito: ?[Boolean]) = proc {
    println("acessoCliente." + i)
    //o sleep abaixo é pra garantir seeds diferentes da randomização
    Thread.sleep(i * 500)
    
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
      Thread.sleep(200)
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
               println("produtoAdicionadoCarrinho." + i + "." + produtoAcessado)
               carrinho = (produtoAcessado, preco, 1)::carrinho
              }
          case false => println("estoqueIndisponível." + i)
        }
      } else { 
        println("estoqueIndisponivel." + i)
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
                  println("pagamentoDebitoOnline." + i)
                  //no debito online o usuario acessa o site do banco
                  println("handshakeBanco." + i + "." + total)
                  handshakeBanco!(usuarios(i-1), total)
                  
                  var retorno = confirmaTransacaoDebito? ;
                  
                  if(retorno == true){ 
                    println("confirmaTransacaoDebito." + i + ".True")
                    pagamentoRealizado = true
                  } else {
                    println("confirmaTransacaoDebito." + i + ".False")
                  }
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
                    println("confirmaTransacaoCredito." + i + ".True")
                    pagamentoRealizado = true
                  } else {
                    println("confirmaTransacaoCredito." + i + ".False")
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
      
        //pegar a tupla do produto acessado
        var produto = produtosBD.head
        if(produtoConsultado == produto._1){
          //pegar o preco do produto acessado
          preco = produto._2
          //pegar o estoque do produto acessado
          estoque = produto._3
        } else {
          var produto2 = produtosBD.tail.head
          //pegar o preco do produto acessado
          preco = produto2._2
          //pegar o estoque do produto acessado
          estoque = produto2._3
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
      retornoLogin: ?[Boolean], handshakeOperadora: ![(Int, String, Int)], confirmaTransacaoCredito: ?[Boolean],
      handshakeBanco: ![(String, Int)], confirmaTransacaoDebito: ?[Boolean]) = proc {
    (|| (for (i <- 1 until 3) yield ACESSOCLIENTE(i,consultaProdutoBD, respostaPrecoBD, respostaEstoqueBD, separaEstoqueBD, 
                                      respostaSeparaEstoqueBD, reverteEstoqueBD, autenticaUsuario, enviaSenha, retornoLogin,
                                      handshakeOperadora, confirmaTransacaoCredito, handshakeBanco, confirmaTransacaoDebito)))()
  }
  
  def OPERADORACARTAO(handshakeOperadora: ?[(Int, String, Int)], confirmaTransacaoCredito: ![Boolean]) = proc {
    while(true){
      alt ((true &&& handshakeOperadora) =?=> { dadosCartao => PAGAMENTOCREDITO(dadosCartao, confirmaTransacaoCredito)()  } )
    }
  }
  
  def BANCO(handshakeBanco: ?[(String,Int)], confirmaTransacaoDebito: ![Boolean]) = proc { 
    while(true){
      alt ((true &&& handshakeBanco) =?=> { dados => PAGAMENTODEBITO(dados, confirmaTransacaoDebito)()  } )
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

  def PAGAMENTODEBITO(dados: (String, Int), confirmaTransacaoDebito: ![Boolean]) = proc {
      
      var nome = dados._1
      var total = dados._2
      var conta = 0
      var senha = 0
      
      //o usuario esta acessando o site do banco para pagar
      if(nome=="Joao"){
        conta = 333333
        senha = 321
      } else {
        conta = 444444
        senha = 654
      }
      
      println("transacaoDebitoOnline." + total + "." + conta + "." + nome + "." + senha)
      
      //localiza o cartao no repositorio
        var registro = repositorioContas.head        
        if(registro._1 == conta){
          if(registro._3 == senha) {
            //checa saldo
            if(registro._4 >= total){
              //autoriza
               repositorioContas = (conta, nome, senha, registro._4 - total)::repositorioContas.tail
               confirmaTransacaoDebito!true
            } else { 
              //nao autoriza
               confirmaTransacaoDebito!false
            }
          } 
        } else { 
          var registro2 = repositorioContas.tail.head
            if(registro2._3 == senha) {
            //checa saldo
              if(registro2._4 >= total){
              //autoriza
                 repositorioContas = List(registro, (conta, nome, senha, registro2._4 - total))
                 confirmaTransacaoDebito!true
              } else { 
                //nao autoriza
                 confirmaTransacaoDebito!false
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
    val handshakeBanco = OneOne[(String, Int)]
    val confirmaTransacaoDebito = OneOne[Boolean]
    
    (ACESSOSCLIENTES(consultaProdutoBD, respostaPrecoBD, respostaEstoqueBD, separaEstoqueBD, 
        respostaSeparaEstoqueBD, reverteEstoqueBD, autenticaUsuario, enviaSenha, retornoLogin, 
        handshakeOperadora, confirmaTransacaoCredito, handshakeBanco, confirmaTransacaoDebito) 
     || BDLOJA(consultaProdutoBD, respostaPrecoBD, respostaEstoqueBD, separaEstoqueBD, 
        respostaSeparaEstoqueBD, reverteEstoqueBD, autenticaUsuario, enviaSenha, retornoLogin)
     || OPERADORACARTAO(handshakeOperadora, confirmaTransacaoCredito)
     || BANCO(handshakeBanco, confirmaTransacaoDebito)
    )()
    exit
  }
  
}



