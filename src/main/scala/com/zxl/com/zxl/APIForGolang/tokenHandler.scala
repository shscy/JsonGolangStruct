package com.zxl.com.zxl.APIForGolang
import com.zxl.com.zxl.APIForGolang._

trait tokenHandler {
  val input:TokenInput
  @throws[Exception]("parser error")
  def parser(): Unit = {
    var token = input.nextToken()
    val obj = parserObject()

  }
  def parseStm():JsonStm[SymbolType] = {
    //println("once")
    println("\n\ndebuyg", input.buffer.substring(input.Index), input.Index)
    val token = input.nextToken()
    //println("smtttt: ", to)
    val res = token match{

      case Token("}", _, _)=>{
        null
      }
      case Token(e, _, _)=>{
        assert(e.isInstanceOf[String])
        val key = e
        val token2 = input.nextToken()
        println("K ", key, token2.content)
        //assert(token2.content==":"
        val value:JsonStm[SymbolType]= if(input.buffer.charAt(input.Index) == '{'){
          JsonStm(e.toString, parserObject())
        } else{
          val nextToken = input.nextToken()
          JsonStm(e.toString, nextToken match {
            case Token(x:String, _, _)=>JsonValueNormal(x)
            case Token(x:Int,_, _)=>JsonValueNormalInt(x)
            case _ => throw  new Exception("fffff" + nextToken.String())
          })
        }

        // skip the
        if(input.Index + 1 < input.length() && input.buffer.charAt(input.Index) == ',') {
          val sk = input.nextToken()
          println("skip: ", sk.content)
        }
        value
      }
    }
    println("debuyg2222", input.buffer.substring(input.Index), input.Index)
    res
  }

  def parserObject()= {
    println("once1111")
    val befToken = input.nextToken()
    assert(befToken.content == "{")
    var token = input.nextToken2(false)
    val t = token match {
      case Token("}", _, _) => {
        JsonValueObject()
      }
        // 遍历节点下的元素
      case _ => {
        var valueSeq = List[JsonStm[SymbolType]]()
        //e.getClass
        var tmp = parseStm()
        while (tmp != null) {
         // println("ffgggg")
          valueSeq = valueSeq.+:(tmp)
          tmp = parseStm()
        }
        //println("vaue: stmp ", valueSeq)
        JsonValueObject(valueSeq: _*)
      }
    }
    t
  }
}

object Run extends App{
  val sc  = new tokenHandler{
    override val input: TokenInput = new TokenInput {
      var in2 = """{"aa":"vv","bbb":1223,"ccK1":{"ccK2":{"eeK":"eeV"}}}"""
      override val buffer: String = in2
      println(buffer + "zzz")

    }
  }
  val res = sc.parserObject()
  //for (s <- res.symbol){
  //  println("smt: ", s.key, s.Value)
  //}

  val obj2 = new ASTAnalyze(res, "TestTable", "table", Seq[String]("fmt"))
  val  z =obj2.Parser()
  println("\n\n--\n\n")

//  z.express.foreach(s=>{
//    s match {
//      case e:GolangExpress[_] =>{
//        println(e.Key, "<>", e.ValueObject)
//      }
//    }
//  })
  //println("\n\n--\n\n")

  val result = new Result {
    override val obj: GolangObject = z
    override val fileName: String = "zzz"
  }
  result.SavetoFile()
}