package com.zxl.com.zxl.APIForGolang
import java.io.PrintWriter
import java.io.File

sealed abstract class targetObject{}

case class GolangExpress[T](val Key:String, val ValueObject:T) extends targetObject{}
case class PackageDeclare(packageName:String) extends targetObject{}
case class PackageImport(packages:Seq[String]) extends targetObject{}


trait Operator {
  def Field(k:String) ={
    k.charAt(0).toUpper.toString + k.substring(1)
  }
  def Tag(bb:String)={
    val k:String = bb
    // example `json:"key"`
    val s = "`json:\"" + k+ "\"`"
    s
  }
  def ObjectName(k:String)={
    k + "$Struct"
  }
}

class ObjectBuffer(f:String) {
  var buffer = ""
  def append(t:String)={
    buffer = buffer + t
  }
}

trait GolangObject extends Operator{
  val StructName:String
  var express:List[targetObject]
}

trait Golang {
  val importPackage:List[String]
  def Golangimport={
  }
}

class ASTAnalyze(rootAst:JsonValueObject[_ <: SymbolType], rootStructName:String,
                 PackageName:String, packageImports:Seq[String]){

  //val rootName = "RootObject"
  //override val importPackage = List("table", "print")

  val rootObj:GolangObject = {
    new GolangObject {
      override val StructName: String = rootStructName
      override var express: List[targetObject] = List()
    }
  }

  def Parser()={
    rootObj.express = rootObj.express :+ new PackageDeclare(PackageName)
    rootObj.express = rootObj.express :+ new PackageImport(packageImports)
    _parser(rootObj, rootAst)
    rootObj
  }

  def _parser(root:GolangObject, ast:JsonValueObject[_ <: SymbolType]): Unit = {
    ast.symbol.foreach {
      case JsonStm(key, e:JsonValueNormal) => {
        root.express = root.express :+ new GolangExpress[String](key, e.Value)
      }
      case JsonStm(key, e:JsonValueNormalInt) => {
        root.express = root.express :+ new GolangExpress[Int](key, e.Value)
      }
      case JsonStm(key, e:JsonValueObject[_]) => {
        val obj = new GolangExpress[GolangObject](key,new GolangObject {
          override val StructName: String = ObjectName(key)
          override var express: List[targetObject] = List()
        })
        _parser(obj.ValueObject, e)
        root.express = root.express :+ obj
      }
      case _=>{
        throw new Exception("exception")
      }
    }
  }
}

trait Result extends Operator{
  val FormatLength = 4
  val obj:GolangObject
  val fileName:String
  implicit  val objBuffer:ObjectBuffer = {
    new ObjectBuffer(fileName)
  }

  def SavetoFile() = {
    val str = generateGolangObject(obj, objBuffer)
    val befStr = objBuffer.buffer
    val writer = new PrintWriter(new File(fileName + ".go"))

    writer.write(str._1)
    writer.write(str._2)
    writer.write(str._3)
    writer.write(befStr)
    writer.flush()
    writer.close()
  }

  implicit def SringWrite(s:String) = new {
    def write(in:String)={
      s + in
    }
  }

  def formatExpress(key:String, unit:String) = {
    " "*FormatLength + Field(key) + " " +   unit + " " + Tag(key) + "\n"
  }

  def formatDeclare(name:String)={
    val dec = "type" + " " + name + " " + " struct " + "{\n"
    val comment = "//" + name + " this is the Comment auto generate\n"
    comment + dec
  }

  def formatOverDeclare()={
    "}\n\n"
  }
  def packageDeclare(packageName:String)={
    "package "+packageName + "\n"
  }

  def packageImport(packages:Seq[String])={
    val st = "import (" + "\n"
    val content = packages.map(str=>{
      " "*FormatLength +  "\"" + str + "\"" + "\n"
    })
    val end = ")\n\n"
    st + content.mkString + end
  }

  def generateGolangObject(obj:GolangObject, ObjectList:ObjectBuffer):(String, String, String) ={

    println("statt: ", obj.StructName)
    var res = formatDeclare(obj.StructName) //wrtie type struct
    var Dec = ""
    var Packages = ""
    obj.express.foreach{
      case GolangExpress(key, value:Int) =>{
        res += formatExpress(key, "int")
      }
      case GolangExpress(key, value:String)=>{
        res += formatExpress(key, "string")
      }
      case GolangExpress(key, value:GolangObject)=>{
        println("value struc name: ", value.StructName)
        res += formatExpress(key, value.StructName)
        val res1 = (generateGolangObject(value, ObjectList))
        ObjectList append res1._3
      }
      case e:PackageDeclare=>{
        println("package name: ", e.packageName)
        Dec = packageDeclare(e.packageName)
      }
      case e:PackageImport=>{
        Packages = packageImport(e.packages)
      }
      case _=>{
        throw new Exception("nooo")
      }
    }
    res += formatOverDeclare()
    (Dec, Packages, res)
  }
}
