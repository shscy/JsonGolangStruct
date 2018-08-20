package com.zxl.com.zxl.APIForGolang
import java.io.EOFException

import scala.util.matching.Regex

// 使用递归下降的方式解析json 来生成 模板类 Golang 减少自定义type struct


sealed abstract class SymbolType

case class JsonStm[T <: SymbolType](key:String, Value:T)

case class JsonValueNormal(Value:String) extends SymbolType
case class JsonValueNormalInt(Value:Int) extends SymbolType

case class JsonValueObject[T <: SymbolType](symbol: JsonStm[T]*) extends SymbolType


case class Token[+T](content:T, St:Int, End:Int) {
  def String():String={
    return content.toString
  }
}

object Parser {
  val TokenStateKey = 0
  val tokenStateValue = 1

}

trait TokenInput {
  val buffer:String

  def length()={
    buffer.length
  }

  var Index:Int = 0
  var currentState = Parser.TokenStateKey
  var CToken:Token[Any] = Token[Any]("init", 0,0)

  implicit def CharObject(ch:Char) = new {
    def IsNumber:Boolean = {
      ch == '0' || ch == '1' ||ch == '2' ||ch == '3' ||ch == '4' ||ch == '5' ||ch == '6' ||ch == '7' ||ch == '8' || ch == '9'
    }
  }
  implicit def StringFindNumber(in:String) = new {
    @throws[Exception]("no not find number")
    def findNumber(st:Int):(Int, Int) = {

      val pattern = new Regex("[0-9]{1,}")
      val res = pattern findFirstIn in.substring(st)
      println("数字啊:  ", in.substring(st))
      if(!res.isEmpty) (res.get.toInt, res.get.length) else throw  new Exception("not found number")
     }
  }
  def nextToken2(IsIndexAdd:Boolean):Token[Any]={
   if (Index >= length()) {
      return Token[EOFException](new EOFException(""), 0, 0)
    }
    val char:Char = buffer.charAt(Index)
    //println("each char: ", char)
    val token = char match {
      case '{' => {
        val t = Token[String]("{", Index, Index + 1)

        t
      }
      case '\"' => {

        val idEnd = buffer.substring(Index + 1).indexOf("\"")
        val id = buffer.substring(Index+1, Index + 1 + idEnd)
        val t = Token[String](id, Index, idEnd)
        //println("\nb\n debug ",id, Index, idEnd, buffer.substring(Index+1))
        //Index= Index + 1 + idEnd + 1
        t
      }
      case ':' => {
        val t = Token[String](":", Index, Index +1)
        //Index += 1
        t
      }
      case ',' => {
        val t = Token[String](",", Index, Index +1)
        //Index += 1
        t
      }
      case '}' =>{
        val t = Token[String]("}", Index, Index + 1)
        //Index += 1
        t
      }
      case w:Char if w.IsNumber => {
        val id = buffer.findNumber(Index)
        val t = Token[Int](id._1, Index, Index + id._2)
        //Index = Index + id._2
        t
      }
      case e =>{
        throw  new Exception("unsupport token type ] " + e.toString)
      }
    }
    CToken = token
    token
  }

  def nextToken():Token[Any] = {
    if (Index >= length()) {
      return Token[EOFException](new EOFException(""), 0, 0)
    }
    val char:Char = buffer.charAt(Index)
    //println("each char: ", char)
    val token = char match {
      case '{' => {
        val t = Token[String]("{", Index, Index + 1)
        Index += 1
        t
      }
      case '\"' => {

        val idEnd = buffer.substring(Index + 1).indexOf("\"")
        val id = buffer.substring(Index+1, Index + 1 + idEnd)
        val t = Token[String](id, Index, idEnd)
        //println("\nb\n debug ",id, Index, idEnd, buffer.substring(Index+1))
        Index= Index + 1 + idEnd + 1
        t
      }
      case ':' => {
        val t = Token[String](":", Index, Index +1)
        Index += 1
        t
      }
      case ',' => {
        val t = Token[String](",", Index, Index +1)
        Index += 1
        t
      }
      case '}' =>{
        val t = Token[String]("}", Index, Index + 1)
        Index += 1
        t
      }
      case w:Char if w.IsNumber => {
        val id = buffer.findNumber(Index)
        val t = Token[Int](id._1, Index, Index + id._2)
        Index = Index + id._2
        t
      }
      case e =>{
        throw  new Exception("unsupport token type ] " + e.toString)
      }
    }
    CToken = token
    if (Index < length()) {
      println("each token: ", token.content, "<-:: ->", buffer.charAt(Index).toString)
    }
    token
  }
}
