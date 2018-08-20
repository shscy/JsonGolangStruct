package com.zxl.message
import java.util

import io.netty.buffer.{ByteBuf, Unpooled}
import io.netty.channel.ChannelHandlerContext
import io.netty.handler.codec.ByteToMessageDecoder

sealed abstract class Expr

// Reader 表示用来读取数据,参考golang的设计
trait Reader {def Read:String}

// 消息头部
case class LiveMessageHeader[T, R](MessageType:T, Length:R){}

// 消息体
case class LiveMessageContent[T, R ,Z <: Reader](MessageType:T, Length: R, Content:Z){}


object Message {
  implicit  def StringToReader(str:String):Reader = {
    val buff = Unpooled.buffer()
    buff.writeBytes(str.getBytes())
    new Reader {
      override def Read:String = {
        buff.toString()
      }
    }
  }

  implicit def ArrayByteToReader(str: Array[Byte]):Reader = {
    val buff = Unpooled.buffer()
    buff.writeBytes(str)
    new Reader {
      override def Read:String = {
        buff.toString()
      }
    }
  }
}


class MessageDecoder extends ByteToMessageDecoder{
  override def decode(ctx: ChannelHandlerContext, in: ByteBuf, out: util.List[AnyRef]): Unit = {

  }
}

