package com.zxl

import io.netty.bootstrap.ServerBootstrap
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.{ChannelHandlerContext, ChannelInitializer, ChannelOption, SimpleChannelInboundHandler}
import io.netty.handler.codec.http._
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioServerSocketChannel
import io.netty.handler.codec.http.HttpObjectAggregator
import io.netty.handler.codec.http.HttpRequestDecoder
import io.netty.handler.codec.http.HttpResponseEncoder
import io.netty.buffer.Unpooled
import io.netty.util.AsciiString


object NettyServer extends App {
  val server = new Server {
    override val Port: Int = 8899
  }
  server.start()
}

trait ServerConfig {
  val Port:Int
}

class HttpHandler() extends SimpleChannelInboundHandler[FullHttpRequest]{
  private  val contentType = HttpHeaderValues.TEXT_PLAIN;
  @throws[Exception]
  override def channelRead0(ctx: ChannelHandlerContext, msg: FullHttpRequest): Unit = {
    // 这里就是 web server 需要做的工作 配合 akka 实现并发处理

    println("class:" + msg.getMethod)
    msg.content()
    val response = new DefaultFullHttpResponse(HttpVersion.HTTP_1_1,
      HttpResponseStatus.OK,
      Unpooled.wrappedBuffer("test".getBytes())); // 2

    val heads = response.headers();
    heads.add(HttpHeaderNames.CONTENT_TYPE, contentType + "; charset=UTF-8");
    heads.add(HttpHeaderNames.CONTENT_LENGTH, response.content().readableBytes()); // 3
    heads.add(HttpHeaderNames.CONNECTION, HttpHeaderValues.KEEP_ALIVE);
    ctx.writeAndFlush(response)
  }

//  override def channelReadComplete(ctx: ChannelHandlerContext): Unit = {
//    println("channelReadComplete");
//    super.channelReadComplete(ctx)
//    ctx.flush()
//
//  }

  override def exceptionCaught(ctx:ChannelHandlerContext , cause:Throwable) ={
    System.out.println("exceptionCaught");
    if(null != cause) cause.printStackTrace();
    if(null != ctx) ctx.close();
  }


}

trait Server extends ServerConfig{
  def start()= {
    val boot = new ServerBootstrap()
    val group = new NioEventLoopGroup()
    boot.group(group)
      .channel(classOf[NioServerSocketChannel])
      .childHandler(
        new ChannelInitializer[SocketChannel] {
          @throws[Exception]("handle each request")
          override def initChannel(ch: SocketChannel): Unit = {
            ch.pipeline()
              .addLast("decoder", new HttpRequestDecoder)
              .addLast("encoder", new HttpResponseEncoder)
              .addLast("aggregator", new HttpObjectAggregator(512 * 1024))
              .addLast("handler", new HttpHandler())
          }
        })
      .option(ChannelOption.SO_BACKLOG, int2Integer(2))
      .childOption(ChannelOption.SO_KEEPALIVE, boolean2Boolean(true))
    boot.bind(Port).sync()
  }
}
