/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.aiotrade.snippets.threadring

import java.io.IOException
import org.eclipse.jetty.client.ContentExchange
import org.eclipse.jetty.client.HttpClient
import org.eclipse.jetty.client.HttpExchange
import org.eclipse.jetty.io.Buffer
import org.eclipse.jetty.util.thread.QueuedThreadPool

object HttpConcurrentClients {

  val url = "http://www.aiotrade.org"

  def main(args: Array[String]) {
    startClient
  }

  def startClient = {
    val client = new HttpClient
    client.setConnectorType(HttpClient.CONNECTOR_SELECT_CHANNEL)
    client.setThreadPool(new QueuedThreadPool(20)) // max 250 threads
    client.setTimeout(30000) // 30 seconds timeout; if no server reply, the request expires
    try {
      client.start
    } catch {case e: Exception => e.printStackTrace}

    
    val exchange = createExchange //new ContentExchange
    exchange.setMethod("GET")
    exchange.setURL(url)

    client.send(exchange)
    
    exchange.waitForDone match {
      case HttpExchange.STATUS_COMPLETED =>
        println("Response status: " + exchange.getResponseStatus)
      case HttpExchange.STATUS_EXCEPTED =>
        println("Response status excepted.")
      case HttpExchange.STATUS_EXPIRED =>
        println("Response expired.")
    }
  }

  // @Note: need more exploring
  def createExchange = {
    new ContentExchange {
      var t = System.currentTimeMillis

      @throws(classOf[IOException])
      override def onRequestCommitted {
        t = System.currentTimeMillis
        super.onRequestCommitted
      }

      @throws(classOf[IOException])
      override def onRequestComplete {
        super.onRequestComplete
      }

      @throws(classOf[IOException])
      override def onResponseComplete {
        super.onResponseComplete
        println("response time: " + (System.currentTimeMillis - t) / 1000.0)
        val content = this.getResponseContent
        println(content)
      }

      @throws(classOf[IOException])
      override def onResponseContent(content: Buffer) {
        super.onResponseContent(content)
      }

      @throws(classOf[IOException])
      override def onResponseHeaderComplete {
        super.onResponseHeaderComplete
      }

      @throws(classOf[IOException])
      override protected def onResponseStatus(version: Buffer, status: Int, reason: Buffer) {
        super.onResponseStatus(version, status, reason)
      }

      @throws(classOf[IOException])
      override protected def onResponseHeader(name: Buffer, value: Buffer) {
        super.onResponseHeader(name, value)
      }

      override protected def onConnectionFailed(ex: Throwable) {
        super.onConnectionFailed(ex)
      }

      override protected def onException(ex: Throwable) {
        super.onException(ex)
      }

      override protected def onExpire {
        super.onExpire
      }
    }
  }
}
