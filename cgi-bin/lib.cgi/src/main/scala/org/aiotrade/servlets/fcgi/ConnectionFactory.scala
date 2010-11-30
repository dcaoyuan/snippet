/*
 * (c) 2009 Julien Rialland, and the jFastCGI project developpers.
 * 
 * Released under BSD License (see license.txt)
 *  
 *   $Id$ 
 */
package org.aiotrade.servlets.fcgi

import java.io.IOException
import java.net.Socket
import java.util.logging.Logger
import org.aiotrade.servlets.util.StreamLogger

/**
 * interface that any service that can create / destroy connections to a fastcgi provider should implement.
 * 
 * @author jrialland
 *
 */
object ConnectionFactory {
  
  val log = Logger.getLogger(classOf[ConnectionFactory].getName)

  def buildConnectionFactoryForClass(className: String): ConnectionFactory = {
    try {
      return Class.forName(className).newInstance.asInstanceOf[ConnectionFactory]
    } catch {case ex: Exception => throw new RuntimeException(ex)}
  }

  var startingCgiDaemon = false
}

trait ConnectionFactory {
  import ConnectionFactory._

  var cgiDaemonCmd: Option[String] = None
  var procLogThread: Thread = _

  @throws(classOf[IOException])
  def startCgiDaemon {
    if (startingCgiDaemon) {
      return
    }
    
    cgiDaemonCmd foreach {x =>
      startingCgiDaemon = true

      val pb = new ProcessBuilder(x.split(" "): _*)
      val proc = pb.start
      log.info(x + " is started")

      if (procLogThread != null) {

      }
      procLogThread = new Thread(new StreamLogger(proc.getErrorStream, log))
      procLogThread.setDaemon(true)
      procLogThread.start
      
      startingCgiDaemon = false
    }
  }

  /**
   * Called when a connection is needed.
   *
   * @return
   */
  @throws(classOf[IOException])
  def getConnection: Option[Socket]

  /**
   * Called  when a connection is released (not needed anymore)
   *
   * Note : it doesn't mean that the socket should be closed at all, but notifies that this connection is no more
   * needed for a particular request.
   * For example, a pooling system could use this method to mark connection as "useable" for another request.
   *
   * @param socket
   */
  def releaseConnection(socket: Socket)
}
