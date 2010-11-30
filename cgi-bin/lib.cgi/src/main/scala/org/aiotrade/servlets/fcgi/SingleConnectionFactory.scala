/*
 * (c) 2009 Julien Rialland, and the jFastCGI project developpers.
 * 
 * Released under BSD License (see license.txt)
 *  
 *   $Id$ 
 */
package org.aiotrade.servlets.fcgi

import java.io.IOException
import java.net.InetAddress
import java.net.Socket
import java.util.regex.Pattern

/**
 * A connection factory that always tries to connect to the same ip/port.
 * 
 * @author jrialland
 *
 */
class SingleConnectionFactory(descriptor: String) extends ConnectionFactory {
  val log = ConnectionFactory.log

  val m = Pattern.compile("([^:]+):([1-9][0-9]*)$").matcher(descriptor.trim)

  val (host, port) = if (m.matches) {
    try {
      (InetAddress.getByName(m.group(1)), Integer.parseInt(m.group(2)))
    } catch {case ex: Exception => throw new IllegalArgumentException(ex)}
  } else {
    throw new IllegalArgumentException("syntax error (required format is <host>:<port>) - " + descriptor)
  }

  @throws(classOf[IOException])
  def getConnection: Option[Socket] = {
    createSocket match {
      case Left(socket) => if (socket != null) Some(socket) else None
      case Right(_) =>
        if (ConnectionFactory.startingCgiDaemon) {
          Thread.sleep(1000) // wait 1s to see if it's available again
          createSocket match {
            case Left(socket) => if (socket != null) Some(socket) else None
            case Right(ex) => throw ex
          }
        } else {
          startCgiDaemon
          Thread.sleep(1000)
          createSocket match {
            case Left(socket) => if (socket != null) Some(socket) else None
            case Right(ex) => throw ex
          }
        }

    }
  }

  private def createSocket: Either[Socket, Throwable] = {
    try {
      Left(new Socket(host, port))
    } catch {
      case ex: IOException => Right(ex)
    }
  }

  def releaseConnection(socket: Socket) {
    try {
      socket.close
    } catch {
      case ex: IOException =>
    }
  }

}
