/*
 * (c) 2009 Julien Rialland, and the jFastCGI project developpers.
 * 
 * Released under BSD License (see license.txt)
 *  
 *   $Id$ 
 */
package org.aiotrade.servlets.fcgi

import java.io.IOException

import java.util.logging.Logger
import javax.servlet.ServletConfig
import javax.servlet.ServletException
import javax.servlet.http.HttpServlet
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse

import scala.collection.immutable.TreeMap


/**
 * @author jrialland
 *
 */
object FastCGIServlet {
  val log = Logger.getLogger(classOf[FastCGIServlet].getName)

  /**
   * Address of the fastcgi provider service to use.
   */
  val PARAM_SERVER_ADDRESS = "serverAddress"
  /**
   * executable that should be launched as the servlet starts.
   */
  val PARAM_START_EXECUTABLE = "startExecutable"
  /**
   * user-provided class that will provide tcp connections.
   */
  val PARAM_CONNECTION_FACTORY = "connectionFactory"
  /**
   * comma-separated list of adresses when using seveal fastcgi endpoints.
   */
  val PARAM_CLUSTER_ADRESSES = "clusterAdresses"

  val PARAM_NAMES = Array(PARAM_SERVER_ADDRESS,
                          PARAM_START_EXECUTABLE,
                          PARAM_CONNECTION_FACTORY,
                          PARAM_CLUSTER_ADRESSES)

  def createGateway(config: Map[String, String]): FastCGIGateway = {
    val gateway = new FastCGIGateway

    if (config.get(PARAM_SERVER_ADDRESS).isDefined) {

      log.info("configuring fastCGI handler using a single connection -based policy")
      gateway.connectionFactory = new SingleConnectionFactory(config.get(PARAM_SERVER_ADDRESS).get)

    } else if (config.get(PARAM_CONNECTION_FACTORY).isDefined) {

      val className = config.get(PARAM_CONNECTION_FACTORY).get.trim
      log.info("configuring fastCGI handler using custom class '" + className + "'")
      gateway.connectionFactory = ConnectionFactory.buildConnectionFactoryForClass(className)

    } else if (config.get(PARAM_CLUSTER_ADRESSES).isDefined) {

      val factory = new PoolFactory
      log.info("configuring fastCGI handler using the following adresses : ")
      config.get(PARAM_CLUSTER_ADRESSES).get.split(";") foreach {addr =>
        log.info("  => " + addr)
        factory.addAddress(addr.trim)
      }
      gateway.connectionFactory = new PooledConnectionFactory(factory)

    } else {

      throw new IllegalArgumentException("Cannot create fcgi handler : did you provide any configuration ?")

    }

    log.info("cgiDaemonCmd is " + config.get(PARAM_START_EXECUTABLE))

    gateway.connectionFactory.cgiDaemonCmd = config.get(PARAM_START_EXECUTABLE)

    gateway
  }

}


@serializable
@SerialVersionUID(8597795652806478718L)
class FastCGIServlet extends HttpServlet {
  import FastCGIServlet._

  private var gateway: FastCGIGateway = _

  @throws(classOf[ServletException])
  override def init(servletConfig: ServletConfig) {
    super.init(servletConfig)
    var config = new TreeMap[String, String]
    for (paramName <- PARAM_NAMES) {
      val value = getInitParameter(paramName)
      if (value != null) {
        config += (paramName -> value)
      }
    }
    gateway = createGateway(config)
  }

  @throws(classOf[ServletException])
  @throws(classOf[IOException])
  override protected def service(request: HttpServletRequest, response: HttpServletResponse) {
    val cgiEnv = new FastCGIEnv(request, getServletContext)
    cgiEnv.setEnv
    gateway.service(cgiEnv, request, response)
  }

  override def destroy {
    super.destroy
    gateway.destroy
  }
}
