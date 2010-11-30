// ========================================================================
// Copyright (c) 2006-2009 Mort Bay Consulting Pty. Ltd.
// ------------------------------------------------------------------------
// All rights reserved. This program and the accompanying materials
// are made available under the terms of the Eclipse Public License v1.0
// and Apache License v2.0 which accompanies this distribution.
// The Eclipse Public License is available at
// http://www.eclipse.org/legal/epl-v10.html
// The Apache License v2.0 is available at
// http://www.opensource.org/licenses/apache2.0.php
// You may elect to redistribute this code under either of these licenses.
// ========================================================================

package org.aiotrade.servlets


import java.io.IOException
import java.net.InetSocketAddress
import java.net.MalformedURLException
import java.net.Socket

import javax.servlet.Servlet
import javax.servlet.ServletConfig
import javax.servlet.ServletContext
import javax.servlet.ServletException
import javax.servlet.ServletRequest
import javax.servlet.ServletResponse
import javax.servlet.UnavailableException
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse

import org.eclipse.jetty.client.HttpClient
import org.eclipse.jetty.client.HttpExchange
import org.eclipse.jetty.continuation.ContinuationSupport
import org.eclipse.jetty.http.HttpHeaderValues
import org.eclipse.jetty.http.HttpHeaders
import org.eclipse.jetty.http.HttpSchemes
import org.eclipse.jetty.http.HttpURI
import org.eclipse.jetty.io.Buffer
import org.eclipse.jetty.io.EofException
import org.eclipse.jetty.util.IO
import org.eclipse.jetty.util.TypeUtil
import org.eclipse.jetty.util.log.Log
import org.eclipse.jetty.util.log.Logger
import org.eclipse.jetty.util.thread.QueuedThreadPool


/**
 * Asynchronous Proxy Servlet.
 *
 * Forward requests to another server either as a standard web proxy (as defined by
 * RFC2616) or as a transparent proxy.
 * <p>
 * This servlet needs the jetty-util and jetty-client classes to be available to
 * the web application.
 * <p>
 * To facilitate JMX monitoring, the "HttpClient", it's "ThreadPool" and the "Logger"
 * are set as context attributes prefixed with "org.eclipse.jetty.servlets."+name
 * (unless otherwise set with attrPrefix). This attribute prefix is also used for the
 * logger name.
 * <p>
 * The following init parameters may be used to configure the servlet: <ul>
 * <li>name - Name of Proxy servlet (default: "ProxyServlet"
 * <li>maxThreads - maximum threads
 * <li>maxConnections - maximum connections per destination
 * <li>HostHeader - Force the host header to a particular value
 * </ul>
 */
class ProxyServlet extends Servlet {
  protected var _log: Logger = _
  var _client: HttpClient = _
  var _hostHeader: String = _

  protected val _DontProxyHeaders = Set(
    "proxy-connection",
    "connection",
    "keep-alive",
    "transfer-encoding",
    "te",
    "trailer",
    "proxy-authorization",
    "proxy-authenticate",
    "upgrade"
  )

  protected var _config: ServletConfig = _
  protected var _context: ServletContext = _
  protected var _name = "ProxyServlet"

  /* ------------------------------------------------------------ */
  /* (non-Javadoc)
   * @see javax.servlet.Servlet#init(javax.servlet.ServletConfig)
   */
  @throws(classOf[ServletException])
  def init(config: ServletConfig) {
    _config = config
    _context = config.getServletContext

    _client = new HttpClient
    _client.setConnectorType(HttpClient.CONNECTOR_SELECT_CHANNEL)

    _hostHeader = config.getInitParameter("HostHeader")

    try {
      var t = config.getInitParameter("attrPrefix")
      if (t != null) {
        _name = t
      }
      _log = Log.getLogger("org.eclipse.jetty.servlets." + _name)

      t = config.getInitParameter("maxThreads")
      if (t != null){
        _client.setThreadPool(new QueuedThreadPool(Integer.parseInt(t)))
      } else {
        _client.setThreadPool(new QueuedThreadPool)
      }
      _client.getThreadPool.asInstanceOf[QueuedThreadPool].setName(_name.substring(_name.lastIndexOf('.') + 1))

      t = config.getInitParameter("maxConnections")
      if (t != null) {
        _client.setMaxConnectionsPerAddress(Integer.parseInt(t))
      }

      _client.start

      if (_context != null) {
        _context.setAttribute("org.eclipse.jetty.servlets." + _name + ".Logger", _log)
        _context.setAttribute("org.eclipse.jetty.servlets." + _name + ".ThreadPool", _client.getThreadPool)
        _context.setAttribute("org.eclipse.jetty.servlets." + _name + ".HttpClient", _client)
      }
    } catch {case e: Exception => throw new ServletException(e)}
  }

  /* ------------------------------------------------------------ */
  /* (non-Javadoc)
   * @see javax.servlet.Servlet#getServletConfig()
   */
  def getServletConfig = _config

  /* ------------------------------------------------------------ */
  /** Get the hostHeader.
   * @return the hostHeader
   */
  def getHostHeader = _hostHeader
  

  /* ------------------------------------------------------------ */
  /** Set the hostHeader.
   * @param hostHeader the hostHeader to set
   */
  def setHostHeader(hostHeader: String) {
    _hostHeader = hostHeader
  }

  /* ------------------------------------------------------------ */
  /* (non-Javadoc)
   * @see javax.servlet.Servlet#service(javax.servlet.ServletRequest, javax.servlet.ServletResponse)
   */
  @throws(classOf[ServletException])
  @throws(classOf[IOException])
  def service($req: ServletRequest, $res: ServletResponse) {
    val debug = if (_log.isDebugEnabled) $req.hashCode else 0

    val req = $req.asInstanceOf[HttpServletRequest]
    val res = $res.asInstanceOf[HttpServletResponse]
    if ("CONNECT".equalsIgnoreCase(req.getMethod)) {
      handleConnect(req, res)
    } else {
      val in  = req.getInputStream
      val out = res.getOutputStream

      val continuation = ContinuationSupport.getContinuation(req)

      if (!continuation.isInitial) {
        res.sendError(HttpServletResponse.SC_GATEWAY_TIMEOUT) // Need better test that isInitial
      } else {
        var uri = req.getRequestURI
        if (req.getQueryString != null) {
          uri += "?" + req.getQueryString
        }

        val url = proxyHttpURI(req.getScheme,
                               req.getServerName,
                               req.getServerPort,
                               uri)

        if (debug != 0) {
          _log.debug(debug + " proxy "+uri+"-->" + url)
        }

        if (url==null) {
          res.sendError(HttpServletResponse.SC_FORBIDDEN);
          return
        }

        val exchange = new HttpExchange {
          
          @throws(classOf[IOException])
          override def onRequestCommitted {
          }

          @throws(classOf[IOException])
          override def onRequestComplete {
          }

          @throws(classOf[IOException])
          override def onResponseComplete {
            if (debug != 0) {
              _log.debug(debug + " complete")
            }
            continuation.complete
          }

          @throws(classOf[IOException])
          override def onResponseContent(content: Buffer) {
            if (debug != 0)
              _log.debug(debug + " content" + content.length)
            content.writeTo(out)
          }

          @throws(classOf[IOException])
          override def onResponseHeaderComplete {
          }

          @throws(classOf[IOException])
          override protected def onResponseStatus(version: Buffer, status: Int, reason: Buffer) {
            if (debug != 0) {
              _log.debug(debug + " " + version + " " + status + " " + reason)
            }

            if (reason != null && reason.length > 0) {
              res.setStatus(status,reason.toString)
            } else {
              res.setStatus(status)
            }
          }

          @throws(classOf[IOException])
          override protected def onResponseHeader(name: Buffer, value: Buffer) {
            val s = name.toString().toLowerCase
            if (!_DontProxyHeaders.contains(s) || (HttpHeaders.CONNECTION_BUFFER.equals(name) &&
                                                   HttpHeaderValues.CLOSE_BUFFER.equals(value))
            ) {
              if (debug != 0) {
                _log.debug(debug + " " + name + ": " + value)
              }

              res.addHeader(name.toString, value.toString)
            } else if (debug != 0) {
              _log.debug(debug + " " + name+"! " + value)
            }
          }

          override protected def onConnectionFailed(ex: Throwable) {
            onException(ex)
          }

          override protected def onException(ex: Throwable) {
            if (ex.isInstanceOf[EofException]) {
              Log.ignore(ex)
              return
            }
            Log.warn(ex.toString)
            Log.debug(ex)
            if (!res.isCommitted) {
              res.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
            }
            continuation.complete
          }

          override protected def onExpire {
            if (!res.isCommitted) {
              res.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR)
            }
            continuation.complete
          }
        }

        exchange.setScheme(if (HttpSchemes.HTTPS == req.getScheme) HttpSchemes.HTTPS_BUFFER else HttpSchemes.HTTP_BUFFER)
        exchange.setMethod(req.getMethod)
        exchange.setURL(url.toString)
        exchange.setVersion(req.getProtocol)

        if (debug != 0) {
          _log.debug(debug + " " + req.getMethod + " " + url + " " + req.getProtocol)
        }
        
        // check connection header
        var connectionHdr = req.getHeader("Connection")
        if (connectionHdr != null) {
          connectionHdr = connectionHdr.toLowerCase
          if (connectionHdr.indexOf("keep-alive") < 0 && connectionHdr.indexOf("close") < 0) {
            connectionHdr = null
          }
        }

        // force host
        if (_hostHeader!=null) {
          exchange.setRequestHeader("Host",_hostHeader)
        }

        // copy headers
        var xForwardedFor = false
        var hasContent = false
        val enm = req.getHeaderNames
        while (enm.hasMoreElements) {
          // TODO could be better than this!
          val hdr = enm.nextElement.asInstanceOf[String]
          val lhdr = hdr.toLowerCase

          if (_DontProxyHeaders.contains(lhdr) ||
              connectionHdr != null && connectionHdr.indexOf(lhdr) >= 0 ||
              _hostHeader != null && "host" == lhdr
          ) {
          } else {
            lhdr match {
              case "content-type" =>
                hasContent = true
              case "content-length" =>
                val contentLength = req.getContentLength
                exchange.setRequestHeader(HttpHeaders.CONTENT_LENGTH, TypeUtil.toString(contentLength))
                if (contentLength > 0) {
                  hasContent = true
                }
              case "x-forwarded-for" =>
                xForwardedFor = true
              case _ =>
            }
            
            val vals = req.getHeaders(hdr)
            while (vals.hasMoreElements) {
              val value = vals.nextElement.asInstanceOf[String]
              if (value != null) {
                if (debug != 0) {
                  _log.debug(debug + " " + hdr + ": " + value)
                }

                exchange.setRequestHeader(hdr, value)
              }
            }
          }
        }

        // Proxy headers
        exchange.setRequestHeader("Via", "1.1 (jetty)")
        if (!xForwardedFor) {
          exchange.addRequestHeader("X-Forwarded-For", req.getRemoteAddr)
        }

        if (hasContent) {
          exchange.setRequestContentSource(in)
        }

        continuation.suspend(res)
        _client.send(exchange)
      }
    }
  }


  /* ------------------------------------------------------------ */
  @throws(classOf[IOException])
  def handleConnect(req: HttpServletRequest, res: HttpServletResponse) {
    val uri = req.getRequestURI

    var port = ""
    var host = ""
    val c = uri.indexOf(':')
    if (c >= 0) {
      port = uri.substring(c + 1)
      host = uri.substring(0, c)
      if (host.indexOf('/') > 0) {
        host = host.substring(host.indexOf('/') + 1)
      }
    }

    // TODO - make this async!


    val inetAddress = new InetSocketAddress (host, Integer.parseInt(port))

    //if (isForbidden(HttpMessage.__SSL_SCHEME,addrPort.getHost(),addrPort.getPort(),false))
    //{
    //    sendForbid(request,response,uri);
    //}
    //else
    {
      val in  = req.getInputStream
      val out = res.getOutputStream

      val socket = new Socket(inetAddress.getAddress, inetAddress.getPort)

      res.setStatus(200)
      res.setHeader("Connection","close")
      res.flushBuffer
      // TODO prevent real close!

      IO.copyThread(socket.getInputStream, out)
      IO.copy(in, socket.getOutputStream)
    }
  }

  /* ------------------------------------------------------------ */
  @throws(classOf[MalformedURLException])
  protected def proxyHttpURI(scheme: String, serverName: String, serverPort: Int, uri: String): HttpURI = {
    new HttpURI(scheme + "://" + serverName + ":" + serverPort + uri)
  }


  /* (non-Javadoc)
   * @see javax.servlet.Servlet#getServletInfo()
   */
  def getServletInfo = "Proxy Servlet"

  /* (non-Javadoc)
   * @see javax.servlet.Servlet#destroy()
   */
  def destroy {
  }

  /**
   * Transparent Proxy.
   *
   * This convenience extension to ProxyServlet configures the servlet
   * as a transparent proxy.   The servlet is configured with init parameter:<ul>
   * <li> ProxyTo - a URI like http://host:80/context to which the request is proxied.
   * <li> Prefix  - a URI prefix that is striped from the start of the forwarded URI.
   * </ul>
   * For example, if a request was received at /foo/bar and the ProxyTo was  http://host:80/context
   * and the Prefix was /foo, then the request would be proxied to http://host:80/context/bar
   *
   */
  class Transparent(prefix: String,server: String, port: Int) extends ProxyServlet {
    var _prefix = prefix
    var _proxyTo = "http://" + server + ":" + port

    @throws(classOf[ServletException])
    override def init(config: ServletConfig) {
      if (config.getInitParameter("ProxyTo") != null)
        _proxyTo=config.getInitParameter("ProxyTo")
      if (config.getInitParameter("Prefix") != null)
        _prefix=config.getInitParameter("Prefix")
      if (_proxyTo == null)
        throw new UnavailableException("No ProxyTo")

      super.init(config)
      _log.info(_name + " @ " + (if (_prefix == null) "-" else _prefix) + " to " + _proxyTo)
    }

    @throws(classOf[MalformedURLException])
    override protected def proxyHttpURI(scheme: String, serverName: String, serverPort: Int, uri: String): HttpURI = {
      if (_prefix != null && !uri.startsWith(_prefix)) {
        return null
      }

      if (_prefix != null) {
        return new HttpURI(_proxyTo + uri.substring(_prefix.length))
      }
      
      new HttpURI(_proxyTo + uri)
    }
  }

}
