/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2008 Sun Microsystems, Inc. All rights reserved.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common Development
 * and Distribution License("CDDL") (collectively, the "License").  You
 * may not use this file except in compliance with the License. You can obtain
 * a copy of the License at https://glassfish.dev.java.net/public/CDDL+GPL.html
 * or glassfish/bootstrap/legal/LICENSE.txt.  See the License for the specific
 * language governing permissions and limitations under the License.
 *
 * When distributing the software, include this License Header Notice in each
 * file and include the License file at glassfish/bootstrap/legal/LICENSE.txt.
 * Sun designates this particular file as subject to the "Classpath" exception
 * as provided by Sun in the GPL Version 2 section of the License file that
 * accompanied this code.  If applicable, add the following below the License
 * Header, with the fields enclosed by brackets [] replaced by your own
 * identifying information: "Portions Copyrighted [year]
 * [name of copyright owner]"
 *
 * Contributor(s):
 *
 * If you wish your version of this file to be governed by only the CDDL or
 * only the GPL Version 2, indicate your decision by adding "[Contributor]
 * elects to include this software in this distribution under the [CDDL or GPL
 * Version 2] license."  If you don't indicate a single choice of license, a
 * recipient has the option to distribute your version of this file under
 * either the CDDL, the GPL Version 2 or to extend the choice of license to
 * its licensees as provided above.  However, if you add GPL Version 2 code
 * and therefore, elected the GPL Version 2 license, then the option applies
 * only if the new code is made subject to such option by the copyright
 * holder.
 *
 *
 * This file incorporates work covered by the following copyright and
 * permission notice:
 *
 * Copyright 2004 The Apache Software Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.aiotrade.servlets.cgi

import java.io.IOException
import java.util.Date
import java.util.Hashtable
import java.util.Locale

import javax.servlet.ServletConfig
import javax.servlet.ServletException
import javax.servlet.ServletOutputStream
import javax.servlet.UnavailableException
import javax.servlet.http.HttpServlet
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import org.aiotrade.servlets.util.Globals


object CGIServlet {
  /** the shell environment variables to be passed to the CGI script */
  protected val shellEnv = new Hashtable[String, String]
}
class CGIServlet extends HttpServlet {
  import CGIServlet._

  /** default cgi name */
  private var defaultCgi: String = _
  /* some vars below copied from Craig R. McClanahan's InvokerServlet */
  /** the debugging detail level for this servlet. */
  private var debug = 0
  /**
   *  The CGI search path will start at
   *    webAppRootDir + File.separator + cgiPathPrefix
   *    (or webAppRootDir alone if cgiPathPrefix is
   *    null)
   */
  private var cgiPathPrefix: String = _
  /** strip the substring from the request URI before passing it on
   to the CGI environment */
  private var stripedRequestURI = ""
  /** the encoding to use for parameters */
  private var parameterEncoding = System.getProperty("file.encoding", "UTF-8")
  private var passShellEnvironment = false

  /**
   * Sets instance variables.
   * <P>
   * Modified from Craig R. McClanahan's InvokerServlet
   * </P>
   *
   * @param config                    a <code>ServletConfig</code> object
   *                                  containing the servlet's
   *                                  configuration and initialization
   *                                  parameters
   *
   * @exception ServletException      if an exception has occurred that
   *                                  interferes with the servlet's normal
   *                                  operation
   */
  @throws(classOf[ServletException])
  override def init(config: ServletConfig) {
    super.init(config)

    // Verify that we were not accessed using the invoker servlet
    var servletName = getServletConfig.getServletName
    if (servletName == null) {
      servletName = ""
    }
    if (servletName.startsWith("org.apache.catalina.INVOKER.")) {
      throw new UnavailableException("Cannot invoke CGIServlet through the invoker")
    }

    // Set our properties from the initialization parameters
    var value: String = null;
    if (getServletConfig.getInitParameter("debug") != null) {
      debug = getServletConfig.getInitParameter("debug").toInt
    }
    if (getServletConfig.getInitParameter("defaultCgi") != null) {
      defaultCgi = getServletConfig.getInitParameter("defaultCgi")
    }
    cgiPathPrefix = getServletConfig.getInitParameter("cgiPathPrefix")

    if (getServletConfig.getInitParameter("passShellEnvironment") != null) {
      passShellEnvironment = getServletConfig.getInitParameter("passShellEnvironment").toBoolean
    }

    if (getServletConfig.getInitParameter("parameterEncoding") != null) {
      parameterEncoding = getServletConfig.getInitParameter("parameterEncoding")
    }

    if (getServletConfig.getInitParameter("stripRequestURI") != null) {
      stripedRequestURI = getServletConfig.getInitParameter("stripRequestURI")
    }
  }


  /**
   * Provides CGI Gateway service -- delegates to <code>doGet</code>
   *
   * @param  req   HttpServletRequest passed in by servlet container
   * @param  res   HttpServletResponse passed in by servlet container
   *
   * @exception  ServletException  if a servlet-specific exception occurs
   * @exception  IOException  if a read/write exception occurs
   *
   * @see javax.servlet.http.HttpServlet
   *
   */
  @throws(classOf[IOException])
  @throws(classOf[ServletException])
  override protected def doPost(req: HttpServletRequest, res: HttpServletResponse) {
    doGet(req, res)
  }

  /**
   * Provides CGI Gateway service
   *
   * @param  req   HttpServletRequest passed in by servlet container
   * @param  res   HttpServletResponse passed in by servlet container
   *
   * @exception  ServletException  if a servlet-specific exception occurs
   * @exception  IOException  if a read/write exception occurs
   *
   * @see javax.servlet.http.HttpServlet
   *
   */
  @throws(classOf[IOException])
  @throws(classOf[ServletException])
  override protected def doGet(req: HttpServletRequest, res: HttpServletResponse) {

    // Verify that we were not accessed using the invoker servlet
    if (req.getAttribute(Globals.INVOKED_ATTR) != null) {
      throw new UnavailableException("Cannot invoke CGIServlet through the invoker");
    }

    val cgiEnv = new CGIEnv(req, getServletContext,
                            defaultCgi,
                            cgiPathPrefix,
                            stripedRequestURI,
                            parameterEncoding,
                            passShellEnvironment)
    cgiEnv.setEnv

    if (cgiEnv.isValid) {
      val cgi = new CGIRunner(cgiEnv.command,
                              cgiEnv.envs,
                              cgiEnv.workingDir,
                              cgiEnv.cmdLineParams)
      //if POST, we need to cgi.setInput
      //REMIND: how does this interact with Servlet API 2.3's Filters?!
      if ("POST" == req.getMethod) {
        cgi.setInput(req.getInputStream)
      }
      cgi.setResponse(res)
      cgi.run
    }

    if (!cgiEnv.isValid) {
      res.setStatus(404)
    }

    if (debug >= 10) {
      val out = res.getOutputStream
      out.println("<HTML><HEAD><TITLE>$Name:  $</TITLE></HEAD>")
      out.println("<BODY>$Header$<p>")

      if (cgiEnv.isValid) {
        out.println(cgiEnv.toString)
      } else {
        out.println("<H3>")
        out.println("CGI script not found or not specified.")
        out.println("</H3>")
        out.println("<H4>")
        out.println("Check the <b>HttpServletRequest ")
        out.println("<a href=\"#pathInfo\">pathInfo</a></b> ")
        out.println("property to see if it is what you meant ")
        out.println("it to be.  You must specify an existant ")
        out.println("and executable file as part of the ")
        out.println("path-info.")
        out.println("</H4>")
        out.println("<H4>")
        out.println("For a good discussion of how CGI scripts ")
        out.println("work and what their environment variables ")
        out.println("mean, please visit the <a ")
        out.println("href=\"http://cgi-spec.golux.com\">CGI ")
        out.println("Specification page</a>.")
        out.println("</H4>")
      }

      printServletEnvironment(out, req, res)

      out.println("</BODY></HTML>")
    } //debugging


  } //doGet


  /**
   * Prints out important Servlet API and container information
   *
   * <p>
   * Copied from SnoopAllServlet by Craig R. McClanahan
   * </p>
   *
   * @param  out    ServletOutputStream as target of the information
   * @param  req    HttpServletRequest object used as source of information
   * @param  res    HttpServletResponse object currently not used but could
   *                provide future information
   *
   * @exception  IOException  if a write operation exception occurs
   *
   */
  @throws(classOf[IOException])
  protected def printServletEnvironment(out: ServletOutputStream,
                                        req: HttpServletRequest, res: HttpServletResponse) {

    // Document the properties from ServletRequest
    out.println("<h1>ServletRequest Properties</h1>");
    out.println("<ul>");
    var attrs = req.getAttributeNames
    while (attrs.hasMoreElements) {
      val attr = attrs.nextElement.asInstanceOf[String]
      out.println("<li><b>attribute</b> " + attr + " = " + req.getAttribute(attr))
    }
    out.println("<li><b>characterEncoding</b> = " + req.getCharacterEncoding)
    out.println("<li><b>contentLength</b> = " + req.getContentLength)
    out.println("<li><b>contentType</b> = " + req.getContentType)
    val locales = req.getLocales
    while (locales.hasMoreElements) {
      val locale = locales.nextElement.asInstanceOf[Locale]
      out.println("<li><b>locale</b> = " + locale)
    }
    var params = req.getParameterNames
    while (params.hasMoreElements) {
      val param = params.nextElement.asInstanceOf[String]
      val values = req.getParameterValues(param)
      values foreach {x => out.println("<li><b>parameter</b> " + param + " = " + x)}
    }
    out.println("<li><b>protocol</b> = " + req.getProtocol)
    out.println("<li><b>remoteAddr</b> = " + req.getRemoteAddr)
    out.println("<li><b>remoteHost</b> = " + req.getRemoteHost)
    out.println("<li><b>scheme</b> = " + req.getScheme)
    out.println("<li><b>secure</b> = " + req.isSecure)
    out.println("<li><b>serverName</b> = " + req.getServerName)
    out.println("<li><b>serverPort</b> = " + req.getServerPort)
    out.println("</ul>")
    out.println("<hr>")

    // Document the properties from HttpServletRequest
    out.println("<h1>HttpServletRequest Properties</h1>")
    out.println("<ul>")
    out.println("<li><b>authType</b> = " + req.getAuthType)
    out.println("<li><b>contextPath</b> = " + req.getContextPath)
    val cookies = req.getCookies
    if (cookies != null) {
      cookies foreach {x => out.println("<li><b>cookie</b> " + x.getName + " = " + x.getValue)}
    }
    val headers = req.getHeaderNames
    while (headers.hasMoreElements) {
      val header = headers.nextElement.asInstanceOf[String]
      out.println("<li><b>header</b> " + header + " = " + req.getHeader(header))
    }
    out.println("<li><b>method</b> = " + req.getMethod)
    out.println("<li><a name=\"pathInfo\"><b>pathInfo</b></a> = " + req.getPathInfo)
    out.println("<li><b>pathTranslated</b> = " + req.getPathTranslated)
    out.println("<li><b>queryString</b> = " + req.getQueryString)
    out.println("<li><b>remoteUser</b> = " + req.getRemoteUser)
    out.println("<li><b>requestedSessionId</b> = " + req.getRequestedSessionId)
    out.println("<li><b>requestedSessionIdFromCookie</b> = " + req.isRequestedSessionIdFromCookie)
    out.println("<li><b>requestedSessionIdFromURL</b> = " + req.isRequestedSessionIdFromURL)
    out.println("<li><b>requestedSessionIdValid</b> = " + req.isRequestedSessionIdValid)
    out.println("<li><b>requestURI</b> = " + req.getRequestURI)
    out.println("<li><b>servletPath</b> = " + req.getServletPath)
    out.println("<li><b>userPrincipal</b> = " + req.getUserPrincipal)
    out.println("</ul>")
    out.println("<hr>")

    // Document the servlet request attributes
    out.println("<h1>ServletRequest Attributes</h1>")
    out.println("<ul>")
    attrs = req.getAttributeNames
    while (attrs.hasMoreElements) {
      val attr = attrs.nextElement.asInstanceOf[String]
      out.println("<li><b>" + attr + "</b> = " + req.getAttribute(attr))
    }
    out.println("</ul>")
    out.println("<hr>")

    // Process the current session (if there is one)
    val session = req.getSession(false)
    if (session != null) {

      // Document the session properties
      out.println("<h1>HttpSession Properties</h1>")
      out.println("<ul>")
      out.println("<li><b>id</b> = " + session.getId)
      out.println("<li><b>creationTime</b> = " + new Date(session.getCreationTime))
      out.println("<li><b>lastAccessedTime</b> = " + new Date(session.getLastAccessedTime))
      out.println("<li><b>maxInactiveInterval</b> = " + session.getMaxInactiveInterval)
      out.println("</ul>")
      out.println("<hr>")

      // Document the session attributes
      out.println("<h1>HttpSession Attributes</h1>")
      out.println("<ul>")
      attrs = session.getAttributeNames
      while (attrs.hasMoreElements) {
        val attr = attrs.nextElement.asInstanceOf[String]
        out.println("<li><b>" + attr + "</b> = " + session.getAttribute(attr))
      }
      out.println("</ul>")
      out.println("<hr>")

    }

    // Document the servlet configuration properties
    out.println("<h1>ServletConfig Properties</h1>")
    out.println("<ul>")
    out.println("<li><b>servletName</b> = " + getServletConfig.getServletName)
    out.println("</ul>")
    out.println("<hr>")

    // Document the servlet configuration initialization parameters
    out.println("<h1>ServletConfig Initialization Parameters</h1>")
    out.println("<ul>")
    params = getServletConfig.getInitParameterNames
    while (params.hasMoreElements) {
      val param = params.nextElement.asInstanceOf[String]
      val value = getServletConfig.getInitParameter(param)
      out.println("<li><b>" + param + "</b> = " + value)
    }
    out.println("</ul>")
    out.println("<hr>")

    // Document the servlet context properties
    out.println("<h1>ServletContext Properties</h1>")
    out.println("<ul>")
    out.println("<li><b>majorVersion</b> = " + getServletContext.getMajorVersion)
    out.println("<li><b>minorVersion</b> = " + getServletContext.getMinorVersion)
    out.println("<li><b>realPath('/')</b> = " + getServletContext.getRealPath("/"))
    out.println("<li><b>serverInfo</b> = " + getServletContext.getServerInfo)
    out.println("</ul>")
    out.println("<hr>")

    // Document the servlet context initialization parameters
    out.println("<h1>ServletContext Initialization Parameters</h1>");
    out.println("<ul>")
    params = getServletContext.getInitParameterNames
    while (params.hasMoreElements) {
      val param = params.nextElement.asInstanceOf[String]
      val value = getServletContext.getInitParameter(param)
      out.println("<li><b>" + param + "</b> = " + value)
    }
    out.println("</ul>")
    out.println("<hr>")

    // Document the servlet context attributes
    out.println("<h1>ServletContext Attributes</h1>")
    out.println("<ul>")
    attrs = getServletContext.getAttributeNames
    while (attrs.hasMoreElements) {
      val attr = attrs.nextElement.asInstanceOf[String]
      out.println("<li><b>" + attr + "</b> = " + getServletContext.getAttribute(attr))
    }
    out.println("</ul>")
    out.println("<hr>")
  }


} //class CGIServlet

