/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.aiotrade.servlets.util

import java.io.BufferedReader
import java.io.IOException
import java.io.InputStreamReader
import java.util.logging.Logger
import javax.servlet.ServletContext
import javax.servlet.ServletException
import javax.servlet.http.HttpServletRequest
import scala.collection.mutable.HashMap

abstract class CGIEnvironment(req: HttpServletRequest, context: ServletContext, passShellEnv: Boolean) {
  val log = Logger.getLogger(this.getClass.getName)
  
  val envs = new HashMap[String, String]

  var isValid = true

  def setEnv: HashMap[String, String] = {
    // Add the shell environment variables (if any)
    if (passShellEnv) {
      try {
        envs ++= getShellEnvironment
      } catch {
        case ioe: IOException => throw new ServletException("Unable to read shell environment variables", ioe)
      }
    }

    envs += ("GATEWAY_INTERFACE" -> "CGI/1.1")

    envs += ("REQUEST_URI" -> nullToBlank(req.getRequestURI)) // may need to strip parts in CGI
    
    envs += ("REQUEST_METHOD" -> nullToBlank(req.getMethod))

    envs += ("SERVER_SOFTWARE" -> "AIOTradeServlets")

    envs += ("SERVER_PROTOCOL" -> nullToBlank(req.getProtocol))

    envs += ("SERVER_NAME" -> nullToBlank(req.getServerName))

    envs += ("SERVER_PORT" -> (req.getServerPort match {case 0 => "-1" case x => x.toString}))

    envs += ("REMOTE_HOST" -> nullToBlank(req.getRemoteHost))
    
    envs += ("REMOTE_ADDR" -> nullToBlank(req.getRemoteAddr))

    envs += ("REMOTE_USER" -> nullToBlank(req.getRemoteUser))

    envs += ("REMOTE_IDENT" -> "") //not necessary for full compliance

    envs += ("AUTH_TYPE" -> nullToBlank(req.getAuthType))

    envs += ("QUERY_STRING" -> nullToBlank(req.getQueryString))

    envs += ("CONTENT_TYPE" -> nullToBlank(req.getContentType))

    /* Note CGI spec says CONTENT_LENGTH must be NULL ("") or undefined
     * if there is no content, so we cannot put 0 or -1 in as per the
     * Servlet API spec.
     */
    envs += ("CONTENT_LENGTH" -> (req.getContentLength match {case x if x <= 0 => "" case x => x.toString}))

    val headers = req.getHeaderNames
    while (headers.hasMoreElements) {
      val header = headers.nextElement.asInstanceOf[String].toUpperCase
      val value = req.getHeader(header)

      //REMIND: rewrite multiple headers as if received as single change character set
      //REMIND: I forgot what the previous REMIND means
      header match {
        case "IF-MODIFIED-SINCE" =>
        case "IF_NONE_MATCH" =>
        case "AUTHORIZATION" | "PROXY-AUTHORIZATION" => //NOOP per CGI specification section 11.2
        case "CONTENT-LENGTH" | "CONTENT-TYPE" => // don't process here, should have been processed via req.getContentLength
        case _ => envs += ("HTTP_" + header.replace('-', '_') -> value)
      }
    }

    setScriptEnv

    envs
  }

  protected def setScriptEnv: Boolean


  /**
   * Converts null strings to blank strings ("")
   *
   * @param    s string to be converted if necessary
   * @return   a non-null string, either the original or the empty string
   *           ("") if the original was <code>null</code>
   */
  protected def nullToBlank(s: String): String = {
    nullToString(s, "")
  }

  /**
   * Converts null strings to another string
   *
   * @param    couldBeNull string to be converted if necessary
   * @param    subForNulls string to return instead of a null string
   * @return   a non-null string, either the original or the substitute
   *           string if the original was <code>null</code>
   */
  protected def nullToString(couldBeNull: String, subForNull: String): String = {
    if (couldBeNull == null) subForNull else couldBeNull
  }

  /**
   * Converts blank strings to another string
   *
   * @param    couldBeBlank string to be converted if necessary
   * @param    subForBlanks string to return instead of a blank string
   * @return   a non-null string, either the original or the substitute
   *           string if the original was <code>null</code> or empty ("")
   */
  protected def blankToString(couldBeBlank: String, subForBlank: String): String = {
    if ("".equals(couldBeBlank) || couldBeBlank == null) subForBlank else couldBeBlank
  }


  /**
   * Get all shell environment variables. Have to do it this rather ugly way
   * as the API to obtain is not available in 1.4 and earlier APIs.
   *
   * See <a href="http://www.rgagnon.com/javadetails/java-0150.html">Read environment
   * variables from an application</a> for original source and article.
   */
  @throws(classOf[IOException])
  protected def getShellEnvironment: HashMap[String, String] = {
    val envs = new HashMap[String, String]
    val r = Runtime.getRuntime
    val OS = System.getProperty("os.name").toLowerCase

    val (p, ignoreCase) = if (OS.indexOf("windows 9") > -1) {
      (r.exec("command.com /c set"), true)
    } else if ((OS.indexOf("nt") > -1) ||
               (OS.indexOf("windows 20") > -1) ||
               (OS.indexOf("windows xp") > -1)) {
      // thanks to JuanFran for the xp fix!
      (r.exec("cmd.exe /c set"), true)
    } else {
      // our last hope, we assume Unix (thanks to H. Ware for the fix)
      (r.exec("env"), false)
    }

    val is = new BufferedReader(new InputStreamReader(p.getInputStream))
    var line: String = null
    while ({line = is.readLine; line != null}) {
      val idx = line.indexOf('=')
      var key = line.substring(0, idx)
      val value = line.substring(idx + 1)
      if (ignoreCase) {
        key = key.toUpperCase
      }
      envs.put(key, value)
    }
    envs
  }


}
