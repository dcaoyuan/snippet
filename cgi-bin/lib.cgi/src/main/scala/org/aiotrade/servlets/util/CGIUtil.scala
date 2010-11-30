/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.aiotrade.servlets.util

import java.io.BufferedReader
import java.io.IOException
import java.io.InputStream
import java.io.InputStreamReader
import java.util.logging.Logger
import javax.servlet.http.HttpServletResponse

object CGIUtil {
  val log = Logger.getLogger(this.getClass.getName)

  @throws(classOf[IOException])
  def flowResponse(res: HttpServletResponse, cgiin: InputStream) {
    try {
      setResponseHeaders(res, cgiin)
      writeResponseContent(res, cgiin)
    } finally {
      // Attempt to consume any leftover byte if something bad happens,
      // such as a socket disconnect on the servlet side; otherwise, the
      // external process could hang
      var available = 0
      while ({available = cgiin.available; available > 0}) {
        cgiin.skip(available)
      }
    }
  }

  @throws(classOf[IOException])
  def setResponseHeaders(res: HttpServletResponse, cgiin: InputStream) = {
    // --- set headers
    val cgiHeaderReader = new BufferedReader(new InputStreamReader(new HTTPHeaderInputStream(cgiin)))
    var line: String = null
    while ({line = cgiHeaderReader.readLine; line != null && line != ""}) {
      log.fine("Response: addHeader(\"" + line + "\")")

      val idxOfColon = line.indexOf(":")
      if (line.startsWith("HTTP")) {
        res.setStatus(getStatusCodeFromHttpStatusLine(line))
      } else if (idxOfColon >= 0) {
        val header = line.substring(0, idxOfColon).trim
        val value = line.substring(idxOfColon + 1).trim
        if (header.equalsIgnoreCase("status")) {
          res.setStatus(getStatusCodeCFromCGIStatusHeader(value))
        } else {
          res.addHeader(header, value)
        }
      } else {
        log.warning("Response: bad header line \"" + line + "\"")
      }
    }
  }

  @throws(classOf[IOException])
  def writeResponseContent(res: HttpServletResponse, cgiin: InputStream): Int = {
    val out = res.getOutputStream
    val buf = new Array[Byte](2048)
    var c = -1
    while ({c = cgiin.read(buf); c >= 0}) {
      log.fine("CGI: output " + c + " bytes of data")
      out.write(buf, 0, c)
    }
    c
  }

  /**
   * Parses the Status-Line and extracts the status code.
   *
   * @param line The HTTP Status-Line (RFC2616, section 6.1)
   * @return The extracted status code or the code representing an
   * internal error if a valid status code cannot be extracted.
   */
  def getStatusCodeFromHttpStatusLine(line: String): Int = {
    val statusStart = line.indexOf(' ') + 1
    if (statusStart < 1 || line.length < statusStart + 3) {
      // Not a valid HTTP Status-Line
      log.warning("Response: invalid HTTP Status-Line:" + line)
      return HttpServletResponse.SC_INTERNAL_SERVER_ERROR
    }

    val status = line.substring(statusStart, statusStart + 3)
    toStatusCode(status)
  }

  /**
   * Parses the CGI Status Header value and extracts the status code.
   *
   * @param value The CGI Status value of the form <code>
   *             digit digit digit SP reason-phrase</code>
   * @return The extracted status code or the code representing an
   * internal error if a valid status code cannot be extracted.
   */
  def getStatusCodeCFromCGIStatusHeader(value: String): Int = {
    if (value.length < 3) {
      // Not a valid status value
      log.warning("Response: invalid status value:" + value)
      return HttpServletResponse.SC_INTERNAL_SERVER_ERROR
    }

    val status = value.substring(0, 3)
    toStatusCode(status)
  }

  private def toStatusCode(status: String): Int = {
    try {
      status.toInt
    } catch {
      case _: NumberFormatException =>
        // Not a valid status value
        log.warning("Response: invalid status code:" + status)
        HttpServletResponse.SC_INTERNAL_SERVER_ERROR
    }
  }



  /**
   * This is an input stream specifically for reading HTTP headers. It reads
   * upto and including the two blank lines terminating the headers. It
   * allows the content to be read using bytes or characters as appropriate.
   */
  class HTTPHeaderInputStream(input: InputStream) extends InputStream {

    private val STATE_CHARACTER  = 0
    private val STATE_FIRST_CR   = 1
    private val STATE_FIRST_LF   = 2
    private val STATE_SECOND_CR  = 3
    private val STATE_HEADER_END = 4

    private var state = STATE_CHARACTER

    /**
     * @see java.io.InputStream#read
     */
    @throws(classOf[IOException])
    def read: Int = {
      if (state == STATE_HEADER_END) {
        return -1
      }

      // Update the state
      // State machine looks like this
      //
      //    -------->--------
      //   |      (CR)       |
      //   |                 |
      //  CR1--->---         |
      //   |        |        |
      //   ^(CR)    |(LF)    |
      //   |        |        |
      // CHAR--->--LF1--->--EOH
      //      (LF)  |  (LF)  |
      //            |(CR)    ^(LF)
      //            |        |
      //          (CR2)-->---

      val c = input.read
      state = c match {
        case 10 =>
          // LF
          state match {
            case STATE_CHARACTER => STATE_FIRST_LF
            case STATE_FIRST_CR  => STATE_FIRST_LF
            case STATE_FIRST_LF | STATE_SECOND_CR => STATE_HEADER_END
          }

        case 13 =>
          // CR
          state match {
            case STATE_CHARACTER => STATE_FIRST_CR
            case STATE_FIRST_CR  => STATE_HEADER_END
            case STATE_FIRST_LF  => STATE_SECOND_CR
          }

        case _ => STATE_CHARACTER
      }

      c
    }
  }  // class HTTPHeaderInputStream

}
