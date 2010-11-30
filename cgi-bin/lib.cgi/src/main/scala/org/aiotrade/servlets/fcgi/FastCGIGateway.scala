/*
 * (c) 2009 Julien Rialland, and the jFastCGI project developpers.
 * 
 * Released under BSD License (see license.txt)
 *  
 *   $Id$ 
 */
package org.aiotrade.servlets.fcgi

import java.io.BufferedOutputStream
import java.io.IOException
import java.io.InputStream
import java.io.OutputStream
import java.util.logging.Logger

import javax.servlet.ServletException
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import org.aiotrade.servlets.util.CGIUtil

/**
 * typedef struct {
 *  unsigned char version;
 *  unsigned char type;
 *  unsigned char requestIdB1;
 *  unsigned char requestIdB0;
 *  unsigned char contentLengthB1;
 *  unsigned char contentLengthB0;
 *  unsigned char paddingLength;
 *  unsigned char reserved;
 *  unsigned char contentData[contentLength];
 *  unsigned char paddingData[paddingLength];
 * } FCGI_Record;
 * 
 */
@serializable
@SerialVersionUID(7733797250176758803L)
object FastCGIGateway {
  val log = Logger.getLogger(classOf[FastCGIGateway].getName)

  /* Values for type record of FCGI_Header */
  val FCGI_BEGIN_REQUEST     =  1
  val FCGI_ABORT_REQUEST     =  2
  val FCGI_END_REQUEST       =  3
  val FCGI_PARAMS            =  4
  val FCGI_STDIN             =  5
  val FCGI_STDOUT            =  6
  val FCGI_STDERR            =  7
  val FCGI_DATA              =  8
  val FCGI_GET_VALUES        =  9
  val FCGI_GET_VALUES_RESULT = 10
  val FCGI_UNKNOWN_TYPE      = 11

  /* Mask for flags component of FCGI_BeginRequestBody */
  val FCGI_KEEP_CONN  = 1

  /* Values for role component of FCGI_BeginRequestBody  */
  val FCGI_RESPONDER  = 1
  val FCGI_AUTHORIZER = 2
  val FCGI_FILTER     = 3

  val FCGI_VERSION = 1
  val FCGI_REQUEST_COMPLETE = 0
  
  val READ_TIMEOUT = 120000
}

class FastCGIGateway {
  import FastCGIGateway._

  var keepAlive: Boolean = false
  var connectionFactory: ConnectionFactory = _
  
  @throws(classOf[Throwable])
  override protected def finalize {
    super.finalize
    destroy
  }

  @throws(classOf[ServletException])
  @throws(classOf[IOException])
  def service(cgienv: FastCGIEnv, req: HttpServletRequest, res: HttpServletResponse) {
    try {
      connectionFactory.getConnection foreach {fcgiSocket =>
        fcgiSocket.setSoTimeout(READ_TIMEOUT)

        try {
          val cgiout = new BufferedOutputStream(fcgiSocket.getOutputStream)

          writeCgiRecord(cgiout, FCGI_BEGIN_REQUEST, 8)

          val role = FCGI_RESPONDER
          cgiout.write(role >> 8)
          cgiout.write(role)
          cgiout.write(if (keepAlive) FCGI_KEEP_CONN else 0) // flags
          for (i <- 0 until 5) {
            cgiout.write(0)
          }

          setRequestHeaders(cgienv, cgiout)

          // --- read POST content and write to cgi
          writeCgiRecord(cgiout, FCGI_PARAMS, 0)
          val reqin = req.getInputStream
          val buf = new Array[Byte](4096)
          var numRead = 0
          var hasStdin = false
          while ({numRead = reqin.read(buf); numRead > 0}) {
            hasStdin = true
            writeCgiRecord(cgiout, FCGI_STDIN, numRead)
            cgiout.write(buf, 0, numRead)
          }
          if (hasStdin) {
            writeCgiRecord(cgiout, FCGI_STDIN, 0)
          }
          cgiout.flush

          // --- begin write to resout
          val cgiin = new FastCGIInputStream(fcgiSocket.getInputStream)
          CGIUtil.flowResponse(res, cgiin)
        } finally {
          connectionFactory.releaseConnection(fcgiSocket)
        }
      }
    } catch {
      case ex: IOException => throw ex
    }
  }

  @throws(classOf[IOException])
  private def setRequestHeaders(cgiEnv: FastCGIEnv, cgiout: OutputStream) {
    for ((k, v) <- cgiEnv.envs) {
      setRequestHeader(cgiout, k, v)
    }
  }

  @throws(classOf[IOException])
  private def setRequestHeader(cgiout: OutputStream, key: String, value: String)  {
    if (value != null) {
      log.fine("fastcgi request header: " + key + ": " + value)

      val keyBytes = key.getBytes
      val valBytes = value.getBytes

      // * use bytes.length instead of string length to avoid char encoding issue
      val keylen = keyBytes.length
      val vallen = valBytes.length

      var len = keylen + vallen

      if (keylen < 0x80) {
        len += 1
      } else {
        len += 4
      }

      if (vallen < 0x80) {
        len += 1
      } else {
        len += 4
      }

      writeCgiRecord(cgiout, FCGI_PARAMS, len)

      if (keylen < 0x80) {
        cgiout.write(keylen)
      } else {
        cgiout.write(0x80 | (keylen >> 24))
        cgiout.write(keylen >> 16)
        cgiout.write(keylen >> 8)
        cgiout.write(keylen)
      }

      if (vallen < 0x80) {
        cgiout.write(vallen)
      } else {
        cgiout.write(0x80 | (vallen >> 24))
        cgiout.write(vallen >> 16)
        cgiout.write(vallen >> 8)
        cgiout.write(vallen)
      }

      cgiout.write(keyBytes)
      cgiout.write(valBytes)
    }
  }

  @throws(classOf[IOException])
  private def writeCgiRecord(cgiout: OutputStream, tpe: Int, lenContent: Int) {
    val id = 1
    val lenPadding = 0

    cgiout.write(FCGI_VERSION)
    cgiout.write(tpe)
    cgiout.write(id >> 8)
    cgiout.write(id)
    cgiout.write(lenContent >> 8)
    cgiout.write(lenContent)
    cgiout.write(lenPadding)
    cgiout.write(0)
  }

  def destroy {
  }

  /**
   * @Note: unlike cgi, fcgi headers must be sent in _one_ packet,
   * leading or trailing zero length packets are allowed.
   */
  class FastCGIInputStream(cgiin: InputStream) extends InputStream {

    private var contentLen = 0
    private var paddingLen = 0
    var isDead = false

    @throws(classOf[IOException])
    def read: Int = {
      do {
        if (contentLen > 0) {
          contentLen -= 1
          return cgiin.read
        }
      } while (readNext)

      -1
    }

    @throws(classOf[IOException])
    private def readNext: Boolean = {
      if (cgiin == null) {
        return false
      }

      if (paddingLen > 0) {
        cgiin.skip(paddingLen)
        paddingLen = 0
      }
      
      var version = 0
      while ({version = cgiin.read; version >= 0}) {
        val tpe = cgiin.read
        
        val id = (cgiin.read << 8) + cgiin.read
        val lcontent = (cgiin.read << 8) + cgiin.read
        val lpadding = cgiin.read
        cgiin.read // reserved

        tpe match {
          case FCGI_END_REQUEST =>
            val appStatus = (cgiin.read << 24) + (cgiin.read << 16) + (cgiin.read << 8) + (cgiin.read)
            val protocolStatus = cgiin.read

            log.fine("fcgiSocket: FCGI_END_REQUEST(appStatus:" + appStatus + ", protocolStatus:" + protocolStatus + ")")

            if (appStatus != 0) {
              isDead = true
            }

            if (protocolStatus != FCGI_REQUEST_COMPLETE) {
              isDead = true
            }

            cgiin.skip(3) // reserved
            return false
            
          case FCGI_STDOUT =>
            log.fine("fcgiSocket: FCGI_STDOUT(length:" + lcontent + ", padding:" + lpadding + ")")

            if (lcontent == 0) {
              if (lpadding > 0) {
                cgiin.skip(lpadding)
              }
            } else {
              contentLen = lcontent
              paddingLen = lpadding
              return true
            }

          case FCGI_STDERR =>
            log.fine("fcgiSocket: FCGI_STDERR(length:" + lcontent + ", padding:" + lpadding + ")")

            val buf = new Array[Byte](lcontent)
            cgiin.read(buf, 0, lcontent)
            log.warning(new String(buf, 0, lcontent))

            if (lpadding > 0) {
              cgiin.skip(lpadding)
            }

          case _ =>
            log.warning("fcgiSocket: Unknown Protocol(" + tpe + ")")

            isDead = true
            cgiin.skip(lcontent + lpadding)
        }
      }

      isDead = true

      false
    }
  }
}
