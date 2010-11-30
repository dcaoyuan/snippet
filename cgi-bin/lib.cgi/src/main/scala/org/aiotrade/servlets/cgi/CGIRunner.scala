/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.aiotrade.servlets.cgi

import java.io.BufferedOutputStream
import java.io.File
import java.io.IOException
import java.io.InputStream
import java.util.logging.Level
import java.util.logging.Logger
import javax.servlet.http.HttpServletResponse
import org.aiotrade.servlets.util.CGIUtil
import org.aiotrade.servlets.util.IOTools
import org.aiotrade.servlets.util.StreamLogger
import scala.collection.mutable.HashMap

/**
 * Encapsulates the knowledge of how to run a CGI script, given the
 * script's desired environment and (optionally) input/output streams
 *
 * <p>
 *
 * Exposes a <code>run</code> method used to actually invoke the
 * CGI.
 *
 * </p>
 * <p>
 *
 * The CGI environment and settings are derived from the information
 * passed to the constuctor.
 *
 * </p>
 * <p>
 *
 * The input and output streams can be set by the <code>setInput</code>
 * and <code>setResponse</code> methods, respectively.
 * </p>
 *
 * @version   $Revision: 1.4 $, $Date: 2006/09/06 16:02:28 $
 *
 *  Creates a CGIRunner and initializes its environment, working
 *  directory, and query parameters.
 *  <BR>
 *  Input/output streams (optional) are set using the
 *  <code>setInput</code> and <code>setResponse</code> methods,
 *  respectively.
 *
 * @param  command  string full path to command to be executed
 * @param  env      Hashtable with the desired script environment
 * @param  wd       File with the script's desired working directory
 * @param  params   ArrayList with the script's query command line
 *                  paramters as strings
 */
class CGIRunner(
  /** script/command to be executed */
  command: String,
  /** environment used when invoking the cgi script */
  envs: HashMap[String, String],
  /** working directory used when invoking the cgi script */
  wd: File,
  /** command line parameters to be passed to the invoked script */
  params: Seq[String]
) {
  import CGIRunner._

  /** stdin to be passed to cgi script */
  private var reqin: InputStream = _
  /** response object used to set headers & get output stream */
  private var res: HttpServletResponse = _
  /** boolean tracking whether this object has enough info to run */
  private var readyToRun = false

  updateReadyStatus


  /**
   * Checks & sets ready status
   */
  protected def updateReadyStatus {
    readyToRun = (command != null && envs != null && wd != null && params != null && res != null)
  }

  /**
   * Gets ready status
   *
   * @return   false if not ready (<code>run</code> will throw
   *           an exception), true if ready
   */
  protected def isReady = readyToRun

  /**
   * Sets HttpServletResponse object used to set headers and send
   * output to
   *
   * @param  response   HttpServletResponse to be used
   *
   */
  def setResponse(res: HttpServletResponse) {
    this.res = res
    updateReadyStatus
  }

  /**
   * Sets standard input to be passed on to the invoked cgi script
   *
   * @param  stdin   InputStream to be used
   *
   */
  def setInput(reqin: InputStream) {
    this.reqin = reqin
    updateReadyStatus
  }

  /**
   * Converts a Map to a String array by converting each
   * key/value pair in the Map to a String in the form
   * "key=value"
   *
   * @param  m   Map to convert
   *
   * @return     converted string array
   *
   * @exception  NullPointerException   if a hash key has a null value
   *
   */
  @throws(classOf[NullPointerException])
  protected def hashToStringArray(m: HashMap[String, String]): Array[String] = {
    (for ((k, v) <- m) yield {k + "=" + v}).toArray
  }

  /**
   * Executes a CGI script with the desired environment, current working
   * directory, and input/output streams
   *
   * <p>
   * This implements the following CGI specification recommedations:
   * <UL>
   * <LI> Servers SHOULD provide the "<code>query</code>" component of
   *      the script-URI as command-line arguments to scripts if it
   *      does not contain any unencoded "=" characters and the
   *      command-line arguments can be generated in an unambiguous
   *      manner.
   * <LI> Servers SHOULD set the AUTH_TYPE metavariable to the value
   *      of the "<code>auth-scheme</code>" token of the
   *      "<code>Authorization</code>" if it was supplied as part of the
   *      request header.  See <code>getCGIEnvironment</code> method.
   * <LI> Where applicable, servers SHOULD set the current working
   *      directory to the directory in which the script is located
   *      before invoking it.
   * <LI> Server implementations SHOULD define their behavior for the
   *      following cases:
   *     <ul>
   *     <LI> <u>Allowed characters in pathInfo</u>:  This implementation
   *             does not allow ASCII NUL nor any character which cannot
   *             be URL-encoded according to internet standards;
   *     <LI> <u>Allowed characters in path segments</u>: This
   *             implementation does not allow non-terminal NULL
   *             segments in the the path -- IOExceptions may be thrown;
   *     <LI> <u>"<code>.</code>" and "<code>..</code>" path
   *             segments</u>:
   *             This implementation does not allow "<code>.</code>" and
   *             "<code>..</code>" in the the path, and such characters
   *             will result in an IOException being thrown;
   *     <LI> <u>Implementation limitations</u>: This implementation
   *             does not impose any limitations except as documented
   *             above.  This implementation may be limited by the
   *             servlet container used to house this implementation.
   *             In particular, all the primary CGI variable values
   *             are derived either directly or indirectly from the
   *             container's implementation of the Servlet API methods.
   *     </ul>
   * </UL>
   * </p>
   *
   * @exception IOException if problems during reading/writing occur
   *
   * @see    java.lang.Runtime#exec(String command, String[] envp,
   *                                File dir)
   */
  @throws(classOf[IOException])
  def run {
    if (!isReady) {
      throw new IOException(this.getClass.getName + ": not ready to run.")
    }

    log.fine("runCGI(envp=[" + envs + "], command=" + command + ")")
    
    if ((command.indexOf(File.separator + "." + File.separator) >= 0) ||
        (command.indexOf(File.separator + "..") >= 0) ||
        (command.indexOf(".." + File.separator) >= 0)
    ) {
      throw new IOException(this.getClass.getName +
                            "Illegal Character in CGI command " +
                            "path ('.' or '..') detected.  Not " +
                            "running CGI [" + command + "].")
    }

    /* original content/structure of this section taken from
     * http://developer.java.sun.com/developer/bugParade/bugs/4216884.html
     * with major modifications by Martin Dengler
     */
    //create query arguments
    var cmdAndArgs = new StringBuffer
    if (command.indexOf(" ") < 0) {
      cmdAndArgs.append(command)
    } else {
      // Spaces used as delimiter, so need to use quotes
      cmdAndArgs.append("\"")
      cmdAndArgs.append(command)
      cmdAndArgs.append("\"")
    }

    for (param <- params) {
      cmdAndArgs.append(" ")
      if (param.indexOf(" ") < 0) {
        cmdAndArgs.append(param)
      } else {
        // Spaces used as delimiter, so need to use quotes
        cmdAndArgs.append("\"")
        cmdAndArgs.append(param)
        cmdAndArgs.append("\"")
      }
    }

    var proc: Process = null
    var c = -1
    try {
      val rt = Runtime.getRuntime
      proc = rt.exec(cmdAndArgs.toString, hashToStringArray(envs), wd)

      // --- read POST content and write to cgi
      val sContentLength = envs.getOrElse("CONTENT_LENGTH", "")
      if (sContentLength != "") {
        // it should be a POST, flow post form content to cgi
        if (reqin != null) {
          val cgiout = new BufferedOutputStream(proc.getOutputStream)
          IOTools.flow(reqin, cgiout)
          cgiout.flush
          cgiout.close
        } else {
          log.warning("There seems POST content that needs to be processed, but no stdin passed to me.")
        }
      }

      (new Thread(new StreamLogger(proc.getErrorStream, log))).start

      /* we want to wait for the process to exit, Process.waitFor
       * is useless in our situation; see
       * http://developer.java.sun.com/developer/bugParade/bugs/4223650.html
       */
      val cgiin = proc.getInputStream
      var isRunning = true
      while (isRunning) {
        try {
          CGIUtil.flowResponse(res, cgiin)

          proc.exitValue // Throws exception if alive

          isRunning = false
        } catch {
          case ex: IllegalThreadStateException =>
            try {
              Thread.sleep(500)
            } catch {case _: InterruptedException =>}
        }
      } //replacement for Process.waitFor
      cgiin.close
    } catch {
      case ex: IOException => log.log(Level.SEVERE, "Caught exception ", ex); throw ex
    } finally {
      log.finest("Running finally block")
      if (proc != null) {
        proc.destroy
        proc = null
      }
    }
  }

} //class CGIRunner

object CGIRunner {
  val log = Logger.getLogger(classOf[CGIRunner].getName)

  def main(args: Array[String]) {
    val cgi = new CGIRunner(null, null, null, null)
  }
}

