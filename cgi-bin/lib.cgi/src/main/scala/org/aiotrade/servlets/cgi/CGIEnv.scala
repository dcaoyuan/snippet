package org.aiotrade.servlets.cgi

/**
 * Encapsulates the CGI environment and rules to derive
 * that environment from the servlet container and request information.
 *
 * <p>
 * </p>
 *
 * @version  $Revision: 1.4 $, $Date: 2006/09/06 16:02:28 $
 * @since    Tomcat 4.0
 *
 */
import java.io.File
import java.io.FileOutputStream
import java.io.IOException
import java.io.InputStream
import java.net.URLDecoder
import java.util.StringTokenizer
import javax.servlet.ServletContext
import javax.servlet.http.HttpServletRequest
import org.aiotrade.servlets.util.Globals
import org.aiotrade.servlets.util.IOTools
import scala.collection.mutable.ArrayBuffer
import org.aiotrade.servlets.util.CGIEnvironment

@throws(classOf[IOException])
class CGIEnv(req: HttpServletRequest, context: ServletContext,
             defaultCgi: String,
             cgiPathPrefix: String,
             stripedRequestURI: String,
             parameterEncoding: String,
             passShellEnvironment: Boolean
) extends CGIEnvironment(req, context, passShellEnvironment) {
  /** object used to ensure multiple threads don't try to expand same file */
  private val expandFileLock = new Object

  /** cgi command to be invoked */
  var command: String = null

  /**
   * Creates a CGIEnvironment and derives the necessary environment,
   * query parameters, working directory, cgi command, etc.
   *
   * @param  req       HttpServletRequest for information provided by
   *                   the Servlet API
   * @param  context   ServletContext for information provided by the
   *                   Servlet API
   *
   */
  /** real file system directory of the enclosing servlet's web app */
  var webAppRootDir = context.getRealPath("/")
  /** tempdir for context - used to expand scripts in unexpanded wars */
  private val tmpDir = context.getAttribute(Globals.WORK_DIR_ATTR).asInstanceOf[File]

  /** context path of enclosing servlet */
  private val contextPath = req.getContextPath
  /** servlet URI of the enclosing servlet */
  private val servletPath = req.getServletPath
  /** pathInfo for the current request */
  // If getPathInfo returns null, must be using extension mapping
  // In this case, pathInfo should be same as servletPath
  private val pathInfo = {val x = req.getPathInfo; if (x == null) servletPath else x}

  /** cgi command's command line parameters */
  val cmdLineParams = new ArrayBuffer[String]
  // If the request method is GET, POST or HEAD and the query string
  // does not contain an unencoded "=" this is an indexed query.
  // The parsed query string becomes the command line parameters
  // for the cgi command.
  req.getMethod match {
    case "GET" | "POST" | "HEAD" =>
      val qs = req.getQueryString
      if (qs != null && qs.indexOf("=") == -1) {
        val qsTokens = new StringTokenizer(qs, "+")
        while (qsTokens.hasMoreTokens) {
          cmdLineParams += URLDecoder.decode(qsTokens.nextToken, parameterEncoding)
        }
      }
    case _ =>
  }

  var workingDir: File = _

  /**
   * Resolves core information about the cgi script.
   *
   * <p>
   * Example URI:
   * <PRE> /servlet/cgigateway/dir1/realCGIscript/pathinfo1 </PRE>
   * <ul>
   * <LI><b>path</b> = $CATALINA_HOME/mywebapp/dir1/realCGIscript
   * <LI><b>scriptName</b> = /servlet/cgigateway/dir1/realCGIscript
   * <LI><b>cgiName</b> = /dir1/realCGIscript
   * <LI><b>name</b> = realCGIscript
   * </ul>
   * </p>
   * <p>
   * CGI search algorithm: search the real path below
   *    &lt;my-webapp-root&gt; and find the first non-directory in
   *    the getPathTranslated("/"), reading/searching from left-to-right.
   *</p>
   *<p>
   *   The CGI search path will start at
   *   webAppRootDir + File.separator + cgiPathPrefix
   *   (or webAppRootDir alone if cgiPathPrefix is
   *   null).
   *</p>
   *<p>
   *   cgiPathPrefix is defined by setting
   *   this servlet's cgiPathPrefix init parameter
   *
   *</p>
   *
   * @param pathInfo       String from HttpServletRequest.getPathInfo
   * @param webAppRootDir  String from context.getRealPath("/")
   * @param contextPath    String as from
   *                       HttpServletRequest.getContextPath
   * @param servletPath    String as from
   *                       HttpServletRequest.getServletPath
   * @param cgiPathPrefix  subdirectory of webAppRootDir below which
   *                       the web app's CGIs may be stored; can be null.
   *                       The CGI search path will start at
   *                       webAppRootDir + File.separator + cgiPathPrefix
   *                       (or webAppRootDir alone if cgiPathPrefix is
   *                       null).  cgiPathPrefix is defined by setting
   *                       the servlet's cgiPathPrefix init parameter.
   *
   *
   * @return
   * <ul>
   * <li>
   * <code>path</code> -    full file-system path to valid cgi script,
   *                        or null if no cgi was found
   * <li>
   * <code>scriptName</code> -
   *                        CGI variable SCRIPT_NAME; the full URL path
   *                        to valid cgi script or null if no cgi was
   *                        found
   * <li>
   * <code>cgiName</code> - servlet pathInfo fragment corresponding to
   *                        the cgi script itself, or null if not found
   * <li>
   * <code>name</code> -    simple name (no directories) of the
   *                        cgi script, or null if no cgi was found
   * </ul>
   *
   * @since Tomcat 4.0
   */
  protected def findCGI(pathInfo: String, $webAppRootDir: String,
                        contextPath: String, servletPath: String,
                        cgiPathPrefix: String
  ): Array[String] = {

    var viaDefaultCGI: String = null

    var webAppRootDir = $webAppRootDir
    if ((webAppRootDir != null) &&
        (webAppRootDir.lastIndexOf(File.separator) == (webAppRootDir.length - 1))) {
      //strip the trailing "/" from the webAppRootDir
      webAppRootDir = webAppRootDir.substring(0, (webAppRootDir.length - 1))
    }

    if (cgiPathPrefix != null) {
      webAppRootDir = webAppRootDir + File.separator + cgiPathPrefix
    }

    log.fine("findCGI: path=" + pathInfo + ", " + webAppRootDir)

    var currentLocation = new File(webAppRootDir)
    val scriptName = if (servletPath.indexOf('/') == 0) {
      servletPath.substring(1, servletPath.length)
    } else servletPath
    val paths = pathInfo.split('/')
    var dirWalker = if (servletPath != null && servletPath != "") {
      scriptName :: paths.toList
    } else paths.toList

    log.fine("findCGI: webAppRootDir=" + currentLocation)
    var pathTokenConsumed = false
    while (!currentLocation.isFile && !dirWalker.isEmpty) {
      log.fine("findCGI: currentLoc=" + currentLocation)
      val name = dirWalker.head
      currentLocation = new File(currentLocation, name)
      dirWalker = dirWalker.tail
      if (scriptName != name) {
        pathTokenConsumed = true
      }
    }

    if (!currentLocation.isFile && defaultCgi != null) {
      currentLocation = new File(webAppRootDir, defaultCgi)
      if (currentLocation.isFile) {
        viaDefaultCGI = defaultCgi
      }
      log.fine("findCGI: use default cgi at " + currentLocation)
    }

    if (!currentLocation.isFile) {
      Array[String](null, null, null, null, null)
    } else {
      log.fine("findCGI: FOUND cgi at " + currentLocation)
      val path = currentLocation.getAbsolutePath
      val name = currentLocation.getName
      val cginame = if (pathTokenConsumed) {
        currentLocation.getParent.substring(webAppRootDir.length) + servletPath + name
      } else ""
      val scriptname = if (contextPath == ".") {
        servletPath + cginame
      } else {
        contextPath + servletPath + cginame
      }

      log.fine("findCGI calc: name=" + name + ", path=" + path + ", scriptname=" + scriptname + ", cginame=" + cginame)

      Array(path, scriptname, cginame, name, viaDefaultCGI)
    }

  }

  /**
   * Constructs the CGI environment to be supplied to the invoked CGI
   * script; relies heavliy on Servlet API methods and findCGI
   *
   * @param    req request associated with the CGI
   *           invokation
   *
   * @return   true if environment was set OK, false if there
   *           was a problem and no environment was set
   */
  @throws(classOf[IOException])
  override protected def setScriptEnv: Boolean = {

    // Add the CGI environment variables
    var sPathInfoOrig: String = null
    var sPathTranslatedOrig: String = null
    var sPathInfoCGI: String = null
    var sPathTranslatedCGI: String = null

    sPathInfoOrig = this.pathInfo
    sPathInfoOrig = if (sPathInfoOrig == null) "" else sPathInfoOrig

    sPathTranslatedOrig = req.getPathTranslated
    sPathTranslatedOrig = if (sPathTranslatedOrig == null) "" else sPathTranslatedOrig

    if (webAppRootDir == null) {
      // The app has not been deployed in exploded form
      webAppRootDir = tmpDir.toString
      expandCGIScript
    }

    val Array(sCGIFullPath,
              sCGIScriptName,
              sCGIFullName,
              sCGIName,
              sViaDefaultCGI) = findCGI(sPathInfoOrig,
                                        webAppRootDir,
                                        contextPath,
                                        servletPath,
                                        cgiPathPrefix)

    if (sCGIFullPath == null || sCGIScriptName == null ||
        sCGIFullName == null || sCGIName == null) {
      return false
    }

    /*-
     * PATH_INFO should be determined by using sCGIFullName:
     * 1) Let sCGIFullName not end in a "/" (see method findCGI)
     * 2) Let sCGIFullName equal the pathInfo fragment which
     *    corresponds to the actual cgi script.
     * 3) Thus, PATH_INFO = request.getPathInfo.substring(
     *                      sCGIFullName.length)
     *
     * (see method findCGI, where the real work is done)
     *
     */
    sPathInfoCGI = if (pathInfo == null || (sViaDefaultCGI == null && pathInfo.length <= 0)) "" else pathInfo
    envs.put("PATH_INFO", sPathInfoCGI)

    /*-
     * PATH_TRANSLATED must be determined after PATH_INFO (and the
     * implied real cgi-script) has been taken into account.
     *
     * The following example demonstrates:
     *
     * servlet info   = /servlet/cgigw/dir1/dir2/cgi1/trans1/trans2
     * cgifullpath    = /servlet/cgigw/dir1/dir2/cgi1
     * path_info      = /trans1/trans2
     * webAppRootDir  = servletContext.getRealPath("/")
     *
     * path_translated = servletContext.getRealPath("/trans1/trans2")
     *
     * That is, PATH_TRANSLATED = webAppRootDir + sPathInfoCGI
     * (unless sPathInfoCGI is null or blank, then the CGI
     * specification dictates that the PATH_TRANSLATED metavariable
     * SHOULD NOT be defined.
     *
     */
    sPathTranslatedCGI = if (sPathInfoCGI != null && sPathInfoCGI != "") {
      context.getRealPath(sCGIFullPath + sPathInfoCGI)
    } else null
    if (sPathTranslatedCGI == null || sPathTranslatedCGI == "") {
      //NOOP
    } else {
      envs.put("PATH_TRANSLATED", nullToBlank(sPathTranslatedCGI))
    }


    envs.put("SCRIPT_NAME", if (sViaDefaultCGI != null) "" else nullToBlank(sCGIScriptName))


    val fCGIFullPath = new File(sCGIFullPath)
    command = fCGIFullPath.getCanonicalPath

    envs.put("X_TOMCAT_SCRIPT_PATH", command)  //for kicks

    envs.put("SCRIPT_FILENAME", command)  //for PHP


    /** cgi command's desired working directory */
    workingDir = new File(command.substring(0, command.lastIndexOf(File.separator)))

    true
  }

  /**
   * If the stripRequestURI is specified and the requestURI
   * starts with it, the stripRequestURI is lopped off in
   * in the returned value. Else, the requestURI is returned as is
   */
  protected def stripRequestURI(reqURI: String): String = {

    if (stripedRequestURI == null ||
        stripedRequestURI.intern == "".intern ||
        !reqURI.startsWith(stripedRequestURI)
    ) {
      return reqURI
    }

    val index = reqURI.indexOf(stripedRequestURI)
    if (index <= 0) {
      reqURI
    } else {
      reqURI.substring(index)
    }
  }

  /**
   * Extracts requested resource from web app archive to context work
   * directory to enable CGI script to be executed.
   */
  protected def expandCGIScript {
    val srcPath = new StringBuffer
    val destPath = new StringBuffer
    var is: InputStream = null

    // paths depend on mapping
    if (cgiPathPrefix == null) {
      srcPath.append(pathInfo)
      is = context.getResourceAsStream(srcPath.toString)
      destPath.append(tmpDir)
      destPath.append(pathInfo)
    } else {
      // essentially same search algorithm as findCGI
      srcPath.append(cgiPathPrefix)
      val pathWalker = new StringTokenizer(pathInfo, "/")
      // start with first element
      while (pathWalker.hasMoreElements && (is == null)) {
        srcPath.append("/")
        srcPath.append(pathWalker.nextElement)
        is = context.getResourceAsStream(srcPath.toString)
      }
      destPath.append(tmpDir)
      destPath.append("/")
      destPath.append(srcPath)
    }

    if (is == null) {
      // didn't find anything, give up now
      log.warning("expandCGIScript: source '" + srcPath + "' not found")
      return
    }

    val f = new File(destPath.toString)
    if (f.exists) {
      // Don't need to expand if it already exists
      return
    }

    // create directories
    val dirPath = new String(destPath.toString.substring(0, destPath.toString.lastIndexOf("/")))
    val dir = new File(dirPath)
    dir.mkdirs

    try {
      expandFileLock synchronized {
        // make sure file doesn't exist
        if (f.exists) {
          return
        }

        // create file
        if (!f.createNewFile) {
          return
        }
        val fos = new FileOutputStream(f)

        // copy data
        IOTools.flow(is, fos)
        is.close
        fos.close
        log.info("expandCGIScript: expanded '" + srcPath + "' to '" + destPath + "'")
      }
    } catch {
      case ioe: IOException =>
        // delete in case file is corrupted
        if (f.exists) {
          f.delete
        }
    }
  }

  /**
   * Print important CGI environment information in a easy-to-read HTML
   * table
   *
   * @return  HTML string containing CGI environment info
   *
   */
  override def toString = {

    val sb = new StringBuffer

    sb.append("<TABLE border=2>")

    sb.append("<tr><th colspan=2 bgcolor=grey>")
    sb.append("CGIEnvironment Info</th></tr>")

    sb.append("<tr><td>Validity:</td><td>")
    sb.append(isValid)
    sb.append("</td></tr>")

    if (isValid) {
      for ((k, v) <- envs) {
        sb.append("<tr><td>")
        sb.append(k)
        sb.append("</td><td>")
        sb.append(blankToString(v, "[will be set to blank]"))
        sb.append("</td></tr>")
      }
    }

    sb.append("<tr><td colspan=2><HR></td></tr>")

    sb.append("<tr><td>Derived Command</td><td>")
    sb.append(nullToBlank(command))
    sb.append("</td></tr>")

    sb.append("<tr><td>Working Directory</td><td>")
    if (workingDir != null) {
      sb.append(workingDir.toString)
    }
    sb.append("</td></tr>")

    sb.append("<tr><td>Command Line Params</td><td>")
    for (param <- cmdLineParams) {
      sb.append("<p>")
      sb.append(param)
      sb.append("</p>")
    }
    sb.append("</td></tr>")

    sb.append("</TABLE><p>end.")

    sb.toString
  }

}