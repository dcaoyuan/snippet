/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.aiotrade.servlets.fcgi

import javax.servlet.ServletContext
import javax.servlet.http.HttpServletRequest
import org.aiotrade.servlets.util.CGIEnvironment

class FastCGIEnv(req: HttpServletRequest, context: ServletContext
) extends CGIEnvironment(req, context, false) {

  override protected def setScriptEnv: Boolean = {
    val scriptName = req.getServletPath
    //log.fine("FCGI file: " + scriptName)
    val pathInfo = req.getPathInfo
    val pathTranslated = req.getRealPath(scriptName)
    envs += ("PATH_INFO" -> pathInfo)
    //log.fine("PATH_INFO: " + pathInfo)
    envs += ("PATH_TRANSLATED" -> pathTranslated)
    envs += ("SCRIPT_FILENAME" -> req.getRealPath(scriptName))

    envs += ("DOCUMENT_ROOT" -> req.getRealPath("/"))

    true
  }

}
