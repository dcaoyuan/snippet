

/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2007 Sun Microsystems, Inc. All rights reserved.
 *
 * Portions Copyright Apache Software Foundation.
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
 */


package org.aiotrade.servlets.util


/**
 * Global constants that are applicable to multiple packages within Catalina.
 *
 * @author Craig R. McClanahan
 * @version $Revision: 1.13 $ $Date: 2007/10/11 23:15:50 $
 */

object Globals {

  /**
   * The servlet context attribute under which we store the alternate
   * deployment descriptor for this web application
   */
  val ALT_DD_ATTR = "org.apache.catalina.deploy.alt_dd"

  /**
   * The request attribute under which we store the array of X509Certificate
   * objects representing the certificate chain presented by our client,
   * if any.
   */
  val CERTIFICATES_ATTR = "javax.servlet.request.X509Certificate"

  /**
   * SSL Certificate Request Attributite.
   */
  val SSL_CERTIFICATE_ATTR = "org.apache.coyote.request.X509Certificate"

  /**
   * The request attribute under which we store the name of the cipher suite
   * being used on an SSL connection (as an object of type
   * java.lang.String).
   */
  val CIPHER_SUITE_ATTR = "javax.servlet.request.cipher_suite"


  /**
   * The servlet context attribute under which we store the class loader
   * used for loading servlets (as an object of type java.lang.ClassLoader).
   */
  val CLASS_LOADER_ATTR = "org.apache.catalina.classloader"

  /**
   * Request dispatcher state.
   */
  val DISPATCHER_TYPE_ATTR = "org.apache.catalina.core.DISPATCHER_TYPE"

  /**
   * Request dispatcher path.
   */
  val DISPATCHER_REQUEST_PATH_ATTR = "org.apache.catalina.core.DISPATCHER_REQUEST_PATH"

  /**
   * The JNDI directory context which is associated with the context. This
   * context can be used to manipulate static files.
   */
  val RESOURCES_ATTR = "org.apache.catalina.resources"
  val ALTERNATE_RESOURCES_ATTR = "org.apache.catalina.alternateResources"


  /**
   * The servlet context attribute under which we store the class path
   * for our application class loader (as an object of type String),
   * delimited with the appropriate path delimiter for this platform.
   */
  val CLASS_PATH_ATTR = "org.apache.catalina.jsp_classpath"


  /**
   * The request attribute under which we forward a Java exception
   * (as an object of type Throwable) to an error page.
   */
  val EXCEPTION_ATTR = "javax.servlet.error.exception"


  /**
   * The request attribute under which we forward the request URI
   * (as an object of type String) of the page on which an error occurred.
   */
  val EXCEPTION_PAGE_ATTR = "javax.servlet.error.request_uri"


  /**
   * The request attribute under which we forward a Java exception type
   * (as an object of type Class) to an error page.
   */
  val EXCEPTION_TYPE_ATTR = "javax.servlet.error.exception_type"


  /**
   * The request attribute under which we forward an HTTP status message
   * (as an object of type STring) to an error page.
   */
  val ERROR_MESSAGE_ATTR = "javax.servlet.error.message"


  /**
   * The request attribute under which the Invoker servlet will store
   * the invoking servlet path, if it was used to execute a servlet
   * indirectly instead of through a servlet mapping.
   */
  val INVOKED_ATTR =
    "org.apache.catalina.INVOKED"


  /**
   * The request attribute under which we expose the value of the
   * <code>&ltjsp-file&gt</code> value associated with this servlet,
   * if any.
   */
  val JSP_FILE_ATTR = "org.apache.catalina.jsp_file"


  /**
   * The request attribute under which we store the key size being used for
   * this SSL connection (as an object of type java.lang.Integer).
   */
  val KEY_SIZE_ATTR = "javax.servlet.request.key_size"


  /**
   * The servlet context attribute under which the managed bean Registry
   * will be stored for privileged contexts (if enabled).
   */
  val MBEAN_REGISTRY_ATTR = "org.apache.catalina.Registry"


  /**
   * The servlet context attribute under which the MBeanServer will be stored
   * for privileged contexts (if enabled).
   */
  val MBEAN_SERVER_ATTR = "org.apache.catalina.MBeanServer"


  /**
   * The request attribute under which we store the servlet name on a
   * named dispatcher request.
   */
  val NAMED_DISPATCHER_ATTR = "org.apache.catalina.NAMED"


  /**
   * The request attribute under which the request URI of the included
   * servlet is stored on an included dispatcher request.
   */
  val INCLUDE_REQUEST_URI_ATTR =
    "javax.servlet.include.request_uri"


  /**
   * The request attribute under which the context path of the included
   * servlet is stored on an included dispatcher request.
   */
  val INCLUDE_CONTEXT_PATH_ATTR = "javax.servlet.include.context_path"


  /**
   * The request attribute under which the path info of the included
   * servlet is stored on an included dispatcher request.
   */
  val INCLUDE_PATH_INFO_ATTR = "javax.servlet.include.path_info"


  /**
   * The request attribute under which the servlet path of the included
   * servlet is stored on an included dispatcher request.
   */
  val INCLUDE_SERVLET_PATH_ATTR = "javax.servlet.include.servlet_path"


  /**
   * The request attribute under which the query string of the included
   * servlet is stored on an included dispatcher request.
   */
  val INCLUDE_QUERY_STRING_ATTR = "javax.servlet.include.query_string"


  /**
   * The request attribute under which the original request URI is stored
   * on an forwarded dispatcher request.
   */
  val FORWARD_REQUEST_URI_ATTR =
    "javax.servlet.forward.request_uri"


  /**
   * The request attribute under which the original context path is stored
   * on an forwarded dispatcher request.
   */
  val FORWARD_CONTEXT_PATH_ATTR = "javax.servlet.forward.context_path"


  /**
   * The request attribute under which the original path info is stored
   * on an forwarded dispatcher request.
   */
  val FORWARD_PATH_INFO_ATTR = "javax.servlet.forward.path_info"


  /**
   * The request attribute under which the original servlet path is stored
   * on an forwarded dispatcher request.
   */
  val FORWARD_SERVLET_PATH_ATTR = "javax.servlet.forward.servlet_path"


  /**
   * The request attribute under which the original query string is stored
   * on an forwarded dispatcher request.
   */
  val FORWARD_QUERY_STRING_ATTR = "javax.servlet.forward.query_string"


  /**
   * The request attribute under which we forward a servlet name to
   * an error page.
   */
  val SERVLET_NAME_ATTR = "javax.servlet.error.servlet_name"


  /**
   * The name of the cookie used to pass the session identifier back
   * and forth with the client.
   */
  val SESSION_COOKIE_NAME = "JSESSIONID"


  /**
   * The name of the path parameter used to pass the session identifier
   * back and forth with the client.
   */
  val SESSION_PARAMETER_NAME = "jsessionid"


  /**
   * The request attribute under which we forward an HTTP status code
   * (as an object of type Integer) to an error page.
   */
  val STATUS_CODE_ATTR = "javax.servlet.error.status_code"


  /**
   * The subject under which the AccessControlContext is running.
   */
  val SUBJECT_ATTR = "javax.security.auth.subject"


  /**
   * The servlet context attribute under which we record the set of
   * welcome files (as an object of type String[]) for this application.
   */
  val WELCOME_FILES_ATTR = "org.apache.catalina.WELCOME_FILES"


  /**
   * The servlet context attribute under which we store a temporary
   * working directory (as an object of type File) for use by servlets
   * within this web application.
   */
  val WORK_DIR_ATTR = "javax.servlet.context.tempdir"


  /**
   * Has security been turned on?
   */
  val IS_SECURITY_ENABLED = (System.getSecurityManager() != null)


  // START GlassFish 740
  val JSP_PROPERTY_GROUPS_CONTEXT_ATTRIBUTE = "com.sun.jsp.propertyGroups"

  val WEB_XML_VERSION_CONTEXT_ATTRIBUTE = "com.sun.servlet.webxml.version"
  // END GlassFish 740

  // START GlassFish 747
  val JSP_TLD_URI_TO_LOCATION_MAP = "com.sun.jsp.tldUriToLocationMap"
  // END GlassFish 747

  // START GlassFish 896
  val SESSION_TRACKER = "com.sun.enterprise.http.sessionTracker"
  // END GlassFish 896

  /**
   * The name of the cookie used to carry a session's version info
   */
  val SESSION_VERSION_COOKIE_NAME = "JSESSIONIDVERSION"

  /**
   * The name of the path parameter used to carry a session's version info
   */
  val SESSION_VERSION_PARAMETER_NAME = "jsessionidversion"

  val SESSION_VERSION_PARAMETER = "" + SESSION_VERSION_PARAMETER_NAME + "="

  val SESSION_VERSIONS_REQUEST_ATTRIBUTE = "com.sun.enterprise.http.sessionVersions"

  val WRAPPED_REQUEST = "__javax.security.auth.message.request"

  val WRAPPED_RESPONSE = "__javax.security.auth.message.response"


  /**
   * The servlet context attribute under which we store a flag used
   * to mark this request as having been processed by the SSIServlet.
   * We do this because of the pathInfo mangling happening when using
   * the CGIServlet in conjunction with the SSI servlet. (value stored
   * as an object of type String)
   */
  val SSI_FLAG_ATTR = "org.apache.catalina.ssi.SSIServlet"

  /**
   * Request path.
   */
  val CONSTRAINT_URI = "org.apache.catalina.CONSTRAINT_URI"
}