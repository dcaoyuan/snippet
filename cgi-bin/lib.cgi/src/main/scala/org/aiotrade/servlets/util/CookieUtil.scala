/*
 * Copyright 2005 Sun Microsystems, Inc.  All Rights Reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Sun designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Sun in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Sun Microsystems, Inc., 4150 Network Circle, Santa Clara,
 * CA 95054 USA or visit www.sun.com if you need additional information or
 * have any questions.
 */

package org.aiotrade.servlets.util

import java.text.SimpleDateFormat
import java.util.StringTokenizer
import java.util.TimeZone
import javax.servlet.http.Cookie
import scala.collection.mutable.ArrayBuffer

object CookieUtil {
  //
  // Since the positive and zero max-age have their meanings,
  // this value serves as a hint as 'not specify max-age'
  //
  private val MAX_AGE_UNSPECIFIED = -1


  //
  // date format used by Netscape's cookie draft
  //
  private val NETSCAPE_COOKIE_DATE_FORMAT = "EEE',' dd-MMM-yyyy HH:mm:ss 'GMT'"

  //
  // constant strings represent set-cookie header token
  //
  private val SET_COOKIE = "set-cookie:"
  private val SET_COOKIE2 = "set-cookie2:"



  val assignors: Map[String, (Cookie, String, String) => Unit] = Map(
    "comment" -> (
      (cookie: Cookie, attrName: String, attrValue: String) => {
        if (cookie.getComment == null) cookie.setComment(attrValue)
      }
    ),

    /* "commenturl" -> (
     (cookie: Cookie, attrName: String, attrValue: String) => {
     if (cookie.getCommentURL() == null) cookie.setCommentURL(attrValue)
     }
     ), */

    /* "discard" -> (
     (cookie: Cookie, attrName: String, attrValue: String) =>{
     cookie.setDiscard(true)
     }
     ), */

    "domain" -> (
      (cookie: Cookie, attrName: String, attrValue: String) => {
        if (cookie.getDomain() == null) cookie.setDomain(attrValue)
      }
    ),

    "max-age" -> (
      (cookie: Cookie, attrName: String, attrValue: String) => {
        try {
          val maxage = attrValue.toInt
          if (cookie.getMaxAge == MAX_AGE_UNSPECIFIED) cookie.setMaxAge(maxage)
        } catch {
          case ex: NumberFormatException =>
            throw new IllegalArgumentException("Illegal cookie max-age attribute")
        }
      }
    ),

    "path" -> (
      (cookie: Cookie, attrName: String, attrValue: String) => {
        if (cookie.getPath() == null) cookie.setPath(attrValue)
      }
    ),

    /* "port" -> (
     (cookie: Cookie, attrName: String, attrValue: String) => {
     if (cookie.getPortlist() == null) cookie.setPortlist(attrValue)
     }
     ), */

    "secure" -> (
      (cookie: Cookie, attrName: String, attrValue: String) => {
        cookie.setSecure(true)
      }
    ),

    "version" -> (
      (cookie: Cookie, attrName: String, attrValue: String) => {
        try {
          val version = Integer.parseInt(attrValue)
          cookie.setVersion(version);
        } catch {
          case ex: NumberFormatException =>
            throw new IllegalArgumentException("Illegal cookie version attribute")
        }
      }
    ),

    "expires" -> ( // Netscape only
      (cookie: Cookie, attrName: String, attrValue: String) =>
      if (cookie.getMaxAge() == MAX_AGE_UNSPECIFIED) {
        cookie.setMaxAge(expiryDate2DeltaSeconds(attrValue).toInt)
      }
    )
  )


  def parse($header: String): List[Cookie] = {
    val version = guessCookieVersion($header)

    // if header start with set-cookie or set-cookie2, strip it off
    val header = if (startsWithIgnoreCase($header, SET_COOKIE2)) {
      $header.substring(SET_COOKIE2.length)
    } else if (startsWithIgnoreCase($header, SET_COOKIE)) {
      $header.substring(SET_COOKIE.length)
    } else $header


    val cookies = new ArrayBuffer[Cookie]
    // The Netscape cookie may have a comma in its expires attribute,
    // while the comma is the delimiter in rfc 2965/2109 cookie header string.
    // so the parse logic is slightly different
    if (version == 0) {
      // Netscape draft cookie
      val cookie = parseInternal(header)
      cookie.setVersion(0)
      cookies += cookie
    } else {
      // rfc2965/2109 cookie
      // if header string contains more than one cookie,
      // it'll separate them with comma
      val cookieStrings = splitMultiCookies(header)
      for (cookieStr <- cookieStrings) {
        val cookie = parseInternal(cookieStr);
        cookie.setVersion(1)
        cookies += cookie
      }
    }

    cookies.toList
  }

  private def parseInternal(header: String): Cookie = {
    var cookie: Cookie = null
    var namevaluePair: String = null

    val tokenizer = new StringTokenizer(header, ";")

    // there should always have at least on name-value pair;
    // it's cookie's name
    try {
      namevaluePair = tokenizer.nextToken
      val index = namevaluePair.indexOf('=')
      if (index != -1) {
        val name = namevaluePair.substring(0, index).trim
        val value = namevaluePair.substring(index + 1).trim
        cookie = new Cookie(name, stripOffSurroundingQuote(value))
      } else {
        // no "=" in name-value pair; it's an error
        throw new IllegalArgumentException("Invalid cookie name-value pair");
      }
    } catch {
      case ex: NoSuchElementException =>
        throw new IllegalArgumentException("Empty cookie header string");
    }

    // remaining name-value pairs are cookie's attributes
    while (tokenizer.hasMoreTokens) {
      namevaluePair = tokenizer.nextToken
      val index = namevaluePair.indexOf('=')
      val (name, value) =
        if (index != -1) {
          (namevaluePair.substring(0, index).trim,
           namevaluePair.substring(index + 1).trim)
        } else {
          (namevaluePair.trim, null)
        }

      // assign attribute to cookie
      assignAttribute(cookie, name, value)
    }

    cookie
  }


  /*
   * try to guess the cookie version through set-cookie header string
   */
  private def guessCookieVersion($header: String): Int = {
    val header = $header.toLowerCase
    if (header.indexOf("expires=") != -1) {
      // only netscape cookie using 'expires'
      0
    } else if (header.indexOf("version=") != -1) {
      // version is mandatory for rfc 2965/2109 cookie
      1
    } else if (header.indexOf("max-age") != -1) {
      // rfc 2965/2109 use 'max-age'
      1
    } else if (startsWithIgnoreCase(header, SET_COOKIE2)) {
      // only rfc 2965 cookie starts with 'set-cookie2'
      1
    } else 0
  }

  /*
   * Split cookie header string according to rfc 2965:
   *   1) split where it is a comma;
   *   2) but not the comma surrounding by double-quotes, which is the comma
   *      inside port list or embeded URIs.
   *
   * @param header            the cookie header string to split
   *
   * @return                  list of strings; never null
   *
   */
  private def splitMultiCookies(header: String): ArrayBuffer[String] = {
    val cookies = new ArrayBuffer[String]();

    var quoteCount = 0
    var i = 0
    var j = 0
    while (i < header.length) {
      val c = header.charAt(i)
      if (c == '"') quoteCount += 1
      if (c == ',' && (quoteCount % 2 == 0)) {      // it is comma and not surrounding by double-quotes
        cookies += header.substring(j, i)
        j = i + 1
      }
      i += 1
    }

    cookies += header.substring(j)

    cookies
  }



  /*
   * @param dateString        a date string in format of
   *                          "EEE',' dd-MMM-yyyy HH:mm:ss 'GMT'",
   *                          which defined in Netscape cookie spec
   *
   * @return                  delta seconds between this cookie's creation
   *                          time and the time specified by dateString
   */
  private def expiryDate2DeltaSeconds(dateString: String): Long = {
    val df = new SimpleDateFormat(NETSCAPE_COOKIE_DATE_FORMAT);
    df.setTimeZone(TimeZone.getTimeZone("GMT"));

    try {
      val date = df.parse(dateString);
      (date.getTime - System.currentTimeMillis) / 1000
    } catch {case e: Exception => 0}
  }


  private def assignAttribute(cookie: Cookie, attrName: String, $attrValue: String) {
    // strip off the surrounding "-sign if there's any
    val attrValue = stripOffSurroundingQuote($attrValue)

    assignors.get(attrName.toLowerCase) foreach {assign =>
      assign(cookie, attrName, attrValue)
    }
  }


  private def stripOffSurroundingQuote(str: String): String = {
    if (str != null && str.length() > 0 &&
        str.charAt(0) == '"' && str.charAt(str.length() - 1) == '"') {
      str.substring(1, str.length() - 1)
    } else str
  }

  private def startsWithIgnoreCase(s: String, start: String): Boolean = {
    if (s == null || start == null) return false;

    if (s.length() >= start.length() &&
        start.equalsIgnoreCase(s.substring(0, start.length()))) {
      return true;
    }

    return false;
  }


  def main(args: Array[String]) {
    val c1 = "Set-Cookie: trac_auth=79696a95d27f78dcf80d8365a64b50c2; Path=/;"
    val c2 = "Set-Cookie: trac_session=a247bcfeb4a798bdea55d1d0; expires=Sat, 02-Jan-2010 21:44:53 GMT; Path=/;"

    val cookie1 = parse(c1)
    val cookie2 = parse(c2)
    cookie1 foreach {x => println(x.getName + "=" + x.getValue)}
    cookie2 foreach {x => println(x.getName + "=" + x.getValue)}
  }
}
