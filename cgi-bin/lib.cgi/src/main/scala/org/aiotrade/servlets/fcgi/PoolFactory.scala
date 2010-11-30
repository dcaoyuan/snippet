/*
 * (c) 2009 Julien Rialland, and the jFastCGI project developpers.
 * 
 * Released under BSD License (see license.txt)
 *  
 *   $Id$ 
 */
package org.aiotrade.servlets.fcgi

import java.net.InetAddress
import java.net.Socket
import java.net.UnknownHostException
import java.util.Random
import java.util.regex.Pattern

import org.apache.commons.pool.BasePoolableObjectFactory
import scala.collection.mutable.ArrayBuffer


/**
 * Implements PoolableObjectFactory so it creates tcp connections on demand using pool configuration.
 * 
 * @author jrialland
 *
 */

class PoolFactory extends BasePoolableObjectFactory {
  /**
   * embeds ip + port into a single structure.
   *
   * @author jrialland
   *
   */
  private class ConnDesc(var addr: InetAddress, var port: Int) {
    override def toString = addr.getHostName + ":" + port
  }
  
  private val random = new Random
  
  /**
   * List of configured host/port pairs.
   *
   */
  private var addresses = new ArrayBuffer[ConnDesc]

  /**
   * build a ConnDesc from a "host:port" string.
   * @param s
   * @return
   */
  private def makeConndesc(s: String): ConnDesc = {
    val m = Pattern.compile("([^:]+):([1-9][0-9]*)$").matcher(s)
    if (m.matches()) {
      try {
        val addr = InetAddress.getByName(m.group(1))
        val port = Integer.parseInt(m.group(2))
        return new ConnDesc(addr, port)
      } catch {
        case ex: UnknownHostException => throw new IllegalArgumentException(ex)
      }
    } else {
      throw new IllegalArgumentException(s)
    }
  }

  /**
   * builds a new socket using one of the descriptors.
   */
  @ throws(classOf[Exception])
  override def makeObject: Object = {
    val index = random.nextInt(addresses.size - 1)
    val desc = addresses(index)
    return new Socket(desc.addr, desc.port)
  }

  def addAddress(address: String) {
    this.addresses += makeConndesc(address)
  }

  def addAdresses(addresses: Iterable[String]) {
    for (s <- addresses) {
      this.addresses += makeConndesc(s)
    }
  }

  /**
   * may simplify future spring integration...
   *
   * @param address
   */
  def setAddress(address: String) {
    addAddress(address)
  }

  /**
   * may simplify future spring integration...
   *
   * @param addresses
   */
  def setAddresses(addresses: Iterable[String]) {
    addAdresses(addresses)
  }
}
