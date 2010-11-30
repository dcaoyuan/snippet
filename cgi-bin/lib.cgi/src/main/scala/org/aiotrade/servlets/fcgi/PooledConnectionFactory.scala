/*
 * (c) 2009 Julien Rialland, and the jFastCGI project developpers.
 * 
 * Released under BSD License (see license.txt)
 *  
 *   $Id$ 
 */
package org.aiotrade.servlets.fcgi

import java.io.IOException
import java.net.Socket

import org.apache.commons.pool.ObjectPool
import org.apache.commons.pool.PoolableObjectFactory
import org.apache.commons.pool.impl.GenericObjectPool

/**
 * A connection factory that handles multiple connections, using an underlying connection pool provided
 * by commons-pool. (http://commons.apache.org/pool/)
 * 
 * @author jrialland
 *
 */
class PooledConnectionFactory(poolableObjectFactory: PoolableObjectFactory) extends ConnectionFactory {

  private var pool: ObjectPool = new GenericObjectPool(poolableObjectFactory)

  /**
   * get e connection from the pool.
   */
  @throws(classOf[IOException])
  def getConnection: Option[Socket] = {
    try {
      val socket = pool.borrowObject.asInstanceOf[Socket]
      if (socket != null) Some(socket) else None
    } catch {
      case ex: IOException => throw ex
    }
  }

  /**
   * returns a connection to the pool.
   *
   */
  def releaseConnection(socket: Socket) {
    try {
      pool.returnObject(socket)
    } catch {
      case ex: IOException =>
    }
  }

  def getPool: ObjectPool = {
    pool
  }

  def setPool(pool: ObjectPool) {
    this.pool = pool
  }
}
