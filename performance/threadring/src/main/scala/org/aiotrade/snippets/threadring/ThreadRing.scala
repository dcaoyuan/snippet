/*
 * ThreadRing.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package org.aiotrade.snippets.threadring

import scala.actors.Actor
import scala.actors.Actor._
import java.net.{HttpURLConnection, URL, MalformedURLException, ProtocolException}
import java.io.{BufferedReader, InputStreamReader, IOException}

object ThreadRing {
  val N_TOKENS = 20000
  val N_PROCS  = 10000

  val N_FIB = 10000000
  var t0 = 0L

  val sb = new StringBuilder(10000 * 10)

  def main(args: Array[String]) : Unit = {
    t0 = System.currentTimeMillis
    fibLoop(1000, 10000)
  }

  def main1(args: Array[String]) : Unit = {
    t0 = System.currentTimeMillis

    // --- create the actors
    val rings = (0 to N_PROCS) map (i => new Ring(i + 1)) toArray

    // --- hook them up
    rings foreach {t => 
      val nextIdx = t.label % rings.length
      t.next = rings(nextIdx)
      t.start
    }

    // -- game begin
    t0 = System.currentTimeMillis
    rings(0) ! N_TOKENS
    collector !? "begin"
  }

  class Ring(val label: Int) extends Actor {
    var next: Ring = null
    def act = {
      loop {
        react {
          case 0 =>
            println("Token ring done! " + (System.currentTimeMillis - t0) / 1000.0 + "s")
            println(label)

          case n: Int =>
            next ! n - 1
            //sb.append(n).append('\n')
            println("token:" + n)
            //val res = http(n, "http://dot2.lightpole.net/")
            val result = http(n, "http://dev3.lightpole.net/user/login")
            collector ! (n, result)
            //fib(N_FIB)
        }
      }
    }
  }

  val collector = actor {
    var count = 0
    loop {
      react {
        case (n: Int, result @ _) =>
          count += 1
          result match {
            case (dt: Long, r @ _) =>
              println("token:" + n + ", result:" + r.toString + ", in " + dt / 1000.0 + "s. Total finished:" + count)
            case _ =>
              println("token:" + n + ", result:" + result.toString + ", total finished:" + count)
          }
          if (count == N_TOKENS) {
            println("With thread Time: " + (System.currentTimeMillis - t0) / 1000.0 + "s")
            System.exit(0)
          }
               
        case msg: String => println("collector: " + msg)
      }
    }
  }

  def fibLoop(n: Int, nFib: Int) {
    if (n == 0) {
      println("No thread time: " + (System.currentTimeMillis - t0) / 1000.0 + "s, " + (System.currentTimeMillis - t0) / N_TOKENS.toDouble + "ms per token")
    } else {
      fib(nFib)
      fibLoop(n - 1, nFib)
    }
  }

  def fibLoop(n: Int): Unit = fibLoop(n, N_FIB)

  def fib(n: Int): Int = fib(n, 1, 0)
  def fib(n: Int, next: Int, res: Int): Int = n match {
    case 0 => res
    case _ => fib(n - 1, res + next, next)
  }

  def http(token: Int, urlStr: String) {
    var conn:HttpURLConnection = null
    try {
      val url = new URL(urlStr)
      val t0 = System.currentTimeMillis
      conn = url.openConnection.asInstanceOf[HttpURLConnection]
      conn.setRequestMethod("GET")
      conn.setDoOutput(true)
      conn.setConnectTimeout(30000)
      conn.setReadTimeout(30000)

      val sb = new StringBuilder
      conn.connect
      val reader = new BufferedReader(new InputStreamReader(conn.getInputStream))
      var line = reader.readLine
      while (line != null) {
        sb.append(line).append('\n')
        line = reader.readLine
      }
      (System.currentTimeMillis - t0, sb.length)
    } catch {
      case e => (System.currentTimeMillis - t0, "error(" + e.getMessage + ")")
    } finally {
      if (conn != null) conn.disconnect
    }
  }

}
