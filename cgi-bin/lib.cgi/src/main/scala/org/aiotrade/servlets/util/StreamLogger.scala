package org.aiotrade.servlets.util

import java.io.BufferedReader
import java.io.IOException
import java.io.InputStream
import java.io.InputStreamReader
import java.util.logging.Level
import java.util.logging.Logger

class StreamLogger(is: InputStream, log: Logger) extends Runnable {

  private val reader = new BufferedReader(new InputStreamReader(is))

  override def run {
    var lineCount = 0
    var sb = new StringBuilder
    var line: String = null
    try {
      while ({line = reader.readLine; line != null}) {
        sb append line
        lineCount += 1
      }
    } catch {
      case ex: IOException => log.log(Level.WARNING, "runCGI stderr logger", ex)
    } finally {
      if (reader != null) {
        try {
          reader.close
        } catch {case ce: IOException => log.log(Level.WARNING, "runCGI stderr logger", ce)}
      }
    }

    if (lineCount > 0) {
      log.info("runCGI stderr: " + sb)
      log.finest("runCGI: " + lineCount + " lines received on stderr")
    }

  }
}
