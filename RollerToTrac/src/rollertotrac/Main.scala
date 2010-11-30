/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package rollertotrac

import java.sql.DriverManager

object Main {
  val debug = true

  val importingAuthor = "dcaoyuan"

  val rollerDbUser = "roller"
  val rollerDbPasswd = "roller"
  val rollerdb = "/Users/dcaoyuan/mysites/rsync/www/derby/RollerDB"
  val tracdb = "/Users/dcaoyuan/mysites/rsync/www/trac/db/trac.db"

  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]): Unit = {
    val rollerConn = connectToDerby(rollerdb)
    val tracConn = connectToSqllite(tracdb)

    if (rollerConn == null || tracConn == null) {
      println("Error on connecting to db")
      return
    }

    var anchors = Set[String]()

    def importBlogs {
      tracConn.setAutoCommit(false)
      val tracStmt = tracConn.prepareStatement(
        "INSERT INTO fullblog_posts (name, version, title, body, publish_time, version_time, version_author, author, categories) VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?)"
      )

      val rollerStmt = rollerConn.createStatement
      rollerStmt.execute("select e.*, u.USERNAME as AUTHOR, c.name as CATEGORY from ROLLER.WEBLOGENTRY as e, ROLLER.ROLLERUSER as u, ROLLER.WEBLOGCATEGORY as c where e.USERID = u.ID and e.CATEGORYID = c.ID")
      val rs = rollerStmt.getResultSet
      var i = 0
      while (rs.next) {
        val name = rs.getString("ANCHOR")
        val version = 1
        val title = rs.getString("TITLE")
        val body = "{{{\n#!html\n" + rs.getString("TEXT") + "\n}}}"

        val publish_time = rs.getDate("PUBTIME").getTime / 1000
        val version_time = publish_time
        //val version_comment = ""
        val version_author = rs.getString("AUTHOR")
        val author = version_author
        val categories = rs.getString("CATEGORY")

        if (author == importingAuthor) {
          val fields = Array(i, name, version, title, "\n" + body.substring(0, Math.min(80, body.length)) + "\n", publish_time, version_time, version_author, author, categories)
          println(fields.mkString(","))
        
          tracStmt.setString(1, name)
          tracStmt.setInt(2, version)
          tracStmt.setString(3, title)
          tracStmt.setString(4, body)
          tracStmt.setLong(5, publish_time)
          tracStmt.setLong(6, version_time)
          tracStmt.setString(7, version_author)
          tracStmt.setString(8, author)
          tracStmt.setString(9, categories)

          tracStmt.addBatch

          anchors += name
        
          i += 1
        }
      }

      val counts = tracStmt.executeBatch
      println("\nWill insert " + counts.length + " blogs")
      if (!debug) {
        tracConn.commit
      }

    } // end importBlog


    def importComments {
      tracConn.setAutoCommit(false)
      val tracStmt = tracConn.prepareStatement(
        "INSERT INTO fullblog_comments (name, number, comment, author, time) VALUES(?, ?, ?, ?, ?)"
      )

      val rollerStmt = rollerConn.createStatement
      rollerStmt.execute("select c.*, e.ANCHOR as ANCHOR from ROLLER.ROLLER_COMMENT as c, ROLLER.WEBLOGENTRY as e where c.SPAM = 0 and e.ID = c.ENTRYID")
      val rs = rollerStmt.getResultSet
      var nameToNumber = Map[String, Int]()
      var i = 0
      while (rs.next) {
        val name = rs.getString("ANCHOR")
        val number = nameToNumber.getOrElse(name, 0) + 1
        nameToNumber += (name -> number)
        val comment = rs.getString("CONTENT")
        val author = rs.getString("NAME")
        val time = rs.getDate("POSTTIME").getTime / 1000

        if (anchors.contains(name)) {
          val fields = Array(i, name, number, "\n" + comment.substring(0, Math.min(80, comment.length)) + "\n", author, time)
          println(fields.mkString("", ",", "\n"))

          tracStmt.setString(1, name)
          tracStmt.setInt(2, number)
          tracStmt.setString(3, comment)
          tracStmt.setString(4, author)
          tracStmt.setLong(5, time)

          tracStmt.addBatch
          
          i += 1
        }
      }

      val counts = tracStmt.executeBatch
      println("\nWill insert " + counts.length + " comments")
      if (!debug) {
        tracConn.commit
      }

      rs.close
      rollerStmt.close
      tracStmt.close
    } // end importComment

    importBlogs
    importComments
    rollerConn.close
    tracConn.close

  }

  def connectToSqllite(jdbcUrl: String) = {
    Class.forName("org.sqlite.JDBC")

    DriverManager.getConnection("jdbc:sqlite:" + jdbcUrl)
  }

  def connectToDerby(jdbcUrl: String) = {
    Class.forName("org.apache.derby.jdbc.EmbeddedDriver")
    DriverManager.getConnection("jdbc:derby:" + jdbcUrl + ";user=" + rollerDbUser + ";password=" + rollerDbPasswd + ";")
  }

}
