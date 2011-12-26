package test.sadrobot.server

import org.sadrobot.server.SadRobotServer
import org.sadrobot.common.util.LogUtil
import test.sadrobot.server.command._

import java.net._
import java.io._
import scala.collection.mutable.{Set, SynchronizedSet, HashSet}

// log4j imports
import org.apache.log4j.PropertyConfigurator

class TSTSocketConn(val listenAddress : String,
                    val listenPort    : Int,
                    val uid : String) {
    val sock = new Socket(listenAddress,listenPort);
    val in   = new BufferedReader(new InputStreamReader(sock.getInputStream()));
    val out  = new PrintWriter(sock.getOutputStream(), true);
}

abstract class Tester extends LogUtil {
  def runTestSuite()
  
  def confirmNoData(connections : HashSet[TSTSocketConn]) = {
    connections foreach( conn => {
       info("verifying no data found for (" + conn.uid + ")")
       assert(conn.in.ready == false)
    })
  }
}

object TSTSadRobotServer extends Tester with LogUtil {

  val userList = "foo" :: "bar" :: "qwerty" :: "poi" :: "yui" :: 
                 "GAR" :: "WER" :: "BXG" :: "BOPX" :: "REQUIEM" ::
                 "@%W@" :: "9823" :: "000000" :: "~!~" :: "');" ::
                 "jesusdoesn'twantmeforasunbeamkurtcobainwasn'taverynicepersonsoithinki'llwriteasongaboutitbutmaybei'lljustpoutinsteadafteralli'msoVERYspecial" ::   
                 Nil
  
  var connections = new HashSet[TSTSocketConn] with SynchronizedSet[TSTSocketConn]
  var res : String = "";
  
  def main(args: Array[String]): Unit = {
    
    info("Running SadRobot Test Suite.")
    info("Starting SadRobotServer")
    SadRobotServer.main(args)
    info("SadRobotServer should be listening on. " + SadRobotServer.listenAddress + ":" + SadRobotServer.listenPort)
    info("Attempting to connect to SadRobotServer")
   
    openConnections
    info(connections.size + " connections opened.")
    
    runTestSuite

    info("sending exit command to each connection.  Placeholder until I write a test for the " +
         "exit command that verifies that no connections hang.")
    connections.foreach( _.out.println("quit") ) 
    
  }
  
  def runTestSuite = {
    TSTTell.runTestSuite()
    TSTWho.runTestSuite()
  }
  
  def openConnections : Boolean = {
    var retVal : Boolean = true;
    for (u <- userList if retVal == true) {
	  connectToServer(u) match {
	    case Some(conn : TSTSocketConn) => {  
	      info("Session: " + conn.sock.toString() + "connected at: " + SadRobotServer.listenAddress + ":" + SadRobotServer.listenPort)
	      connections += conn;
	      confirmConnectionNotifications(u)
	    }
	    case None => {
	      error("Unable to connect to SadRobotServer at user: " + u);
	      retVal = false;
	    }
	  }
	}
    
    retVal;
  }
      
  def connectToServer(uid : String) : Option[TSTSocketConn] = {
    val listenAddress = if (SadRobotServer.listenAddress == "0.0.0.0") { "127.0.0.1" } else { SadRobotServer.listenAddress }
    try {
       val conn = new TSTSocketConn(listenAddress,SadRobotServer.listenPort,uid)
       
       res = conn.in.readLine;
       info("Received: " + res)
       assert(res.indexOf("SadRobot test server " + SadRobotServer.VERSION) >= 0)
       
       var c : Int = 0
       // in this case, we'll never get a newline after "Username: ", 
       // so fall back to read vs readLine
       res = ""
       for (i <- 1 to "Username: ".size if c != -1) {
         c = conn.in.read()
         if (c != -1) { res += c.toChar }
       }
       
       info("Received: " + res + " Bytes read: " + res.size)
       assert(res == "Username: ")
       conn.out.println(uid);
       res = conn.in.readLine;
       info("Sent: " + uid)
       info("Received: " + res)
       assert(res == "Welcome, " + uid)
       Some(conn);
    } 
    catch {
      case e: UnknownHostException => {
        error("Unknown host: " + listenAddress + " : " + SadRobotServer.listenPort);
        error(e)
        None
      }
      case e: IOException => {
        error("IOException when attempting to use socket to: " + listenAddress + " : " + SadRobotServer.listenPort);
        error(e)
        None
      }
      case e => {
        error(e)
        None
      }
    }
  }
    
  def confirmConnectionNotifications(u : String) = {
     connections filter(conn => {conn.uid != u}) foreach (conn => {
       res = conn.in.readLine;
       info("Received: " + res)
       assert(res == u + " has just logged in.")
     })
  }
}