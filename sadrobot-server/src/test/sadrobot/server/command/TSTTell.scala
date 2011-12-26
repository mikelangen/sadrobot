package test.sadrobot.server.command

import test.sadrobot.server.Tester
import test.sadrobot.server.TSTSadRobotServer

object TSTTell extends Tester {
  var res : String = ""
  
  def runTestSuite() = {
    
    val uid1 = "yui";
    val uid2 = "foo";
    val uid3 = "bar";
    val uid4 = "~!~";
    
    testTell
  }
  
  def testTell = {
    
    val uid1 = "yui";
    val uid2 = "foo";
    val uid3 = "bar";
    val uid4 = "~!~";
    
    val msg2 = "foLloW tHe HapPy PatH!"
    val msg3 = "Do not meddle in the affairs of wizards, for they are subtle and quick to anger."
    val msg4 = "TRUE! nervous, very, very dreadfully nervous I had been and am; but why WILL you say that I am mad?"
    
    TSTSadRobotServer.connections find (_.uid == uid1) match {
      case Some(conn) => {

        conn.out.println("tell " + uid2 + " " + msg2)
        Thread.sleep(1000)
        res = conn.in.readLine
        info("Received " + res);
        assert(res == "You tell " + uid2 + ": " + msg2)
        
        conn.out.println("tell " + uid3 + " " + msg3)
        Thread.sleep(1000)
        res = conn.in.readLine
        info("Received " + res);
        assert(res == "You tell " + uid3 + ": " + msg3)
        
        conn.out.println("tell " + uid4 + " " + msg4)
        Thread.sleep(1000)
        res = conn.in.readLine
        info("Received " + res);
        assert(res == "You tell " + uid4 + ": " + msg4)
        
        confirmTellReceipt(uid1,uid2,msg2);
        confirmTellReceipt(uid1,uid3,msg3);
        confirmTellReceipt(uid1,uid4,msg4);
        
        confirmNoData(TSTSadRobotServer.connections)
        
      }
      case None => {
        error("UID: " + uid1 + " not found.")
        assert(false)
      }
    }
  }
  
  def confirmTellReceipt(fromUID : String, toUID : String, msg : String) = {
      TSTSadRobotServer.connections find(_.uid == toUID) match {
        case Some(conn) => {          
          res = conn.in.readLine
          info("Received " + res);
          assert(res == fromUID + " tells you: " + msg)
        }
        case None => {
          error("UID: " + toUID + " not found.")
          assert(false)
        }
      }
  }

}