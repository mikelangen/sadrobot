package test.sadrobot.server.command

import scala.collection.mutable.{Set, SynchronizedSet, HashSet, ListBuffer}
import test.sadrobot.server.Tester
import test.sadrobot.server.TSTSadRobotServer

object TSTWho extends Tester {
  def runTestSuite() = {
    testWho
  }
  
  def testWho = {
    var res = "";
    val whoBoilerplateLines = "--- Users currently logged in ---" :: Nil
    
    val uid1 = "yui";
    
    TSTSadRobotServer.connections find (_.uid == uid1) match {
      case Some(conn) => {
        var whoResults = ListBuffer[String]()       
        conn.out.println("who")
        Thread.sleep(1000)
                
        //
        // pull back all the results from the who command
        //
        while (conn.in.ready) { 
          val res = conn.in.readLine
          var nonUserLine=false

          info("Received " + res);
          
          if ((whoBoilerplateLines exists (_==res)) == false) { 
            whoResults find(_==res) match {
              case Some(u) => {
                info("User (" + u + ") found in who listing multiple times.")
                assert(false)
              }
              case None => {
                if (nonUserLine == false) { whoResults += res }
              }
            }
          }
          else
          {
            info("Found expected who boilerplate: " + res)
          }
        }
        
        TSTSadRobotServer.connections.toList filter(_!=uid1) map (_.uid) foreach (uid => {
          if (whoResults.exists(_==uid) == false) {
            info("User (" + uid + ") not found in who listing");
            assert(false)
          }
        })
      
        whoResults foreach (uid => {
          if ((TSTSadRobotServer.connections.toList filter(_!=uid1) map (_.uid) exists(_==uid)) == false) {
            info("Unknown user (" + uid + ") found in who listing.");
            assert(false)
          }
        })
        
        confirmNoData(TSTSadRobotServer.connections)
      }
      case None => {
        error("UID: " + uid1 + " not found.")
        assert(false)
      }
    }
  }
}