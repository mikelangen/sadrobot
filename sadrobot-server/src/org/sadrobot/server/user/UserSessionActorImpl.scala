package org.sadrobot.server.user

import java.io.IOException

import net.lag.naggati.{IoHandlerActorAdapter, MinaMessage, ProtocolError}

import org.apache.mina.core.buffer.IoBuffer
import org.apache.mina.core.session.{IdleStatus, IoSession}

//import scala.actors.Actor
//import scala.collection.mutable.{Set, SynchronizedSet, HashSet}

import org.sadrobot.common.command.Command
import org.sadrobot.common.user.User
import org.sadrobot.common.user.UserSessionActor
import org.sadrobot.common.util.LogUtil
import org.sadrobot.common.Request
import org.sadrobot.common.user.AllUserSessions

import org.sadrobot.server.Response
import org.sadrobot.server.SadRobotServer


/******************************************************************************
  * class UserSessionActorImpl
  *****************************************************************************
  *
  * One of these is instantiated for each session / connection
  * 
  * Modification history:
  *
  */
class UserSessionActorImpl(val session: IoSession) extends UserSessionActor with LogUtil {
  
  var user : User = _
  
  debug("Instantiating UserSessionActorImpl for session: " + session)
  
  start

  def act = {
    loop {
      react {
        case MinaMessage.MessageReceived(msg) => {
          if (user == null) 
            { userLogin(msg.asInstanceOf[Request]) }
          else 
            { handleRequest(msg.asInstanceOf[Request]) }
        }
        case MinaMessage.SessionClosed => { 
          info("session closed: " + session)
          exit() 
        }
        case MinaMessage.SessionIdle(status) => { 
          info("closing idle session: " + session + " status: " + status)
          disconnectSession
          exit()
        }
        case MinaMessage.SessionOpened => { 
          info("opening session " + session)
          sreplyln("SadRobot test server "+SadRobotServer.VERSION) 
          sreply  ("Username: ")
        }

        case MinaMessage.ExceptionCaught(cause) => {
          cause.getCause match {
            case e: ProtocolError => {sreplyln("Error: " + e.getMessage); error("Error: " + e.getMessage) }
            case i: IOException   => {sreplyln("Error: " + i.getMessage); error("Error: " + i.getMessage) }
            case _                => {sreplyln("Exception: " + cause);    error("Exception: " + cause) }
          }
        }
      }
    }
  }
  
  def disconnectSession() = {
    if (!session.isClosing) {
      info("disconnecting session for user: " + user.username)
      execCommand("broadcast",user.username + " has disconnected.")
      AllUserSessions -= this
      session.close 
    }
  }
  
  //
  // trivial placeholder for initializing a user
  // until I tackle authentication.
  //
  private def userLogin(req: Request): Unit = {
	req.data match {
	  case Some(uname : String) => {
	      this.user = new User(uname)
	      
	      SadRobotServer.cmdMap
	      
	      grantCommand("tell")
	      grantCommand("who")
	      grantCommand("quit")
	      grantCommand("help")
	      grantCommand("broadcast")
	      grantCommand("topicjoin")
   	      grantCommand("topiccreate")
   	      grantCommand("topiclist")
   	      grantCommand("topicusers")
	      
	      AllUserSessions += this
		  info("User: " + uname + " logged in.")
		  sreplyln("Welcome, " + uname)
		  
		  execCommand("broadcast",uname + " has just logged in.")
	  }
	  case None => {
		  sreplyln("hrm?")
		  sreply  ("Username: ")
	  }
	}
  }
    
  def sreply(str: String) : String = {
    debug((if (user != null) 
             {"[reply to user] " + user.username + "| "}
           else
             {"[reply to sess] "} + session + " ")
          + str ) 
    session.write(new Response(IoBuffer.wrap(str.getBytes)))  
    str
  }
  
  private def grantCommand(cmdVerb : String) = {
    SadRobotServer.getCommand(cmdVerb) match {
      case Some(c : Command) => { this.user.addCommand(c.cmdVerb,c) }
      case None              => { warn("Command: |" + cmdVerb + 
                                       "| not loaded.  Unable to grant command to user " + 
                                       user.username) } 
	}
  }
  
  private def handleRequest(req: Request) = {
    user.getCommand(req.cmdVerb) match {
      case Some(c : Command) => { 
        try {
          c.exec(this,req) match {
            case Some(s : String) => {info(c.cmdVerb + " returned |" + s + "|")}
            case None => {warn("no response received from cmd: " + c.cmdVerb)}
          }
        } catch {
          case ex: Exception => {
            error("Unexpected error when calling exec on plugin: " + c.cmdVerb,ex)
          }
        }
      }
      case None => {
        warn("Unable to find command: " + req.cmdVerb + " for user: " + user.username)
        warn("If this is a valid command (not a typo), the user hasn't been granted access.")
        sreplyln("hrm?")
      }
    }
  }
    
  /**
    *
    * This will execute the command regardless of whether the user has access to 
    * it or not.  Do not make this available in the UserSessionActor common
    * interface (abstract class) without first considering exactly what commands you
    * want plugin authors to have access to, as this will executute ANY loaded 
    * command.
    * 
    * use handleRequest if you only want to execute a command if the user has 
    * access to it.  Use execCommand when you want to execute a command regardless.
    * 
    * 2011/11/15 Mike Langen
    * 
    */
  private def execCommand(cmdVerb : String, data : String) : Unit = { execCommand(cmdVerb,None,data) }
  
  private def execCommand(cmdVerb : String, 
                          mods : Option[List[String]], 
                          data : String) : Unit = {
    
    var response : Option[String] = None
    
    SadRobotServer.getCommand(cmdVerb) match {
      case Some(c : Command) => { 
        try {
          mods match {
            case Some(modList : List[String]) => { response = c.exec(this,modList,data) }
            case None                         => { response = c.exec(this,data) }
          }
          response match {
            case Some(s: String) => { info(cmdVerb + " returned |" + s + "|")  }
            case None => { warn("no response received from cmd: " + cmdVerb)}
          }
        } catch {
          case ex: Exception => {
            error("Unexpected error when calling exec on plugin: " + cmdVerb,ex)
          }
        }
      }
      case None => { 
        warn("Command: " + cmdVerb + " not loaded.  Unable to execute cmd: " + 
             cmdVerb + " : " + data) 
      } 
	}
  }
  
}