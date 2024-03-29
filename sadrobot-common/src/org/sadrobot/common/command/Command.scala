package org.sadrobot.common.command

import org.sadrobot.common.user.UserSessionActor
import org.sadrobot.common.Request

import net.liftweb.json.JsonAST
import net.liftweb.json.JsonAST.JObject
import net.liftweb.json.Printer._


/******************************************************************************
  * abstract class Command
  *****************************************************************************
  *
  * Abstract class Command is subclassed by plugin / extensions classes that wish
  * to add functionality to the main server.  These classes are then provided in 
  * their own jar(s) and added into the classpath (please use the /ext dir)
  * 
  * Each command subclass must provide an implemention of the exec() method.
  * 
  * Each plugin must provide the following file in its jar:
  * 
  *  /META-INF/services/org.sadrobot.common.Command
  *  
  *  Inside this line should be a list of the fully qualified classnames of the new
  *  Command implementation(s).  One classname per line, if you want it to work.
  * 
  * To implement a plugin, the following steps need to occur:
  * 
  * Create a new Jar with a class that subclasses org.sadrobot.common.command.Command
  * 
  * This class must implement the exec() method.  Exec is the main entry point that
  * will be called when the server receives this command.  The commandVerb is passed
  * to the Command class constructor, e.g.
  * 
  * class FooCommand extends Command("foo")
  * 
  * The java.util.ServiceLoader also requires that the new subclass provide a zero 
  * argument constructor.  Hence, don't make it a scala object.  It needs to be a 
  * class.
  * 
  * See: sadrobot-core-commands for some sample command implementations
  * 
  * 11/14/2011 Mike Langen
  *
  */
abstract class Command(val cmdVerb : String, val numModifiers : Integer) {
  
  def exec(issuingUserSession : UserSessionActor, req : Request) : Option[String]
  
  // 
  // Convenience method, translates a String into a request to be
  // processed.  If you just specify one string, it all gets stuffed
  // in request.data.  There is another method that lets you pass
  // in a modifier if you need to as well, e.g. you might call
  //
  // exec(session,"Bazinga!")
  //
  // on BroadcastCommand to broadcast the string
  // "Bazinga!" to everyone.  Or, if you wanted to tell a particular
  // user something, you would call
  //
  // exec(session,List("leonard"),"Stay away from my sister!")
  //
  // on TellCommand
  //
  // 11/14/2011 Mike Langen
  //
  def exec(issuingUserSession : UserSessionActor, req : String) : Option[String] = {
    exec(issuingUserSession,new Request(cmdVerb,None,Some(req)))
  }
  
  def exec(issuingUserSession : UserSessionActor, mods : List[String], req : String) : Option[String] = {
     exec(issuingUserSession,new Request(cmdVerb,Some(mods),Some(req)))   
  }
  
  def renderJson(jsonObj : Option[JObject]) : Option[String] = {
    jsonObj match {
       case Some(jObj : JObject) => { Some(compact(JsonAST.render(jObj))) }
       case None  => { None }
    }
  }
}