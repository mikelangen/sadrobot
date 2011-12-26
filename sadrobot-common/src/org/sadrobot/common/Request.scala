package org.sadrobot.common

import org.sadrobot.common.util.LogUtil

/****************************************************************************** 
  * class Request
  *****************************************************************************
  * Modification history:
  * 
  * Reverted to regular class from case class.  Had to explicitly add "val"
  * before attributes in order to keep the eclipse scala parser from complaining.
  * 
  * 11/04/2011 -- Mike Langen
  * 
  * Spent today trying to figure out how to incorporate the Validation trait.  
  * 
  * Should Command extend it, or Request?  It seems like a Request ought to 
  * do validation, but a shouldn't a Command do it too?  Since a Command "isA"
  * Request, shouldn't Command inherit Request, then Request would inherit 
  * Validator?
  * 
  * So I spent a while making a Command a subclass of Request, and having
  * the codec instantiate the correct Command subclass (WhoCommand, TellCommand, etc)
  * instead of a generic "Request" object.
  * 
  * Ultimately, I want have one obvious way to map command "verbs"
  * to command objects.  Right now, I envision a Map that has the verb as 
  * the key and the command object as the value.  (i.e. "tell" => TellCommand)
  * 
  * For now, I'm keeping Request simple and leaving commands subclasses as
  * Singletons that take Requests as parameters.  It ultimately
  * just boils down to the fact that that implementation feels easier to 
  * understand and it more inherently obvious when doing maintenance.  One of 
  * the kinda sucky things about inheritance is the fact that when you're working
  * on a subclass, (which is where you'll be spending all your time), it isn't
  * necessarily obviously what parent attributes you need to use in order to 
  * implement your class without stopping to go do research.  In this case,
  * having the object right there on the command line (Request), makes it obvious.
  * 
  * 11/04/2011 -- Mike Langen
  * 
  * Extracted interface and moved to common
  * 
  * 11/08/2011 -- Mike Langen
  * 
  */
class Request (val cmdVerb   : String, 
               val modifiers : Option[List[String]], 
               val data      : Option[String])
              
/**
  * Created a companion object for Request, and moved static methods
  * requestAllData and RequestCommand into it from Codec
  * 
  * 11/14/2011 Mike Langen
  *  
  */
object Request extends LogUtil {
  //
  // Default message decoding.  take the first
  // token and stash it in Request.command.  Take the whole
  // line (including the command) and stash it in Request.data
  //
  //
  def requestAllData(tokens : List[String]) : Request = {
    debug("parsing data: " + tokens.mkString(" "))
    
    if (tokens.isEmpty)  
      {new Request("",None,None);}
    else 
      {new Request(tokens.head,None, Some(tokens.mkString(" ")))}
  }
  
  //
  // Take the first token on the line and stash it in Request.command
  // take numMods tokens after that and stash them the Request.modifiers
  // take everything else and stash it in Request.data
  //
  // Ex.  Given the incoming line:  
  //
  // tell foo don't be a jerkwad 
  //
  // called with numMods set to: 1
  //
  // Request.command is  : tell
  // Request.modifiers is: foo
  // Request.data is     : don't be a jerkwad
  //
  // This is very simple at the moment, and will probably become a key
  // place where incoming data would need to be validated / sanitized.
  //
  // Mike Langen
  //
  // Review this code to see what it will do (make sure it acts sane)
  // when someone sends in some absurd number for numMods
  //
  // Also, will I eventually need to be able to account for a variable number of
  // modifiers?  I can't think of any non-hokey way to do that.  It seems to me 
  // that the only sane way of doing that is to ask the individual command 
  // subclasses to specify a number of modifiers of zero, and handle parsing
  // the modifiers themselves.  They'll still be there, available in Request.data, 
  // they just won't be nicely pre-parsed.
  //
  // 11/14/2011 -- Mike Langen
  //
  def requestCommand(tokens : List[String], numMods : Integer) : Request = {
    if (!tokens.isEmpty)
    {
    	debug("cmd is: " + tokens.head)

    	new Request(tokens.head,

    	        // Building a request obj.  Use the return val of this if
    	        // as the first parameter (modifiers)
    	    
    			if (tokens.length > numMods+1) {
    				Some(tokens.slice(1,numMods+1))
    			}
    			else if (tokens.length > 1) {
    				Some(tokens.tail)
    			}
    			else {
    				None
    			},
    			
    			// Use return val of this if as second request parameter (data)

    			if (tokens.length > numMods+1) {
    				Some(tokens.slice(numMods+1,tokens.length).mkString(" "))
    			}
    			else {
    				None
    			})
    }
    else
    {
      new Request("",None,None)
    }
  }

}