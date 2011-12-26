package org.sadrobot.server

import org.apache.mina.core.buffer.IoBuffer
import org.apache.mina.core.session.{IdleStatus, IoSession}
import org.apache.mina.filter.codec._
import net.lag.naggati._
import net.lag.naggati.Steps._
import org.sadrobot.common.util.LogUtil
import org.sadrobot.common.Request
import org.sadrobot.common.command.Command

/******************************************************************************
  * trait Validator
  *****************************************************************************
  * 
  * Modification history:
  *
  */
trait Validator {
  def isValid(validationFailedMsg: String) : Boolean = true
}
               
/******************************************************************************
  * class Response
  *****************************************************************************
  * Modification history:
  * 
  * 11/04/2011 -- Mike Langen
  * 
  * Reverted to regular class from case class.  Had to explicitly add "val"
  * before attributes in order to keep the eclipse scala parser from complaining.
  * 
  */
class Response(val data: IoBuffer)

/******************************************************************************
  * object Codec
  *****************************************************************************
  * Modification history:
  *
  */
object Codec extends LogUtil {
  
  val encoder = new ProtocolEncoder {
    def encode(session: IoSession, message: AnyRef, out: ProtocolEncoderOutput) = {      
      out.write(message.asInstanceOf[Response].data)
    }

    // no-op, required by ProtocolEncoder trait
    def dispose(session: IoSession): Unit = {} 
  }
  
  val decoder = new Decoder(readLine(true, "ISO-8859-1") { line => 
    {
       val tokens : Array[String] = line.split(' ');
      
       SadRobotServer.getCommand(tokens.head.toLowerCase()) match {
         case Some(c : Command) => { state.out.write(Request.requestCommand(List.fromArray(tokens),c.numModifiers)); End }
         case None =>              { state.out.write(Request.requestAllData(List.fromArray(tokens)));                End }
       }
      
      //tokens.head.toLowerCase() match {
      //  case "tell" =>  { state.out.write(requestCommand(List.fromArray(tokens),1)); End }
      //  case "say"  =>  { state.out.write(requestCommand(List.fromArray(tokens),0)); End }
      //  case "quit" =>  { state.out.write(requestCommand(List.fromArray(tokens),0)); End }
      //  case "who"  =>  { state.out.write(requestCommand(List.fromArray(tokens),0)); End }
      //  case "help" =>  { state.out.write(requestCommand(List.fromArray(tokens),1)); End }
      //  case "man"  =>  { state.out.write(requestCommand(List.fromArray(tokens),1)); End }
      //  case _      =>  { state.out.write(requestAllData(List.fromArray(tokens)));   End }
      //}
    }
  })
    
}
