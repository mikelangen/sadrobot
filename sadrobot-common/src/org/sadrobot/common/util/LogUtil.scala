package org.sadrobot.common.util

import org.apache.log4j.Logger
import org.apache.log4j.Level._
import org.apache.log4j.PatternLayout
import org.apache.log4j.ConsoleAppender

/****************************************************************************** 
  * class LogUtil
  *****************************************************************************
  * Modification history:
  * 
  */
trait LogUtil {

    val fname_fmt = "[%-14.14s] "
    val logger = Logger.getLogger(this.getClass.getName)
    
    private def getCallingFname() : String = { 
      try {
        //
        // If you're running in debug mode, I do a little bit of magic here
        // to automatically prepend the function / method name that the debug
        // method was called within.  Basically, the best (only) way that I could 
        // find was to pull it out of a stack trace.
        //
        // trimStr is a regular expression that basically is going to remove
        // anything from the beginning of the line (^), up until it finds a $$ 
        // in the method name, if it exists.  This is an optional package name / 
        // classname that Scala sometimes prepends, depending on circumstances
        // I don't fully understand.  So, if it looks like I got it wrong here
        // and the function names in the debug log are misleading, it might
        // be worth spending some more time here.  For now though, I think 
        // it's pretty slick, and much nicer than hardcoding function names
        // in strings and passing them into every "debug" call.
        //
        // Also, If this seems too expensive, please keep in mind that this
        // this only happens if debug mode is turned on.
        //
        val trimStr = """^.*\$\$""".r
        val ste : StackTraceElement = new Exception().getStackTrace()(4);        
        fname_fmt.format(trimStr.replaceAllIn(ste.getMethodName(),""))
      }
      catch { case e : Exception => {fname_fmt.format("")} }
    }
    
    private def formatMsg(msg : String) : String = {
      if (logger.isEnabledFor(DEBUG)) 
        {getCallingFname + msg.stripLineEnd}
      else 
        {msg.stripLineEnd}
    }
    
    def debug(message: => String) = { if (logger.isEnabledFor(DEBUG)) logger.debug(formatMsg(message)) }
    def debug(message: => String, ex:Throwable) = { if (logger.isEnabledFor(DEBUG)) logger.debug(formatMsg(message),ex) }
    
    def debugValue[T](valueName: String, value: => T):T = {
      val result:T = value
      debug(valueName.stripLineEnd + " == " + result.toString)
      result
    }
    
    def info(message: => String) = if (logger.isEnabledFor(INFO)) logger.info(formatMsg(message))
    def info(message: => String, ex:Throwable) = if (logger.isEnabledFor(INFO)) logger.info(formatMsg(message),ex)

    def warn(message: => String) = if (logger.isEnabledFor(WARN)) logger.warn(formatMsg(message))
    def warn(message: => String, ex:Throwable) = if (logger.isEnabledFor(WARN)) logger.warn(formatMsg(message),ex)

    def error(ex:Throwable) = if (logger.isEnabledFor(ERROR)) logger.error(ex.toString,ex)
    def error(message: => String) = if (logger.isEnabledFor(ERROR)) logger.error(formatMsg(message))
    def error(message: => String, ex:Throwable) = if (logger.isEnabledFor(ERROR)) logger.error(formatMsg(message),ex)

    def fatal(ex:Throwable) = if (logger.isEnabledFor(FATAL)) logger.fatal(ex.toString,ex)
    def fatal(message: => String) = if (logger.isEnabledFor(FATAL)) logger.fatal(formatMsg(message))
    def fatal(message: => String, ex:Throwable) = if (logger.isEnabledFor(FATAL)) logger.fatal(formatMsg(message),ex)
}
