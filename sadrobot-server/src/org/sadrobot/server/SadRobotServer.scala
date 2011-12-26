package org.sadrobot.server

import java.net._
import java.io._
import java.util.concurrent.{Executors, ExecutorService}
import java.util.Properties

import org.apache.mina.filter.codec.ProtocolCodecFilter
import org.apache.mina.transport.socket.SocketAcceptor
import org.apache.mina.transport.socket.nio.{NioProcessor, NioSocketAcceptor}
import org.apache.log4j.PropertyConfigurator

import net.lag.naggati.IoHandlerActorAdapter

import org.sadrobot.common.util.LogUtil
import org.sadrobot.server.user.UserSessionActorImpl
import org.sadrobot.common.command.Command

import scala.collection.mutable.{HashMap, SynchronizedMap}

import java.util.ServiceLoader
import collection.JavaConversions._


/** [[https://wiki.scala-lang.org/display/SW/Syntax Scaladoc syntax Wiki]]
  * 
  * Blocking operations in Scala actors are a no-no.  So in order
  * to have an actor that "waits" for user input without
  * blocking, you need to use Java NIO.  Apache Mina is a wrapper
  * around the Java NIO classes, and Nagatti gives up a pretty
  * scala interface to Apache Mina.   
  * 
  * Basically, the important part is that after initializing Mina /
  * Nagatti (below), we can set up an actor to receive messages 
  * whenever input arrives on a socket without blocking.
  * 
  * Incoming data gets routed through the Codec.Decoder and
  * outgoing data gets routed through the Coded.Encoder. 
  * 
  * Codec.encoder basically does nothing, but Codec.Decoder does
  * some trivial parsing, changing the incoming datastream into
  * a "Request" object to be passed into the UserSessionActor
  * 
  * In addition to initializing the Apache Mina layer, we also
  * get log4j configured.  Logging functions are mixed in via
  * the LogUtil trait.  
  * 
  * Once the server is up and running, just telnet to the appropriate
  * address / port (0.0.0.0 becomes localhost 127.0.0.1) to 
  * connect
  * 
  * Mike Langen
  * 
  * Pulled log4j config file name off command line (system properties)
  * 
  */

object SadRobotServer extends LogUtil {
  
  val VERSION="0.9"
  val listenAddress = "0.0.0.0"
  val listenPort = 9999

  val cmdMap = new HashMap[String, Command] with SynchronizedMap[String, Command]

  def setMaxThreads = {
    val maxThreads = (Runtime.getRuntime.availableProcessors * 2)
    System.setProperty("actors.maxPoolSize", maxThreads.toString)
  }

  def initializeAcceptor = {
    info("Initializing NioSockectAcceptor")
    var acceptorExecutor = Executors.newCachedThreadPool()
    var acceptor = new NioSocketAcceptor(acceptorExecutor, 
                                         new NioProcessor(acceptorExecutor))
    acceptor.setBacklog(1000)
    acceptor.setReuseAddress(true)
    acceptor.getSessionConfig.setTcpNoDelay(true)
    acceptor.getFilterChain.addLast("codec",
            new ProtocolCodecFilter(Codec.encoder, 
                                    Codec.decoder))
    acceptor.setHandler(
            new IoHandlerActorAdapter(session => new UserSessionActorImpl(session)))
    acceptor.bind(new InetSocketAddress(listenAddress, listenPort))
  }
  
  def loadExtensions = {
    info("Loading extensions")
   
    val loader = ServiceLoader.load(classOf[Command])
   
    if (loader.iterator.size == 0) {
      warn("WARNING: No extensions found, this program probably won't do much.  You know, like anything at all.  Sorry!")
      warn("Jar containing extensions should be added to into the sadrobot-server/ext dir added to the class path directory")
      warn("Extensions must subclass org.sadrobot.common.command.Command. ")
      warn("Plugins must also have the file /META-INF/services/org.sadrobot.common.command.Command available in their ")
      warn("respective jar(s).  This file should contain the fully qualified classname of the new plugin class ")
      warn("Plugin MUST also have a zero argument constructor")
    }
    
    // import collection.JavaConversions._ does some magic that
    // converts Java iterators to Scala iterators.  I didn't
    // dig into it too much, but that's the scoop.
    //
    for (cmd : Command <- loader.iterator) {
      cmdMap += (cmd.cmdVerb -> cmd)
      info("Loaded command: " + cmd.cmdVerb)
    }
  }
  
  def getCommand(cmdVerb : String) : Option[Command] = cmdMap get (cmdVerb)

  def main(args: Array[String]): Unit = {    
    val log4jPropFileName = System.getProperty("log4j.properties")
    
    PropertyConfigurator.configure(if (log4jPropFileName != null) {log4jPropFileName} else {"log4j.properties"})
    
    setMaxThreads
    initializeAcceptor
    loadExtensions
    
    info("Listening on " + listenAddress + ":" + listenPort)
  }
  
}