package org.sadrobot.common.user

import scala.actors.Actor

import org.apache.mina.core.session.IoSession

import org.sadrobot.common.command.Command
import org.sadrobot.common.util.LogUtil

import scala.collection.mutable.{Set, SynchronizedSet, HashSet}


/******************************************************************************
  * class UserGroup
  *****************************************************************************
  *
  */

class UserGroup(val groupName : String)  {
  // Perhaps instead of creating UserGroups and adding users to them,
  // I should just create a list of Topics / Groups (what is a better name?)
  // and let users collect a list of topics they are monitoring
  //
  // But if I do that, I'll have to scan the entire list of users in order
  // to tell whenever there are no users left that are participating in 
  // a topic, and it should be deleted.  
  //
  // So maybe a topic should at least have a reference count?
  // Hrm.
  //
  // Leaving this map here for now, because the listtopics command
  // needs it in order to have an easy way to tell how many users
  // are subscribed to each topic.  The semi-obvious (and ugly) 
  // alternative is to have listtopics iterate through all the users and 
  // count how many users are on each topic.  While I'm sure I could
  // encapsulate that in a function somewhere, its not very scalable.
  //
  // Consider that in addition to the fact that UserGroups will eventually
  // need to keep track of whether or not they have any users in them
  // (they will go away once the last user leaves the group/topic), it 
  // seems to make the most sense to leave this user map here.
  
  var userMap : Map[String,UserSessionActor] = Map.empty;
  
  def addUserSession   (userObj : UserSessionActor)  = {userMap += (userObj.user.username -> userObj)}
  def removeUserSession(uname : String)  = {userMap -= uname}
  def getUserSession   (uname : String)  = {userMap get uname}
}

/******************************************************************************
  * class UserGroupSet
  *****************************************************************************
  *
  * Modification history:
  *
  */
class UserGroupSet extends HashSet[UserGroup] with SynchronizedSet[UserGroup] {
    def getUserGroup(groupName : String) = find {_.groupName == groupName}
}

/******************************************************************************
  * object AllUserGroups
  *****************************************************************************
  *
  */
object AllUserGroups extends UserGroupSet

/******************************************************************************
  * class User
  *****************************************************************************
  *
  * Not much here yet.  Right now, it just consists of a username and a list of 
  * commands that have been assigned to the user.  I'm setting this up
  * so that different users can have different commands assigned to them, and 
  * that they can be added / removed dynamically
  *
  * Each UserSessionActor has it's own user instance, so this doesn't require
  * a synchronized Map at the moment.  However, if privileged users (i.e. admin users)
  * were to be given the ability to add / remove commands from normal users
  * then that decision might need to be reconsidered.  
  *
  * Modification history:
  *
  */
class User(val username : String) {
  var cmdMap : Map[String,Command] = Map.empty;
  
  def addCommand   (cmdVerb : String, cmdFunc : Command) = {cmdMap += (cmdVerb -> cmdFunc)}
  def removeCommand(cmdVerb : String)                    = {cmdMap -= cmdVerb}
  def getCommand   (cmdVerb : String) : Option[Command]  = {cmdMap get cmdVerb}
  
  var groupAliasesMap : Map[String,UserGroup] = Map.empty;
  
  def addGroupAlias   (groupAlias : String, ug : UserGroup)     = {groupAliasesMap += (groupAlias -> ug)}
  def removeGroupAlias(groupAlias : String)                     = {groupAliasesMap -= groupAlias}
  def getGroupAlias   (groupAlias : String) : Option[UserGroup] = {groupAliasesMap get groupAlias}
}

/******************************************************************************
  * class UserSessionSet
  *****************************************************************************
  *
  * Modification history:
  * 
  * With the introduction of UserGroups, class UserSessionSet probably 
  * can be replaced by UserGroup.  The AllUserSesssions object could
  * probably become a UserGroup that consists of "all" sessions.
  *
  */
class UserSessionSet extends HashSet[UserSessionActor] with SynchronizedSet[UserSessionActor] {
    def getUserSession(username : String)= find {_.user.username == username}
}

/******************************************************************************
  * object AllUserSessions
  *****************************************************************************
  *
  * Modification history:
  *
  */
object AllUserSessions extends UserSessionSet

/******************************************************************************
  * trait UserSessionActor
  *****************************************************************************
  *
  * Modification history:
  *
  */
trait UserSessionActor extends Actor with LogUtil {
  def sreplyln(str: String) : String = { sreply(str + "\n"); str }
  
  def sreply(str: String) : String
  def disconnectSession() : Unit
  
  def user() : User
  def session() : IoSession
}