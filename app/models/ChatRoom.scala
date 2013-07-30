package models

import play.api._
import play.api.libs.json._
import play.api.libs.iteratee._

object ChatRoom {
  val MaxNumber = 100
  val MinNumber = 1

  def validateNumber(n: Int): Boolean = (MinNumber <= n) && (n <= MaxNumber)

  private[this] var rooms = Map.empty[Int, ChatRoom] // TODO: max room number control

  def join(roomNumber: Int, username:String):(Iteratee[JsValue,_],Enumerator[JsValue]) = synchronized {
    require(validateNumber(roomNumber))
    val room: ChatRoom = rooms.get(roomNumber).getOrElse {
        val newRoom = new ChatRoom
        rooms = rooms + (roomNumber -> newRoom)
        newRoom
      }
    room.receive(Join(username)) match {
      case Connected(enumerator) =>
        // Create an Iteratee to consume the feed
        val iteratee = Iteratee.foreach[JsValue] { event =>
          val command = (event \ "command").as[String]
          command match {
            case "members" => room.receive(Members())
            case _         => room.receive(Talk(username, (event \ "text").as[String]))
          }
        }.mapDone { _ =>
          room.receive(Quit(username)) // TODO: free room
        }
        (iteratee, enumerator)
      case CannotConnect(error) =>
        // A finished Iteratee sending EOF
        val iteratee = Done[JsValue,Unit]((),Input.EOF)
        // Send an error and close the socket
        val enumerator =  Enumerator[JsValue](JsObject(Seq("error" -> JsString(error)))).andThen(Enumerator.enumInput(Input.EOF))
        (iteratee, enumerator)
    }
  }
}

class ChatRoom {
  var members = Set.empty[String]
  val (chatEnumerator, chatChannel) = Concurrent.broadcast[JsValue]

  def receive: PartialFunction[Command, _] = {
    case Join(username) => {
      if(members.contains(username)) {
        CannotConnect("This username is already used")
      } else {
        members = members + username
        receive(NotifyJoin(username))
        Connected(chatEnumerator)
      }
    }
    case NotifyJoin(username) => {
      notifyAll("join", username, "has entered the room")
    }
    case Talk(username, text) => {
      notifyAll("talk", username, text)
    }
    case Members() => {
      notifyAll("members")
    }
    case Quit(username) => {
      members = members - username
      notifyAll("quit", username, "has left the room")
    }
  }
  
  def notifyAll(kind: String, user: String, text: String) {
    val msg = JsObject(
      Seq(
        "kind"    -> JsString(kind),
        "user"    -> JsString(user),
        "message" -> JsString(text),
        "members" -> JsArray(
          members.toList.map(JsString)
        )
      )
    )
    println(msg.toString)
    chatChannel.push(msg)
  }

  def notifyAll(kind: String) {
    notifyAll(kind, "none", "none")
  }
}

sealed abstract class Command
case class Join(username: String) extends Command
case class Quit(username: String) extends Command
case class Talk(username: String, text: String) extends Command
case class NotifyJoin(username: String) extends Command
case class Members() extends Command

case class Connected(enumerator:Enumerator[JsValue])
case class CannotConnect(msg: String)
