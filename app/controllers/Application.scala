package controllers

import play.api._
import play.api.mvc._

import play.api.libs.json._
import play.api.libs.iteratee._

import models._

import akka.actor._
import scala.concurrent.duration._

object Application extends Controller {
  
  /**
   * Just display the home page.
   */
  def index = Action { implicit request =>
    Ok(views.html.index())
  }
  
  /**
   * Redirect to the chat room page.
   */
  def chatRoom(username: Option[String], roomNumber: Option[Int]) = Action { request =>
    val pair: Option[(String, Int)] = for {
      username <- username if ! username.isEmpty
      roomNumber <- roomNumber if ChatRoom.validateNumber(roomNumber)
    } yield {
      (username, roomNumber)
    }
    pair.map { case (username, roomNumber) =>
      Redirect(routes.Application.room(roomNumber)).flashing(
        "username" -> username
      )
    }.getOrElse {
      Redirect(routes.Application.index).flashing(
        "error" ->
          "Please choose a valid username and a valid room number(%d to %d).".format(
            ChatRoom.MinNumber, ChatRoom.MaxNumber
          )
      )
    }
  }

  /**
   * Display the chat room page.
   * @param id
   * @return
   */
  def room(id: Int) = Action { implicit request =>
    request.flash.get("username") match {
      case Some(username) => Ok(views.html.chatRoom(id, username))
      case _ => Redirect(routes.Application.index).flashing(
        "error" -> "Some error has occured. Please try again."
      )
    }
  }
  
  /**
   * Handles the chat websocket.
   */
  def chat(id: Int, username: String) = WebSocket.using[JsValue] { request  =>
    ChatRoom.join(id, username)
  }
}
