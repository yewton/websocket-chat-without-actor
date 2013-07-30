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
  def chatRoom(username: Option[String], roomNumber: Option[Int]) = Action { implicit request =>
    val pair: Option[(String, Int)] = for {
      username <- username if ! username.isEmpty
      roomNumber <- roomNumber if ChatRoom.validateNumber(roomNumber)
    } yield {
      (username, roomNumber)
    }
    pair.map { case (username, roomNumber) =>
      Redirect(routes.Application.room(roomNumber, username))
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
   * @param username
   * @return
   */
  def room(id: Int, username: String) = Action { implicit request =>
    Ok(views.html.chatRoom(id, username))
  }
  
  /**
   * Handles the chat websocket.
   */
  def chat(id: Int, username: String) = WebSocket.using[JsValue] { request  =>
    ChatRoom.join(id, username)
  }
}
