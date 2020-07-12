package one.lab.tasks.week.three

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import one.lab.tasks.week.three.RestClientImpl.get
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods.parse
import sttp.client.ResponseError

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import scala.util.{Failure, Success}

object Githuber extends App {
  implicit val system: ActorSystem                        = ActorSystem("lalka")
  implicit val materializer: ActorMaterializer            = ActorMaterializer.create(system)
  implicit val executionContext: ExecutionContextExecutor = ExecutionContext.global
  implicit val defaultFormats: DefaultFormats.type        = DefaultFormats

  // TODO: поля можете добавить какие хотите
  case class GithubUser(login: String) {
    override def toString: String = s"Login: $login"
  }

  case class GithubRepository(fullName: String) {
    override def toString: String = s"Repo: $fullName"
  }

  //  https://api.github.com/users/{$USER}
  def getGithubUser(username: String): Future[GithubUser] = {
    val response = get(s"https://api.github.com/users/$username")
    response.map { parse(_).extract[GithubUser] }
  }

  def getUserRepositories(username: String): Future[List[GithubRepository]] = {
    val response = get(s"https://api.github.com/users/$username/repos")
    response.map { parse(_).camelizeKeys.extract[List[GithubRepository]] }
  }

  def getUserInfo(username: String): Unit = {
    getGithubUser(username) onComplete {
      case Success(response) => println(response)
      case Failure(errorMessage) => throw new Exception(errorMessage)
    }

    getUserRepositories(username) onComplete {
      case Success(response) => response.map(println)
      case Failure(errorMessage) => throw new Exception(errorMessage)
    }
  }

  getUserInfo("arturka")

//  getUserInfo("")
}
