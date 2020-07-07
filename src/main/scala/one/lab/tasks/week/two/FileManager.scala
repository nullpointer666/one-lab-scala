package one.lab.tasks.week.two

import java.io.File

import scala.jdk.CollectionConverters._
import scala.util.chaining._

/**
  * Можете реализовать свою логику.
  * Главное чтобы работали команды и выводились ошибки при ошибочных действиях.
  * ll - показать все что есть в тек. папке
  * dir - показать только директории в тек. папке
  * ls - показать только файлы в тек. папке
  * cd some_folder - перейте из тек. папки в другую (учитывайте что путь можно сделать самым простым
  *                  то есть если я сейчас в папке /main и внутри main есть папка scala и я вызову
  *                  cd scala то мы должны просто перейти в папку scala. Реализация cd из текущей папки
  *                  по другому ПУТИ не требуется. Не забудьте только реализовать `cd ..`)
  *
  * Бонусные команды и идеи привествуются.
  */
object FileManager extends App {

  trait Command {
    def isSubstitutive = false
  }

  case class PrintErrorCommand(error: String) extends Command

  case class ListDirectoryCommand() extends Command

  case class ListFilesCommand() extends Command

  case class ListAllContentCommand() extends Command

  case class ChangeDirectoryCommand(destination: String) extends Command {
    override val isSubstitutive = true
  }

  case class ChangePathError(error: String)

  def getFiles(path: String) = {
    val d = new File(path)
    if (d.exists && d.isDirectory) d.listFiles.filter(_.isFile).map(_.getName).toList
    else List[String]()
  }

  def getDirectories(path: String) = {
    val d = new File(path)
    if (d.exists && d.isDirectory) d.listFiles.filter(_.isDirectory).map(_.getName).toList
    else List[String]()
  }

  def changePath(current: String, path: String) = {
    val dirs = getDirectories(current)
    if (dirs.contains(path)) Right(s"$current/$path")
    else Left(ChangePathError(s"Couldn't change current directory: no directory named $path"))
  }

  def parseCommand(input: String) = {
    val cmd = input.split(' ')
    cmd.head match {
      case "ll" => ListAllContentCommand()
      case "ls" => ListFilesCommand()
      case "dir" => ListDirectoryCommand()
      case "cd" => ChangeDirectoryCommand(cmd.last)
      case _ => PrintErrorCommand("No such command")
    }
  }

  def handleCommand(command: Command, currentPath: String) = command match {
    case ListFilesCommand() => getFiles(currentPath).toString
    case ListDirectoryCommand() => getDirectories(currentPath).toString
    case ListAllContentCommand() => (getFiles(currentPath) ++ getDirectories(currentPath)).toString
    case ChangeDirectoryCommand(destination: String) => changePath(currentPath, destination).toString
  }

  def main(basePath: String): Unit = ???
}
