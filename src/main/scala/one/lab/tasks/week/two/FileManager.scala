package one.lab.tasks.week.two

import java.io.File
import java.nio.file.NoSuchFileException

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

  def getFilenames(path: String, filterBy: Option[File => Boolean] = None): List[String] = {
    val file = new File(path)
    if (file.exists && file.isDirectory)
      filterBy match {
        case Some(param) => file.listFiles.filter(param).map(_.getName).toList
        case None => file.listFiles.map(_.getName).toList
      }
    else throw new NoSuchFileException("Such file doesn't exist")
  }

  def byFile(file: File): Boolean = file.isFile
  def byDirectory(file: File): Boolean = file.isDirectory

  def getFiles(path: String): List[String] = getFilenames(path, Some(byFile))
  def getDirectories(path: String): List[String] = getFilenames(path, Some(byDirectory))

  def changePath(current: String, path: String): Either[ChangePathError, String] = {
    val dirs = getDirectories(current)
    if (dirs.contains(path)) Right(s"$current/$path")
    else Left(ChangePathError(s"Couldn't change current directory: no directory named $path"))
  }

  def parseCommand(input: String): Command = {
    val cmd = input.split(' ')
    cmd.head match {
      case "dir" => ListDirectoryCommand()
      case "ll" => ListAllContentCommand()
      case "ls" => ListFilesCommand()
      case "cd" => ChangeDirectoryCommand(cmd.last)
      case _ => PrintErrorCommand("No such command")
    }
  }

  def handleCommand(command: Command, currentPath: String) = command match {
    case ListFilesCommand() => getFiles(currentPath).toString
    case ListDirectoryCommand() => getDirectories(currentPath).toString
    case ListAllContentCommand() => getFilenames(currentPath).toString
    case ChangeDirectoryCommand(destination: String) => changePath(currentPath, destination).toString
  }

  println(handleCommand(ListAllContentCommand(), "/home/azamat"))

  def main(basePath: String): Unit = {
  }
}
