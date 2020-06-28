package one.lab.tasks.week.one

/**
  * 1. You have to define three [[one.lab.tasks.week.one.Interfaces.Console]] class implementation, let's say Xbox,
  *    PlayStation, Sega
  * 2. You also need to define implementation GameDisk traits for each of console, and some classes of games, see
  *    [[one.lab.tasks.week.one.Interfaces.XboxGameDisk]] and [[one.lab.tasks.week.one.Interfaces.ForzaHorizon]]
  * 3. When creating implementation of Console be sure to properly implement play method,
  *    so that when I try to play Xbox with PS game disk, it will print me that disk format is invalid.
  *    But when I supply appropriate disk it will print s"playing ${disk.game()}"
  */
object Interfaces {
  trait GameDisk {
    val consoleType: String
    val game: String
  }

  trait Console {
    def play(disk: GameDisk): Unit
  }

  trait XboxGameDisk extends GameDisk {
    override val consoleType: String = "Xbox"
  }

  class ForzaHorizon extends XboxGameDisk {
    override val game: String = "ForzaHorizon race game"
  }

  class Xbox extends Console {
    def play(disk: GameDisk): Unit =
      if (disk.consoleType == "Xbox") println(s"Playing $disk.game")
      else println("Disk format is invalid")
  }

  class PlayStation extends Console {
    def play(disk: GameDisk): Unit =
      if (disk.consoleType == "PlayStation") println(s"Playing $disk.game")
      else println("Disk format is invalid")
  }

  class Sega extends Console {
    def play(disk: GameDisk): Unit =
      if (disk.consoleType == "Sega") println(s"Playing $disk.game")
      else println("Disk format is invalid")
  }
}
