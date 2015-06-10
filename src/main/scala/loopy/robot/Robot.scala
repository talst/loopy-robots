    package loopy.robot

    import scala.annotation.tailrec
    import scala.io.StdIn.readLine

    object Robot extends App {

      case class Position(x: Int, y: Int) {
        def dx(d: Int) = copy(x = x + d)
        def dy(d: Int) = copy(y = y + d)
      }

      sealed trait Move
      case object Step extends Move
      case object Left extends Move
      case object Right extends Move

      sealed trait Heading
      case object North extends Heading
      case object East extends Heading
      case object South extends Heading
      case object West extends Heading

      case class Robot(position: Position, heading: Heading) {
        def dx(d: Int) = Robot(position.dx(d), heading)
        def dy(d: Int) = Robot(position.dy(d), heading)

        def step = heading match {
          case North => dx(1)
          case East => dy(1)
          case South => dx(-1)
          case West => dy(-1)
        }

        def left = heading match {
          case North => Robot(position, West)
          case East => Robot(position, North)
          case South => Robot(position, East)
          case West => Robot(position, South)
        }

        def right = heading match {
          case North => Robot(position, East)
          case East => Robot(position, South)
          case South => Robot(position, West)
          case West => Robot(position, North)
        }
      }

      def readInput(input: String) = input.split("").map {
        case "S" => Step
        case "R" => Right
        case "L" => Left
      }.toList

      @tailrec
      def executeCommands(commands: List[Move], robot: Robot): Robot = {
        if (commands.isEmpty) robot
        else commands.head match {
          case Step => executeCommands(commands.tail, robot.step)
          case Left => executeCommands(commands.tail, robot.left)
          case Right => executeCommands(commands.tail, robot.right)
        }
      }

      def isLooping(commands: List[Move]) = {
        @tailrec
        def innerRec(commands: List[Move], robot: Robot, cycle: Int): (Boolean, Int) = {
          if (robot.heading == North && cycle != 0) (robot.position.x == 0 && robot.position.y == 0, cycle)
          else innerRec(commands, executeCommands(commands, robot), cycle + 1)
        }
        innerRec(commands, Robot(Position(0, 0), North), 0)
      }

      val result = isLooping(readInput(readLine()))
      if (result._1) println(s"Loop detected! ${result._2} cycle(s) to complete loop")
      else println("No loop detected!")
    }
