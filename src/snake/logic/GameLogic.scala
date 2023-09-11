package snake.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import snake.logic.GameLogic._

/** To implement Snake, complete the ``TODOs`` below.
 *
 * If you need additional files,
 * please also put them in the ``snake`` package.
 */
class GameLogic(val random: RandomGenerator,
                val gridDims: Dimensions) {

  def gameOver: Boolean = snake.tail.exists(segment => segment._1 == nextPosition)

  var currentPosition = Point(2, 0)
  var currentDirection: Direction = East()
  var nextPosition = Point(2, 0)


  var snake: List[(Point, CellType)] = List((Point(2, 0), SnakeHead(East())), (Point(1, 0), SnakeBody()), (Point(0, 0), SnakeBody()))

  val allPoints: IndexedSeq[Point] =
    for {
      x <- 0 until gridDims.width
      y <- 0 until gridDims.height
    } yield Point(x, y)

  var freeSpots: List[Point] = allPoints.toList


  var currentApple = Point(random.randomInt(gridDims.width), random.randomInt(gridDims.height))

  // TODO implement me
  def step(): Unit = {

    currentDirection match {
      case East() => nextPosition = currentPosition + East().toPoint
      case West() => nextPosition = currentPosition + West().toPoint
      case North() => nextPosition = currentPosition + North().toPoint
      case South() => nextPosition = currentPosition + South().toPoint
    }

    if (nextPosition.x > gridDims.width - 1) nextPosition = Point(0, nextPosition.y)
    if (nextPosition.x < 0) nextPosition = Point(gridDims.width - 1, nextPosition.y)
    if (nextPosition.y > gridDims.height - 1) nextPosition = Point(nextPosition.x, 0)
    if (nextPosition.y < 0) nextPosition = Point(nextPosition.x, gridDims.height - 1)

    if (gameOver) return

    val newHead = (nextPosition, SnakeHead(currentDirection))

    val increment = 1.0f / snake.length

    val updatedBody = snake.map {

      case (point, SnakeHead(_))
      => (point, SnakeBody(increment))

      case (point, SnakeBody(distance))
      => (point, SnakeBody(distance + increment))

    }

    if (nextPosition == currentApple) {
      snake = newHead +: updatedBody
      currentApple = Point(random.randomInt(gridDims.width), random.randomInt(gridDims.height))
    } else {
      snake = newHead +: updatedBody.init
    }

    currentPosition = nextPosition
  }

  // TODO implement me
  def changeDir(d: Direction): Unit = {
    d match {
      case East() => currentDirection = East()
      case West() => currentDirection = West()
      case North() => currentDirection = North()
      case South() => currentDirection = South()
    }
  }

  // TODO implement me
  def getCellType(p: Point): CellType = {
    if (p == currentApple) {
      Apple()
    } else {
      snake.find(segment => segment._1 == p).map(segment => segment._2).getOrElse(Empty())
    }
  }

  // TODO implement me
  def setReverse(r: Boolean): Unit = ()

}

/** GameLogic companion object */
object GameLogic {

  val FramesPerSecond: Int = 8 // change this to increase/decrease speed of game

  val DrawSizeFactor = 1.5 // increase this to make the game bigger (for high-res screens)
  // or decrease to make game smaller

  // These are the dimensions used when playing the game.
  // When testing the game, other dimensions are passed to
  // the constructor of GameLogic.
  //
  // DO NOT USE the variable DefaultGridDims in your code!
  //
  // Doing so will cause tests which have different dimensions to FAIL!
  //
  // In your code only use gridDims.width and gridDims.height
  // do NOT use DefaultGridDims.width and DefaultGridDims.height
  val DefaultGridDims
  : Dimensions =
  Dimensions(width = 25, height = 25) // you can adjust these values to play on a different sized board


}


