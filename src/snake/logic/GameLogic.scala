package snake.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import snake.logic.GameLogic._

import scala.util.control.Breaks.break

/** To implement Snake, complete the ``TODOs`` below.
 *
 * If you need additional files,
 * please also put them in the ``snake`` package.
 */
class GameLogic(val random: RandomGenerator,
                val gridDims: Dimensions) {
  def gameOver: Boolean = {
    snake.tail.exists(segment => segment._1 == snake.head._1)
  }

  private var currentPosition = Point(2, 0)
  private var nextPosition = Point(2, 0)
  private var currentDirection: Direction = East()
  private var previousDirection: Direction = West()
  private var toGrow = 0
  private var snake: List[(Point, CellType)] = List(
    (Point(2, 0), SnakeHead(East())),
    (Point(1, 0), SnakeBody()),
    (Point(0, 0), SnakeBody())
  )
  private var currentApple = chooseRandomApple()

  private def freePositions(): List[Point] = {
    val positionsList = gridDims.allPointsInside
    positionsList.filter(p => getCellType(p) == Empty()).toList
  }

  private def chooseRandomApple(): Point = {
    val free = freePositions()
    if (free.isEmpty) Point(-1, -1)
    else free(random.randomInt(free.length))
  }

  private def updateNextPosition(): Unit = {

    nextPosition = currentPosition + currentDirection.toPoint

    if (nextPosition.x > gridDims.width - 1) nextPosition = Point(0, nextPosition.y)
    else if (nextPosition.x < 0) nextPosition = Point(gridDims.width - 1, nextPosition.y)

    if (nextPosition.y > gridDims.height - 1) nextPosition = Point(nextPosition.x, 0)
    else if (nextPosition.y < 0) nextPosition = Point(nextPosition.x, gridDims.height - 1)
  }

  private def updateSnake(): Unit = {
    val newHead = (nextPosition, SnakeHead(currentDirection))
    val increment = 1.0f / snake.length

    val updatedBody = snake.map {
      case (point, SnakeHead(_)) => (point, SnakeBody(increment))
      case (point, SnakeBody(distance)) => (point, SnakeBody(distance + increment))
    }

    snake = newHead +: updatedBody.init

    if (toGrow > 0) {
      snake = snake :+ updatedBody.last
      toGrow -= 1
    }
  }

  def step(): Unit = {
    updateNextPosition()

    if (gameOver) return

    updateSnake()

    if (nextPosition == currentApple) {
      toGrow += 3
      currentApple = chooseRandomApple()
    }

    previousDirection = currentDirection
    currentPosition = nextPosition
  }

  private def isDirectionChangeAllowed(newDirection: Direction): Boolean = {
    newDirection match {
      case East() if currentDirection != West() && previousDirection != West() => true
      case West() if currentDirection != East() && previousDirection != East() => true
      case North() if currentDirection != South() && previousDirection != South() => true
      case South() if currentDirection != North() && previousDirection != North() => true
      case _ => false
    }
  }

  def changeDir(d: Direction): Unit = {
    if (isDirectionChangeAllowed(d)) {
      currentDirection = d
    }
  }

  def getCellType(p: Point): CellType = {
    if (p == currentApple && currentApple.x >= 0) {
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


