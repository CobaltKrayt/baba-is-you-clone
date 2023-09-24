package snake.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import snake.logic.GameLogic._

import scala.collection.immutable.Queue
import scala.util.control.Breaks.break

/** To implement Snake, complete the ``TODOs`` below.
 *
 * If you need additional files,
 * please also put them in the ``snake`` package.
 */
case class AppleGenerator(gridDims: Dimensions, random: RandomGenerator) {

  private def getAvailablePositions(snake: List[Point]): List[Point] = {
    val positionsList = gridDims.allPointsInside
    positionsList.filter(p => !snake.contains(p)).toList
  }

  def generateRandomApple(snake: List[Point]): Point = {
    val availablePositions = getAvailablePositions(snake)
    if (availablePositions.isEmpty) Point(-1, -1)
    else availablePositions(random.randomInt(availablePositions.length))
  }

}

case class GameState(gridDims: Dimensions, snake: List[Point],
                     currentApple: Point,
                     appleGenerator: AppleGenerator,
                     previousDirection: Direction,
                     toGrow: Int) {
  private def wrapAround(point: Point): Point = point match {
    case Point(x, y) if x >= gridDims.width => Point(0, y)
    case Point(x, y) if x < 0 => Point(gridDims.width - 1, y)
    case Point(x, y) if y >= gridDims.height => Point(x, 0)
    case Point(x, y) if y < 0 => Point(x, gridDims.height - 1)
    case _ => point
  }

  private def nextPosition(nextDirection: Direction): Point = {
    val nextPosition = snake.head + nextDirection.toPoint
    wrapAround(nextPosition)
  }

  private def nextSnake(nextDirection: Direction): List[Point] = {

    val newHead: Point = nextPosition(nextDirection)
    if (toGrow > 0) {
      newHead +: snake
    } else {
      newHead +: snake.init
    }
  }

  def nextState(nextDirection: Direction): GameState = {
    if (nextPosition(nextDirection) == currentApple) {

      val newApple = appleGenerator.generateRandomApple(nextSnake(nextDirection))
      GameState(gridDims, nextSnake(nextDirection), newApple, appleGenerator, nextDirection, Math.max(toGrow - 1, 0) + 3)
    }
    else {
      GameState(gridDims, nextSnake(nextDirection), currentApple, appleGenerator, nextDirection, Math.max(toGrow - 1, 0))
    }
  }

}

class GameLogic(val random: RandomGenerator,
                val gridDims: Dimensions) {

  private var currentDirection: Direction = East()
  private var reverseFlag: Boolean = false
  private val toGrow = 0
  private val initialSnake: List[Point] = List(Point(2, 0), Point(1, 0), Point(0, 0))
  private val appleGenerator = AppleGenerator(gridDims, random)
  private val initialApple = appleGenerator.generateRandomApple(initialSnake)
  private var gameStates: List[GameState] = List(GameState(gridDims, initialSnake, initialApple, appleGenerator, currentDirection, toGrow))

  def gameOver: Boolean = {
    val lastState = gameStates.last
    lastState.snake.tail.contains(lastState.snake.head)
  }

  def step(): Unit = {
    if (reverseFlag) {
      reverseSnake()
    } else if (!gameOver) {
      gameStates = gameStates :+ gameStates.last.nextState(currentDirection)
    }
  }

  private def reverseSnake(): Unit = {
    if (gameStates.length > 1) {
      gameStates = gameStates.init
      currentDirection = gameStates.last.previousDirection
    }
  }

  private def canChangeDirection(newDirection: Direction): Boolean = {

    val previousDirection =
      if (gameStates.length >= 2) gameStates.last.previousDirection
      else currentDirection

    newDirection != currentDirection.opposite && newDirection != previousDirection.opposite
  }

  def changeDir(d: Direction): Unit = {
    if (canChangeDirection(d)) {
      currentDirection = d
    }
  }

  def getCellType(p: Point): CellType = {
    val lastState = gameStates.last

    def computeDistanceToHead: Float = {
      val index = lastState.snake.indexOf(p)
      index.toFloat / (lastState.snake.length - 1)
    }

    p match {
      case _ if p == lastState.currentApple => Apple()
      case _ if lastState.snake.head == p => SnakeHead(currentDirection)
      case _ if lastState.snake.contains(p) => SnakeBody(computeDistanceToHead)
      case _ => Empty()
    }
  }

  def setReverse(r: Boolean): Unit = {
    reverseFlag = r
  }

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


