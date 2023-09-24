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

  def freePositions(snake: List[Point]): List[Point] = {
    val positionsList = gridDims.allPointsInside
    positionsList.filter(p => !snake.contains(p)).toList
  }

  def chooseRandomApple(snake: List[Point]): Point = {
    val free = freePositions(snake)
    if (free.isEmpty) Point(-1, -1)
    else free(random.randomInt(free.length))
  }

}

case class GameState(gridDims: Dimensions, snake: List[Point],
                     currentApple: Point,
                     appleGenerator: AppleGenerator,
                     currentDirection: Direction,
                     toGrow: Int) {
  private def checkBounds(point: Point): Point = point match {
    case Point(x, y) if x > gridDims.width - 1 => Point(0, y)
    case Point(x, y) if x < 0 => Point(gridDims.width - 1, y)
    case Point(x, y) if y > gridDims.height - 1 => Point(x, 0)
    case Point(x, y) if y < 0 => Point(x, gridDims.height - 1)
    case _ => point
  }

  private def nextPosition(nextDirection: Direction): Point = {
    val nextPosition = snake.head + nextDirection.toPoint
    checkBounds(nextPosition)
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
      val newApple = appleGenerator.chooseRandomApple(nextSnake(nextDirection))
      GameState(gridDims, nextSnake(nextDirection), newApple, appleGenerator, nextDirection, Math.max(toGrow - 1, 0) + 3)
    }
    else {
      GameState(gridDims, nextSnake(nextDirection), currentApple, appleGenerator, nextDirection, Math.max(toGrow - 1, 0))
    }
  }

}

class GameLogic(val random: RandomGenerator,
                val gridDims: Dimensions) {
  def gameOver: Boolean = {
    gameStates.last.snake.tail.contains(gameStates.last.snake.head)
  }

  // Variable:
  // State List
  // Reverse flag
  // Current direction
  // GameState arguments (Snake, currentDirection, isReverse, toGrow)
  // Need to find a solution to avoid having an argument called toGrow in order to make it immutable, right?

  var currentDirection: Direction = East()
  var reverseFlag: Boolean = false

  val toGrow = 0

  val snake: List[(Point)] = List(
    (Point(2, 0)),
    (Point(1, 0)),
    (Point(0, 0))
  )
  val appleGenerator = new AppleGenerator(gridDims, random)
  val currentApple = appleGenerator.chooseRandomApple(snake)

  var gameStates: List[GameState] = List(GameState(gridDims, snake, currentApple, appleGenerator, currentDirection, toGrow))

  def step(): Unit = {
    if (reverseFlag) {
      if (gameStates.length > 1) {
        gameStates = gameStates.init
        currentDirection = gameStates.last.currentDirection
      }
    } else if (!gameOver) {
      gameStates = gameStates :+ gameStates.last.nextState(currentDirection)
    }

  }

  def isDirectionChangeAllowed(newDirection: Direction): Boolean = {

    val previousDirection =
      if (gameStates.length >= 2) gameStates(gameStates.length - 2).currentDirection
      else currentDirection

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
      if (gameStates.nonEmpty) {
        val lastState = gameStates.last
        gameStates = gameStates.dropRight(1) :+ lastState.copy(currentDirection = d)
      }
    }
  }

  def getCellType(p: Point): CellType = p match {
    case _ if p == gameStates.last.currentApple && gameStates.last.currentApple.x >= 0 => Apple()
    case _ if gameStates.last.snake.indexOf(p) == 0 => SnakeHead(currentDirection)
    case _ if gameStates.last.snake.contains(p) => SnakeBody()
    case _ => Empty()
  }

  // TODO implement me
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


