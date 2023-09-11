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

  def gameOver: Boolean = false

  var snakeBody: List[Point] = List(Point(5, 3))
  var currentPosition = Point(5, 3)
  var currentDirection: Direction = East()
  var nextPosition = Point(0, 0)
  var currentApple = Point(random.randomInt(gridDims.width), random.randomInt(gridDims.height))
  var snakeLentgh = 0

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

    snakeBody = nextPosition +: snakeBody

    if (nextPosition == currentApple) {
      snakeLentgh += 1
      currentApple = Point(random.randomInt(gridDims.width), random.randomInt(gridDims.height))
    } else snakeBody = snakeBody.init

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
    if (p == currentPosition) SnakeHead(currentDirection)
    else if (snakeBody.contains(p)) SnakeBody()
    else if (p == currentApple) Apple()
    else Empty()
  }

  // TODO implement me
  def setReverse(r: Boolean): Unit = ()

}

/** GameLogic companion object */
object GameLogic {

  val FramesPerSecond: Int = 5 // change this to increase/decrease speed of game

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


