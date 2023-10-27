package baba.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import baba.logic.GameLogic._

import scala.collection.immutable.Queue
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.break

/** To implement Snake, complete the ``TODOs`` below.
 *
 * If you need additional files,
 * please also put them in the ``baba`` package.
 */
case class GameState(gridDims: Dimensions, player: List[Block], gameMap: List[Block],
                     previousDirection: Direction) {

  val updatedGameMap = new ListBuffer[Block]() ++ gameMap

  def getBlockAtPosition(p: Point): Block = {
    gameMap.find(_.coordinates == p).getOrElse(Empty())
  }

  def isWithinGrid(p: Point, gridDims: Dimensions): Boolean = {
    p.x >= 0 && p.x < gridDims.width && p.y >= 0 && p.y < gridDims.height
  }


  private def nextPosition(nextDirection: Direction, block: Block): Point = {
    val nextPosition = block.coordinates + nextDirection.toPoint
    nextPosition
  }

  private def nextPlayer(nextDirection: Direction): (List[Block], List[Block]) = {

    val updatedGameMap = new ListBuffer[Block]() ++ gameMap

    val newPlayerBlocks = player.flatMap { playerBlock =>
      val newPosition = nextPosition(nextDirection, playerBlock)


      updatedGameMap.find(block => block.coordinates == newPosition) match {

        case Some(block) if block.push =>
          val pushedBlockNewPosition = nextPosition(nextDirection, block)
          if (updatedGameMap.exists(b => b.coordinates == pushedBlockNewPosition && b.stop && !b.isInstanceOf[RuleBlock])) {
            Some(playerBlock)
          } else {

            updatedGameMap -= block

            val newBlock = block match {
              case _: Connector => Connector(pushedBlockNewPosition)
              case _: WallRule => WallRule(pushedBlockNewPosition)
              case _: StopRule => StopRule(pushedBlockNewPosition)
              case _ => block
            }

            updatedGameMap += newBlock
            Some(Baba(nextDirection, newPosition))
          }
        case Some(block) if block.stop =>
          Some(playerBlock)
        case _ =>
          Some(Baba(nextDirection, newPosition))
      }
    }

    (newPlayerBlocks.toList, updatedGameMap.toList)
  }

  def nextState(nextDirection: Direction): GameState = {
    val (newPlayerBlocks, updatedGameMap) = nextPlayer(nextDirection)

    if (newPlayerBlocks.forall(block => isWithinGrid(block.coordinates, gridDims))) {
      GameState(gridDims, newPlayerBlocks, updatedGameMap, nextDirection)
    } else {
      this
    }
  }

}

class GameLogic(val random: RandomGenerator,
                val gridDims: Dimensions) {

  var currentDirection: Direction = East()
  private val initalPlayer: List[Block] = List(Baba(currentDirection, Point(2, 0)), Baba(currentDirection, Point(3, 1)))
  private val initialGameMap: List[Block] = List(Wall(Point(6, 6)), Wall(Point(6, 8)), WallRule(Point(8, 8))
    , Connector(Point(8, 9)), StopRule(Point(9, 9)))

  private var gameStates: List[GameState] = List(GameState(gridDims, initalPlayer, initialGameMap, currentDirection))

  def detectRules(): List[Rule] = {
    val rules = ListBuffer[Rule]()

    for (y <- 0 until gridDims.height) {
      for (x <- 0 until gridDims.width) {
        val point = Point(x, y)
        gameStates.last.getBlockAtPosition(point) match {
          case Connector(_) => {
            val leftBlock = gameStates.last.getBlockAtPosition(Point(x - 1, y))
            val rightBlock = gameStates.last.getBlockAtPosition(Point(x + 1, y))

            if (leftBlock.isInstanceOf[Subject] && rightBlock.isInstanceOf[Predicate]) {
              rules.append(Rule(leftBlock.asInstanceOf[Subject], rightBlock.asInstanceOf[Predicate]))
            }

            val upBlock = gameStates.last.getBlockAtPosition(Point(x, y - 1))
            val downBlock = gameStates.last.getBlockAtPosition(Point(x, y + 1))

            if (upBlock.isInstanceOf[Subject] && downBlock.isInstanceOf[Predicate]) {
              rules.append(Rule(leftBlock.asInstanceOf[Subject], rightBlock.asInstanceOf[Predicate]))
            }


          }
          case _ => ()
        }
      }
    }
    rules.toList
  }

  def gameOver: Boolean = false

  def step(): Unit = {
    if (!gameOver) {
      val rules: List[Rule] = detectRules()
      rules.foreach(_.evaluate(gameStates.last.gameMap))
      gameStates = gameStates :+ gameStates.last.nextState(currentDirection)
    }
  }


  def changeDir(d: Direction): Unit = {
    currentDirection = d
  }

  def getBlockType(p: Point): Block = {
    val lastState = gameStates.last

    lastState.player.find(_.coordinates == p) match {
      case Some(block) => block
      case None => lastState.getBlockAtPosition(p)
    }
  }

  def getPlayerBlocks(): List[Block] = gameStates.last.player


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


