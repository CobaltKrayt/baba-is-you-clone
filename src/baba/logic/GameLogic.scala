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

  def getBlockAtPosition(p: Point): Block = {
    gameMap.find(_.coordinates == p).getOrElse(Empty())
  }

  def isWithinGrid(p: Point, gridDims: Dimensions): Boolean = {
    p.x >= 0 && p.x < gridDims.width && p.y >= 0 && p.y < gridDims.height
  }


  private def nextPosition(nextDirection: Direction, coords: Point): Point = {
    val nextPosition = coords + nextDirection.toPoint
    nextPosition
  }


  private def nextPlayer(nextDirection: Direction): List[Block] = {
    val currentControllableBlocks = (player ++ gameMap).collect {
      case block: Environment if block.controllable => block
    }

    currentControllableBlocks.flatMap { playerBlock =>
      val newPosition = nextPosition(nextDirection, playerBlock.coordinates)

      gameMap.find(block => block.coordinates == newPosition) match {
        case Some(block) if block.stop && !block.isInstanceOf[RuleBlock] =>
          Some(playerBlock)
        case _ =>
          val chainOfBlocks = findChainOfBlocks(playerBlock.coordinates, nextDirection)
          // Remove chain from from gameMap
          // get moved blocks with moveChain
          // add new chain to gameMap
          if (moveChain(chainOfBlocks, nextDirection)) {
            playerBlock match {
              case _: Baba => Some(Baba(nextDirection, newPosition))
              case _: Wall => Some(Wall(newPosition))
              case _ => Some(playerBlock)
            }
          } else {
            Some(playerBlock)
          }
      }
    }

  }


  private def findChainOfBlocks(startPosition: Point, direction: Direction): List[Block] = {
    var currentPosition = startPosition
    var chain = ListBuffer[Block]()
    var keepSearching = true

    while (keepSearching) {
      val nextPos = nextPosition(direction, currentPosition)
      gameMap.find(block => block.coordinates == nextPos) match {
        case Some(block) if block.isInstanceOf[RuleBlock] =>
          chain.append(block)
          currentPosition = nextPos
        case Some(block) if !block.isInstanceOf[RuleBlock] && block.stop =>
          keepSearching = false
        case _ =>
          keepSearching = false
      }
    }

    chain.toList
  }

  private def moveChain(chain: List[Block], direction: Direction): Boolean = {
    if (chain.nonEmpty) {
      val endBlockPosition = nextPosition(direction, chain.last.coordinates)

      // Check if the end block position is within grid and not hitting a stop block
      if (isWithinGrid(endBlockPosition, gridDims) && !gameMap.exists(b => b.coordinates == endBlockPosition && b.stop)) {
        chain.foreach { block =>
          val newBlockPosition = nextPosition(direction, block.coordinates)

          // Only move the block if the new position is within grid
          if (isWithinGrid(newBlockPosition, gridDims)) {
            block.coordinates = newBlockPosition
          } else {
            return false
          }
        }
        true
      } else {
        false
      }
    } else {
      true
    }
  }


  def nextState(nextDirection: Direction): GameState = {

    if (player.exists(_.stop))
      return this

    val newPlayerBlocks = nextPlayer(nextDirection)
    val newGameMapBlocks = (player ++ gameMap).filterNot(block => block.isInstanceOf[Environment] && block.asInstanceOf[Environment].controllable)


    if (newPlayerBlocks.forall(block => isWithinGrid(block.coordinates, gridDims))) {
      GameState(gridDims, newPlayerBlocks, newGameMapBlocks, nextDirection)
    } else {
      this
    }
  }

}

class GameLogic(val random: RandomGenerator,
                val gridDims: Dimensions) {

  var currentDirection: Direction = East()
  var currentRules: List[Rule] = List()
  private val initalPlayer: List[Block] = List()
  private val initialGameMap: List[Block] = List(Baba(currentDirection, Point(2, 0)), Wall(Point(6, 6)),
    Wall(Point(6, 8)), WallRule(Point(8, 8)), Connector(Point(8, 9)), StopRule(Point(9, 9)), BabaSubject(Point(13, 13)),
    Connector(Point(14, 13)), YouPredicate(Point(15, 13)))

  private var gameStates: List[GameState] = List(GameState(gridDims, initalPlayer, initialGameMap, currentDirection))
  private var reverseFlag: Boolean = false

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
              rules.append(Rule(upBlock.asInstanceOf[Subject], downBlock.asInstanceOf[Predicate]))
            }


          }
          case _ => ()
        }
      }
    }
    rules.toList
  }

  def gameOver: Boolean = getPlayerBlocks().exists(_.stop)

  def step(): Unit = {

    if (reverseFlag) {
      reverseSnake()
    } else if (!gameOver) {

      val newRules: List[Rule] = detectRules()
      val allBlocks: List[Block] = gameStates.last.gameMap ++ gameStates.last.player

      currentRules.foreach { rule =>
        if (!newRules.contains(rule)) {
          rule.revert(allBlocks)
        }
      }

      newRules.foreach(_.evaluate(allBlocks))

      currentRules = newRules
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

  private def reverseSnake(): Unit = {
    if (gameStates.length > 1) {
      gameStates = gameStates.init
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


