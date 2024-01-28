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
case class GameState(gridDims: Dimensions, gameMap: List[Block],
                     previousDirection: Direction) {
  var oldChain: List[Block] = List()

  def getBlockAtPosition(p: Point): Block = {
    gameMap.find(_.coordinates == p).getOrElse(Empty(p))
  }

  def isWithinGrid(p: Point, gridDims: Dimensions): Boolean = {
    p.x >= 0 && p.x < gridDims.width && p.y >= 0 && p.y < gridDims.height
  }


  private def nextPosition(nextDirection: Direction, coords: Point): Point = {
    val nextPosition = coords + nextDirection.toPoint
    nextPosition
  }


  private def nextGameMap(nextDirection: Direction): List[Block] = {
    gameMap.flatMap {
      case block: Block if block.controllable =>
        val newPosition = nextPosition(nextDirection, block.coordinates)
        gameMap.find(b => b.coordinates == newPosition) match {
          case Some(nextBlock) if nextBlock.stop && !nextBlock.isInstanceOf[RuleBlock] =>
            Some(block)
          case _ =>
            val chainOfBlocks = findChainOfBlocks(block.coordinates, nextDirection)
            val chain = chainOfBlocks ++ List(block)
            oldChain = oldChain ++ chain;
            val newChain = moveChain(chain, nextDirection)
            newChain
        }

      case otherBlock =>
        Some(otherBlock)
    }
  }


  private def findChainOfBlocks(startPosition: Point, direction: Direction): List[Block] = {
    var currentPosition = startPosition
    var chain = ListBuffer[Block]()
    var keepSearching = true

    while (keepSearching) {
      val nextPos = nextPosition(direction, currentPosition)
      gameMap.find(block => block.coordinates == nextPos) match {
        case Some(block) if block.push =>
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

  private def moveChain(chain: List[Block], direction: Direction): List[Block] = {
    if (chain.nonEmpty) {

      val endBlockPosition = nextPosition(direction, chain.last.coordinates)

      if (isWithinGrid(endBlockPosition, gridDims) &&
        !gameMap.exists(b => b.coordinates == endBlockPosition && b.stop && !b.isInstanceOf[RuleBlock])) {
        chain.map { block =>
          val newBlockPosition = nextPosition(direction, block.coordinates)
          if (isWithinGrid(newBlockPosition, gridDims)) {
            block match {
              case _: Baba => Baba(direction, newBlockPosition)
              case _: Wall => Wall(newBlockPosition)
              case _: WallSubject => WallSubject(newBlockPosition)
              case _: Connector => Connector(newBlockPosition)
              case _: BabaSubject => BabaSubject(newBlockPosition)
              case _: StopPredicate => StopPredicate(newBlockPosition)
              case _: YouPredicate => YouPredicate(newBlockPosition)
              case _: WinPredicate => WinPredicate(newBlockPosition)
              case _ => block // this will keep the original block for now, refine as necessary
            }
          } else {
            block // return original block if not in grid
          }
        }
      } else {
        chain
      }
    } else {
      List.empty
    }
  }

  def hasWon(): Boolean = {
    val winningBlocks = gameMap.filter(_.win).map(_.coordinates)
    val playerBlocks = gameMap.filter(_.controllable).map(_.coordinates)

    winningBlocks.intersect(playerBlocks).nonEmpty
  }

  def getPlayerBlocks = gameMap.collect {
    case block if block.controllable => block
  }


  def nextState(nextDirection: Direction): GameState = {


    val temp = nextGameMap(nextDirection)
    val newGameMapBlocks = temp.diff(oldChain)

    if (newGameMapBlocks.forall(block => isWithinGrid(block.coordinates, gridDims))) {
      GameState(gridDims, newGameMapBlocks, nextDirection)
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
    Wall(Point(6, 8)), WallSubject(Point(8, 8)), Connector(Point(8, 9)), StopPredicate(Point(9, 9)), BabaSubject(Point(13, 13)),
    Connector(Point(14, 13)), YouPredicate(Point(15, 13)), WinPredicate(Point(18, 18)))
  val initialGameState = GameState(gridDims, initialGameMap, currentDirection)

  private var gameStates: List[GameState] = List(initialGameState)
  private var reverseFlag: Boolean = false
  var resetFlag = false

  def gameOver: Boolean = false

  var gameWon: Boolean = false

  def reset() = {
    gameStates = List(initialGameState)
  }

  def undo(): Unit = {

  }

  def update(direction: Direction): Unit = {

    updateDir(direction)

    updateRules()

    val newGame = gameStates.last.nextState(currentDirection)
    gameWon = newGame.hasWon()
    gameStates = gameStates :+ newGame

  }

  def updateRules() = {
    // TODO: Make currentRules part of the game state

    val newRules: List[Rule] = detectRules()
    val allBlocks: List[Block] = gameStates.last.gameMap

    currentRules.foreach { rule =>
      if (!newRules.contains(rule)) {
        rule.repealOn(allBlocks)
      }
    }

    newRules.foreach(_.enactOn(allBlocks))
    println(getPlayerBlocks())

    currentRules = newRules
  }

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

  def updateDir(d: Direction): Unit = {
    currentDirection = d
  }

  def getBlockType(p: Point): Block = {
    val lastState = gameStates.last

    lastState.getBlockAtPosition(p)

  }

  def getPlayerBlocks(): List[Block] = gameStates.last.gameMap.collect {
    case block if block.controllable => block
  }

  private def reverseSnake(): Unit = {
    if (gameStates.length > 1) {
      gameStates = gameStates.init
    }
    val newRules: List[Rule] = detectRules()
    val allBlocks: List[Block] = gameStates.last.gameMap

    currentRules.foreach { rule =>
      if (!newRules.contains(rule)) {
        rule.repealOn(allBlocks)
      }
    }

    newRules.foreach(_.enactOn(allBlocks))

    currentRules = newRules
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


