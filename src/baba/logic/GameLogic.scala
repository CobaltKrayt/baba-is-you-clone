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
                     previousDirection: Direction, rules: List[Rule]) {

  def getBlockAtPosition(p: Point, blockMap: List[Block]): Block = {
    blockMap.find(_.coordinates == p).getOrElse(Empty(p))
  }

  def isWithinGrid(p: Point, gridDims: Dimensions): Boolean = {
    p.x >= 0 && p.x < gridDims.width && p.y >= 0 && p.y < gridDims.height
  }

  def getBlockChain(startingBlock: Block, nextDirection: Direction): List[Block] = {
    LazyList.unfold(startingBlock) {
      currentBlock =>
        val nextPos = currentBlock.coordinates + nextDirection.toPoint
        gameMap.find(_.coordinates == nextPos) match {
          case Some(nextBlock) if nextBlock.push => Some(nextBlock, nextBlock)
          case _ => None
        }
    }.toList
  }

  def moveChain(chain: List[Block], nextDirection: Direction): List[Block] = {
    chain.map(block => {
      matchBlock(block, nextDirection, block.coordinates + nextDirection.toPoint)
    })
  }

  def getPlayerBlocks = gameMap.collect {
    case block if block.controllable => block
  }

  private def nextPosition(nextDirection: Direction, coords: Point): Point = {
    val nextPosition = coords + nextDirection.toPoint
    nextPosition
  }

  private def movePlayer(nextDirection: Direction): List[Block] = {
    var addChains: List[Block] = List()
    var removeChains: List[Block] = List()

    gameMap.foreach {
      block =>
        if (block.controllable) {

          val newPosition = nextPosition(nextDirection, block.coordinates)

          // Get the chain of blocks in the direction of nextDirection
          // if all of them have push == true
          val blockList: List[Block] = block +: getBlockChain(block, nextDirection)


          // if theres an empty space after the last block in the chain
          // if the empty space after the last block in the chain is within the border
          if (blockList.nonEmpty) {

            val nextBlockCoords = blockList.last.coordinates + nextDirection.toPoint
            val nextBlock = gameMap.find(block => block.coordinates == nextBlockCoords)

            // move all
            nextBlock match {
              case Some(nextBlock) if isWithinGrid(nextBlockCoords, gridDims) && !nextBlock.stop =>
                addChains = addChains ::: moveChain(blockList, nextDirection)
                removeChains = removeChains ::: blockList

              case None =>
                addChains = addChains ::: moveChain(blockList, nextDirection)
                removeChains = removeChains ::: blockList

              case Some(_) =>
            }

          } else {
            println("dafuq")
          }
        }
    }
    (gameMap diff removeChains) ::: addChains
  }

  private def matchBlock(block: Block, nextDirection: Direction, newPosition: Point): Block = {
    block match {
      case _: Baba => Baba(nextDirection, newPosition)
      case _: Wall => Wall(newPosition)
      case _: WallSubject => WallSubject(newPosition)
      case _: Connector => Connector(newPosition)
      case _: BabaSubject => BabaSubject(newPosition)
      case _: StopPredicate => StopPredicate(newPosition)
      case _: YouPredicate => YouPredicate(newPosition)
      case _: WinPredicate => WinPredicate(newPosition)
      case _: PushPredicate => PushPredicate(newPosition)
      case _ => block // this will keep the original block for now, refine as necessary
    }
  }

  def updateRules(newGameMap: List[Block]): List[Rule] = {

    val newRules: List[Rule] = detectRules(newGameMap)
    val allBlocks: List[Block] = newGameMap

    rules.foreach { rule =>
      if (!newRules.contains(rule)) {
        rule.repealOn(allBlocks)
      }
    }

    newRules.foreach(_.enactOn(allBlocks))

    newRules
  }

  def detectRules(newGameMap: List[Block]): List[Rule] = {
    val rules = ListBuffer[Rule]()

    for (y <- 0 until gridDims.height) {
      for (x <- 0 until gridDims.width) {
        val point = Point(x, y)
        getBlockAtPosition(point, newGameMap) match {
          case Connector(_) => {
            val leftBlock = getBlockAtPosition(Point(x - 1, y), newGameMap)
            val rightBlock = getBlockAtPosition(Point(x + 1, y), newGameMap)

            if (leftBlock.isInstanceOf[Subject] && rightBlock.isInstanceOf[Predicate]) {
              rules.append(Rule(leftBlock.asInstanceOf[Subject], rightBlock.asInstanceOf[Predicate]))
            }

            val upBlock = getBlockAtPosition(Point(x, y - 1), newGameMap)
            val downBlock = getBlockAtPosition(Point(x, y + 1), newGameMap)

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


  def nextState(nextDirection: Direction): GameState = {


    val newGameMapBlocks = movePlayer(nextDirection)
    val newRules = updateRules(newGameMapBlocks)
    //println(newRules)

    if (newGameMapBlocks.forall(block => isWithinGrid(block.coordinates, gridDims))) {
      GameState(gridDims, newGameMapBlocks, nextDirection, newRules)
    } else {
      this
    }
  }
}

class GameLogic(val random: RandomGenerator,
                val gridDims: Dimensions) {

  private val initialDirection: Direction = East()
  private val initialRules: List[Rule] = List(Rule(WallSubject(Point(8, 8)), StopPredicate(Point(8, 10))), Rule(BabaSubject(Point(13, 13)), YouPredicate(Point(15, 13))))

  private val initialGameMap: List[Block] = List(Baba(initialDirection, Point(2, 0)), Wall(Point(6, 6)),
    Wall(Point(6, 8)), WallSubject(Point(8, 8)), Connector(Point(8, 9)), StopPredicate(Point(8, 10)),
    PushPredicate(Point(15, 15)), BabaSubject(Point(13, 13)), Connector(Point(14, 13)), YouPredicate(Point(15, 13)), WinPredicate(Point(15, 18)))
  val initialGameState = GameState(gridDims, initialGameMap, initialDirection, initialRules)

  var gameStates: List[GameState] = List(initialGameState)

  var gameWon: Boolean = false

  def currentState: GameState = gameStates.last

  def reset() = {
    gameStates = List(initialGameState)
  }

  def undo(): Unit = {

  }

  def update(direction: Direction): Unit = {

    val newGame = gameStates.last.nextState(direction)
    
    //println(newGame.rules)
    gameWon = hasWon(newGame)
    gameStates = gameStates :+ newGame

  }

  def getBlockType(p: Point): Block = {
    val lastState = gameStates.last
    lastState.getBlockAtPosition(p, gameStates.last.gameMap)
  }

  def hasWon(gameState: GameState): Boolean = {
    val winningBlocks = gameState.gameMap.filter(_.win).map(_.coordinates)
    val playerBlocks = gameState.gameMap.filter(_.controllable).map(_.coordinates)

    println(winningBlocks)
    println(playerBlocks)
    winningBlocks.intersect(playerBlocks).nonEmpty

  }

}

/** GameLogic companion object */
object GameLogic {

  val FramesPerSecond: Int = 60 // change this to increase/decrease speed of game

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
  Dimensions(width = 25, height = 20) // you can adjust these values to play on a different sized board


}


