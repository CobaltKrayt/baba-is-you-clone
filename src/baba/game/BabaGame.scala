// scalastyle:off
package baba.game

import java.awt.event
import processing.core.{PApplet, PConstants, PImage}
import processing.event.KeyEvent

import java.awt.event.KeyEvent._
import engine.GameBase
import engine.graphics.{Color, Point, Rectangle}
import baba.logic.{Baba, BabaSubject, Block, Connector, Dimensions, Direction, East, Empty, Environment, GameLogic, North, South, StopPredicate, Wall, WallSubject, West, WinPredicate, YouPredicate, Point => GridPoint}
import baba.game.BabaGame._
import engine.graphics.Color._
import engine.random.ScalaRandomGen

class BabaGame extends GameBase {

  var gameLogic = new GameLogic(new ScalaRandomGen(), GameLogic.DefaultGridDims)
  val updateTimer = new UpdateTimer(GameLogic.FramesPerSecond.toFloat)
  val gridDimensions: Dimensions = gameLogic.gridDims
  val widthInPixels: Int = (GameLogic.DrawSizeFactor * WidthCellInPixels * gridDimensions.width).ceil.toInt
  val heightInPixels: Int = (GameLogic.DrawSizeFactor * HeightCellInPixels * gridDimensions.height).ceil.toInt
  val screenArea: Rectangle = Rectangle(Point(0, 0), widthInPixels.toFloat, heightInPixels.toFloat)

  var babaUp: PImage = _
  var babaDown: PImage = _
  var babaLeft: PImage = _
  var babaRight: PImage = _
  var wallSubject: PImage = _
  var isConnector: PImage = _
  var stopPredicate: PImage = _
  var babaSubject: PImage = _
  var youPredicate: PImage = _
  var winPredicate: PImage = _

  // this function is wrongly named draw by processing (is called on each update next to drawing)
  override def draw(): Unit = {
    updateState()
    drawGrid()
    if (gameLogic.gameWon) drawGameWon()
  }

  def drawGameOverScreen(): Unit = {
    setFillColor(LightBlue)
    drawTextCentered("Stuck? Press Z to revert or R to reset", 20, screenArea.center)
  }

  def drawGameWon(): Unit = {
    setFillColor(LawnGreen)
    drawTextCentered("GameWon", 20, screenArea.center)
  }

  def drawGrid(): Unit = {

    val widthPerCell = screenArea.width / gridDimensions.width
    val heightPerCell = screenArea.height / gridDimensions.height

    def getCell(p: GridPoint): Rectangle = {
      val leftUp = Point(screenArea.left + p.x * widthPerCell,
        screenArea.top + p.y * heightPerCell)
      Rectangle(leftUp, widthPerCell, heightPerCell)
    }

    def drawBabaForDirection(dir: Direction, area: Rectangle): Unit = {
      val sprite = dir match {
        case North() => babaUp
        case South() => babaDown
        case West() => babaLeft
        case East() => babaRight
      }
      image(sprite, area.left, area.top, area.width, area.height)
    }


    def drawCell(area: Rectangle, block: Block): Unit = {
      val player = gameLogic.currentState.getPlayerBlocks

      def drawShapeForDirection(block: Block): Unit = {
        if (player.contains(block)) {
          block match {
            case Baba(_, _) => drawBabaForDirection(gameLogic.gameStates.last.previousDirection, area)
            case _ => ()
          }
        } else {
          block match {
            case Baba(direction, _) => drawBabaForDirection(direction, area)
            case _ => ()
          }
        }
      }

      block match {
        case Baba(_, _) =>
          drawShapeForDirection(block)
        case Wall(_) =>
          setFillColor(Color.DarkGreen)
          drawRectangle(area)
        case WallSubject(_) =>
          image(wallSubject, area.left, area.top, area.width, area.height)
        case BabaSubject(_) =>
          image(babaSubject, area.left, area.top, area.width, area.height)
        case Connector(_) =>
          image(isConnector, area.left, area.top, area.width, area.height)
        case StopPredicate(_) =>
          image(stopPredicate, area.left, area.top, area.width, area.height)
        case YouPredicate(_) =>
          image(youPredicate, area.left, area.top, area.width, area.height)
        case WinPredicate(_) =>
          image(winPredicate, area.left, area.top, area.width, area.height)
        case _ => ()
      }
    }

    setFillColor(Black)
    drawRectangle(screenArea)

    for (p <- gridDimensions.allPointsInside) {
      drawCell(getCell(p), gameLogic.getBlockType(p))
    }

  }

  /** Method that calls handlers for different key press events.
   * You may add extra functionality for other keys here.
   * See [[event.KeyEvent]] for all defined keycodes.
   *
   * @param event The key press event to handle
   */
  override def keyPressed(event: KeyEvent): Unit = {

    event.getKeyCode match {
      case VK_UP => gameLogic.update(North())
      case VK_DOWN => gameLogic.update(South())
      case VK_LEFT => gameLogic.update(West())
      case VK_RIGHT => gameLogic.update(East())
      case VK_R => {
        gameLogic.reset()
      }
      case VK_Z => {
        gameLogic.undo()
      }
      case _ => ()
    }

  }

  override def keyReleased(event: KeyEvent): Unit = {
    event.getKeyCode match {
      case _ => ()
    }
  }

  override def settings(): Unit = {
    pixelDensity(displayDensity())
    // If line below gives errors try size(widthInPixels, heightInPixels, PConstants.P2D)
    size(widthInPixels, heightInPixels)
  }

  override def setup(): Unit = {
    babaUp = loadImage("baba_up.png")
    babaDown = loadImage("baba_down.png")
    babaLeft = loadImage("baba_left.png")
    babaRight = loadImage("baba_right.png")
    wallSubject = loadImage("wall_subject.png")
    isConnector = loadImage("isConnector.png")
    stopPredicate = loadImage("stop_predicate.png")
    babaSubject = loadImage("baba_subject.png")
    youPredicate = loadImage("you_predicate.png")
    winPredicate = loadImage("win_predicate.png")
    // Fonts are loaded lazily, so when we call text()
    // for the first time, there is significant lag.
    // This prevents it from happening during gameplay.
    text("", 0, 0)
    // This should be called last, since the game
    // clock is officially ticking at this point
    updateTimer.init()
  }


  def updateState(): Unit = {
    if (updateTimer.timeForNextFrame()) {
      updateTimer.advanceFrame()
    }
  }

}


object BabaGame {


  val WidthCellInPixels: Double = 20 * GameLogic.DrawSizeFactor
  val HeightCellInPixels: Double = WidthCellInPixels

  def main(args: Array[String]): Unit = {
    // This is needed for Processing, using the name
    // of the class in a string is not very beautiful...
    PApplet.main("baba.game.BabaGame")
  }

}
