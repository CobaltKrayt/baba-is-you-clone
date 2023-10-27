package baba.logic

// Assuming Point is defined elsewhere

abstract class Block() {

  var push: Boolean
  var stop: Boolean

  var coordinates: Point
}

abstract class RuleBlock(val location: Point) extends Block {

}

case class Connector(override val location: Point) extends RuleBlock(location) {
  override var coordinates: Point = location

  override var stop: Boolean = true
  override var push: Boolean = true
}

abstract class Subject(override val location: Point) extends RuleBlock(location) {

  override var stop: Boolean = true


}

case class WallRule(override val location: Point) extends Subject(location) {
  override var coordinates: Point = location
  override var push: Boolean = true
}

abstract class Predicate(override val location: Point) extends RuleBlock(location) {
  override var coordinates: Point = location


  override var stop: Boolean = true
}


case class StopRule(override val location: Point) extends Predicate(location) {
  override var push: Boolean = true
}


abstract class Environment(val location: Point) extends Block {
  override var coordinates: Point = location

  override var push: Boolean = false


}

case class Baba(direction: Direction, override val location: Point) extends Environment(location) {

  override var stop: Boolean = false

}

case class Wall(override val location: Point) extends Environment(location) {

  override var stop: Boolean = false
}

case class Empty() extends Block {
  override var coordinates: Point = Point(0, 0)

  override var push: Boolean = false

  override var stop: Boolean = false
}
