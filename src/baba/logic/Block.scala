package baba.logic

// Assuming Point is defined elsewhere

abstract class Block() {

  var push: Boolean
  var stop: Boolean
  var controllable: Boolean
  var win: Boolean

  var coordinates: Point
}

abstract class RuleBlock(val location: Point) extends Block {

}

case class Connector(override val location: Point) extends RuleBlock(location) {
  override var coordinates: Point = location

  override var stop: Boolean = true
  override var push: Boolean = true
  override var controllable: Boolean = false
  override var win: Boolean = false
}

abstract class Subject(override val location: Point) extends RuleBlock(location) {

  override var win: Boolean = false
  override var stop: Boolean = true

}

case class WallRule(override val location: Point) extends Subject(location) {
  override var coordinates: Point = location
  override var push: Boolean = true
  override var controllable: Boolean = false
}

case class BabaSubject(override val location: Point) extends Subject(location) {
  override var coordinates: Point = location
  override var push: Boolean = true
  override var controllable: Boolean = false
}

abstract class Predicate(override val location: Point) extends RuleBlock(location) {
  override var win = false
  override var coordinates: Point = location

  def action: Action

  override var stop: Boolean = true
}


case class StopPredicate(override val location: Point) extends Predicate(location) {
  override var push: Boolean = true

  override def action: Action = StopAction()

  override var controllable: Boolean = false
}

case class YouPredicate(override val location: Point) extends Predicate(location) {
  override var push: Boolean = true

  override def action: Action = YouAction()

  override var controllable: Boolean = false
}

case class WinPredicate(override val location: Point) extends Predicate(location) {
  override var push: Boolean = true

  override def action: Action = WinAction()

  override var controllable: Boolean = false
}


abstract class Environment(val location: Point) extends Block {
  var win = false
  override var coordinates: Point = location

  override var push: Boolean = false
  override var stop: Boolean = false
  var controllable: Boolean = false


}

case class Baba(direction: Direction, override val location: Point) extends Environment(location) {


}

case class Wall(override val location: Point) extends Environment(location) {

}

case class Empty() extends Block {
  override var win = false
  override var coordinates: Point = Point(0, 0)

  override var push: Boolean = false

  override var stop: Boolean = false
  override var controllable: Boolean = false
}
