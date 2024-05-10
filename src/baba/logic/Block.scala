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

  override var stop: Boolean = true
  override var push: Boolean = true
  override var controllable: Boolean = false
  override var win: Boolean = false

  override var coordinates: Point = location

}

case class Connector(override val location: Point) extends RuleBlock(location) {
}

abstract class Subject(override val location: Point) extends RuleBlock(location) {
}

case class WallSubject(override val location: Point) extends Subject(location) {
}

case class BabaSubject(override val location: Point) extends Subject(location) {
}

abstract class Predicate(override val location: Point) extends RuleBlock(location) {
  def property: Property

}

case class StopPredicate(override val location: Point) extends Predicate(location) {
  override def property: Property = StopProperty()

}

case class PushPredicate(override val location: Point) extends Predicate(location) {
  override def property: Property = PushProperty()

}

case class YouPredicate(override val location: Point) extends Predicate(location) {
  override def property: Property = YouProperty()

}

case class WinPredicate(override val location: Point) extends Predicate(location) {
  override def property: Property = WinProperty()

}


abstract class Environment(val location: Point) extends Block {

  override var coordinates: Point = location

  override var push: Boolean = false
  override var stop: Boolean = false
  override var controllable: Boolean = false
  override var win = false
}

case class Baba(direction: Direction, override val location: Point) extends Environment(location) {
}

case class Wall(override val location: Point) extends Environment(location) {
}

case class Empty(override val location: Point) extends Environment(location) {
}
