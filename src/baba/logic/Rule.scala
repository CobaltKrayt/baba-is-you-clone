package baba.logic

case class Rule(subject: Subject, predicate: Predicate) {
  def evaluate(blocks: List[Block]): Unit = {
    // Here, you would implement the logic of what the rule means.
    // For example, if subject is Wall and predicate is Stop,
    // then walls become obstacles that can't be passed through.
    if (subject.isInstanceOf[WallRule] && predicate.isInstanceOf[StopRule]) {
      blocks.foreach {
        case wall: Wall => wall.stop = true
        case _ =>
      }
    }
  }
}
