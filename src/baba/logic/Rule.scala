package baba.logic

case class Rule(subject: Subject, predicate: Predicate) {
  def evaluate(blocks: List[Block]): Unit = {
    blocks.foreach { block =>
      if (matchesSubject(block)) predicate.action.applyAction(block)
    }
  }

  def revert(blocks: List[Block]): Unit = {
    blocks.foreach { block =>
      if (matchesSubject(block)) predicate.action.revertAction(block)
    }
  }

  def matchesSubject(block: Block): Boolean = {
    subject match {
      case subject: WallRule => block.isInstanceOf[Wall]
      case subject: BabaSubject => block.isInstanceOf[Baba]
      case _ => false
    }
  }
}

trait Action {
  def applyAction(block: Block): Unit

  def revertAction(block: Block): Unit
}

case class StopAction() extends Action {
  def applyAction(block: Block): Unit = block.stop = true

  def revertAction(block: Block): Unit = block.stop = false
}

case class YouAction() extends Action {
  def applyAction(block: Block): Unit = {
    if (block.isInstanceOf[Environment]) {
      block.asInstanceOf[Environment].controllable = true
    }
  }


  def revertAction(block: Block): Unit = {
    if (block.isInstanceOf[Environment]) {
      block.asInstanceOf[Environment].controllable = false
    }
  }
}


