package baba.logic

case class Rule(subject: Subject, predicate: Predicate) {
  def enactOn(blocks: List[Block]): Unit = {
    blocks.foreach { block =>
      if (matchesSubject(block)) predicate.property.applyProperty(block)
    }
  }

  def repealOn(blocks: List[Block]): Unit = {
    blocks.foreach { block =>
      if (matchesSubject(block)) predicate.property.removeProperty(block)
    }
  }

  def matchesSubject(block: Block): Boolean = {
    subject match {
      case subject: WallSubject => block.isInstanceOf[Wall]
      case subject: BabaSubject => block.isInstanceOf[Baba]
      case _ => false
    }
  }
}

trait Property {
  def applyProperty(block: Block): Unit

  def removeProperty(block: Block): Unit
}

case class PushProperty() extends Property {
  def applyProperty(block: Block): Unit = {
    block.stop = true
    block.push = true
  }

  def removeProperty(block: Block): Unit = {
    block.stop = false
    block.push = false
  }
}

case class StopProperty() extends Property {
  def applyProperty(block: Block): Unit = block.stop = true

  def removeProperty(block: Block): Unit = block.stop = false
}

case class YouProperty() extends Property {
  def applyProperty(block: Block): Unit = {
    if (block.isInstanceOf[Environment]) {
      block.asInstanceOf[Environment].controllable = true
    }
  }


  def removeProperty(block: Block): Unit = {
    if (block.isInstanceOf[Environment]) {
      block.asInstanceOf[Environment].controllable = false
    }
  }
}

case class WinProperty() extends Property {
  def applyProperty(block: Block): Unit = block.win = true

  def removeProperty(block: Block): Unit = block.win = false
}
