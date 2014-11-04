package com.jakemadethis.grammargen;

object StringGrammar {

  def entitySeq(string : String) = string.map(c => Entity(c.toString, isCharTerminal(c)))
  def formFromString(string: String) = EntityFormGenerator.createForm(entitySeq(string)) 

  type EntitySeq = Seq[Entity]

  trait Entity {
    val value : String
    val terminal : Boolean
  }
  case class Terminal(value : String) extends Entity {
    assert(value.forall(_.isLower))
    val terminal = true
    override def toString = value
  }
  case class NonTerminal(value : String) extends Entity {
    assert(value.forall(_.isUpper))
    val terminal = false
    override def toString = value
  }
  object Entity {
    def apply(value : String, terminal : Boolean) = if (terminal) Terminal(value) else NonTerminal(value)
  }

  def isCharNonTerminal(thing: Char) = thing.isUpper
  def isCharTerminal(thing: Char) = !thing.isUpper

  implicit object EntityFormGenerator extends FormGenerator[EntitySeq] {

    def createForm(seq : EntitySeq) = {
      val nonTerminals = seq.filter(!_.terminal)
      val numTerminals = seq.count(_.terminal)
      new Form[Entity, EntitySeq](nonTerminals, numTerminals, seq, None)(this)
    }

    def deriveThing(head : EntitySeq, tail : EntitySeq) : EntitySeq = {
      val sentence1 = tail.takeWhile(_.terminal)
      val sentence2 = tail.drop(sentence1.size + 1)
      sentence1 ++ head ++ sentence2
    }
  }

  implicit object CharacterFormConverter extends FormConverter[String, String, Entity, EntitySeq] {
    def createNonTerminal(str : String) = NonTerminal(str)
    def createForm(string : String) = formFromString(string)
  }
  
}