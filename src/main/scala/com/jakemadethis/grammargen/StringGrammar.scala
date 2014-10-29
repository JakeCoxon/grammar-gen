package com.jakemadethis.grammargen;

object StringGrammar {

  def entitySeq(string : String) = string.map(c => Entity(c.toString, isCharTerminal(c)))
  def formFromString(string: String) = EntityFormGenerator.createForm(entitySeq(string)) 

  type EntitySeq = Seq[Entity]

  case class Entity(val value : String, val terminal : Boolean)
  object Terminal {
    def apply(value: String) = Entity(value, true)
    def unapply(ent : Entity) = ent.terminal
  }
  object NonTerminal {
    def apply(value: String) = Entity(value, false)
    def unapply(ent : Entity) = !ent.terminal
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

  implicit object CharacterFormGenerator extends FormConverter[String, String, Entity, EntitySeq] {
    def createNonTerminal(str : String) = NonTerminal(str)
    def createForm(string : String) = formFromString(string)
  }
  
}