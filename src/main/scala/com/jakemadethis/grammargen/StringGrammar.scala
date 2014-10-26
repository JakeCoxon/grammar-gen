package com.jakemadethis.grammargen;

object StringGrammar {

  def entitySeq(string : String) = string.map(c => Entity(c.toString, isCharTerminal(c)))
  def formFromString(string: String) = new EntityForm(entitySeq(string)) 

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

  implicit object EntityFormGenerator extends FormGenerator[EntitySeq, EntityForm] {
    def createForm(seq : EntitySeq) = new EntityForm(seq)
  }

  implicit object CharacterFormGenerator extends FormConverter[String, String, Entity, EntitySeq, EntityForm] {
    def createNonTerminal(str : String) = NonTerminal(str)
    def createForm(string : String) : EntityForm = formFromString(string)
  }

  class EntityForm(val seq : EntitySeq) extends Form[Entity, EntitySeq](seq) {
    type FormType = EntityForm
    val nonTerminals = seq.filter(!_.terminal)
    val numTerminals = seq.count(_.terminal)

    lazy val (sentence1, sentence2) = split

    def split = {
      val s1 = seq.takeWhile(_.terminal)
      val s2 = seq.drop(s1.size + 1)
      (s1, s2) 
    }

    def deriveForm(replaceForm: EntityForm): EntityForm = {
      new EntityForm(sentence1 ++ replaceForm.seq ++ sentence2)
    }
  }
  
}