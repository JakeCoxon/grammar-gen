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

  implicit object EntityFormGenerator extends FormGenerator[EntitySeq, StaticForm[Entity, EntitySeq]] {

    // def createForm(seq : EntitySeq) = new EntityForm(seq)

    def createForm(seq : EntitySeq) = {
      val nonTerminals = seq.filter(!_.terminal)
      val numTerminals = seq.count(_.terminal)
      new StaticForm[Entity, EntitySeq](nonTerminals, numTerminals, seq, None)(this)
    }

    def deriveForm(head : EntitySeq, tail : EntitySeq) : EntitySeq = {
      println("Derive")
      println("head", head)
      println("tail", tail)
      val sentence1 = tail.takeWhile(_.terminal)
      val sentence2 = tail.drop(sentence1.size + 1)
      val r = sentence1 ++ head ++ sentence2
      println(r)
      r
    }
  }

  implicit object CharacterFormGenerator extends FormConverter[String, String, Entity, EntitySeq, StaticForm[Entity, EntitySeq]] {
    def createNonTerminal(str : String) = NonTerminal(str)
    def createForm(string : String) : StaticForm[Entity, EntitySeq] = formFromString(string)
  }
  // implicit object CharacterFormGenerator extends FormConverter[String, String, Entity, EntitySeq, EntityForm] {
  //   def createNonTerminal(str : String) = NonTerminal(str)
  //   def createForm(string : String) : EntityForm = formFromString(string)
  // }

  // class EntityForm(thing : EntitySeq) extends ThingForm[Entity, EntitySeq](thing) {
  //   type FormType = EntityForm
  //   lazy val nonTerminals = thing.filter(!_.terminal)
  //   lazy val numTerminals = thing.count(_.terminal)

  //   private lazy val sentence1 = thing.takeWhile(_.terminal)
  //   private lazy val sentence2 = thing.drop(sentence1.size + 1)

  //   def deriveForm(replaceForm: EntityForm): EntityForm = {
  //     new EntityForm(sentence1 ++ replaceForm.thing ++ sentence2)
  //   }
  // }
  
}