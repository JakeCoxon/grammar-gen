package com.jakemadethis.grammargen;

object StringGrammar {

  def characterForm(string: String) =
    new StringForm(string.map {
      case nt if nt.isUpper => new NonTerminal(nt.toString)
      case t => new Terminal(t.toString)
    })

  sealed abstract class Entity(val value: String)
  class Terminal(value: String) extends Entity(value)
  class NonTerminal(value: String) extends Entity(value) with Morphism[String] {
    override def hashCode = value.hashCode
    override def equals(other : Any) = other match {
      case s : NonTerminal => s.value == value
      case _ => false
    }
  }

  def nonTerminals(entities: Seq[Entity]) = entities.collect { case nt: NonTerminal => nt.value }
  def numTerminals(entities: Seq[Entity]) = entities.collect { case t: Terminal => t }.size

  class StringForm(entities: Seq[Entity]) extends Form(nonTerminals(entities), numTerminals(entities))
  
  implicit object StringHasMorphism extends HasMorphism[String] {
    def create(s : String) = new NonTerminal(s)
  }

}