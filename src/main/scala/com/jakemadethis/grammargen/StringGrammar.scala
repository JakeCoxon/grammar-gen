package com.jakemadethis.grammargen;

object StringGrammar {

  def characterForm(string: String) =
    stringForm(string.map {
      case nt if nt.isUpper => new NonTerminal(nt.toString)
      case t => new Terminal(t.toString)
    })

  sealed abstract class Entity(val value: String)
  class Terminal(value: String) extends Entity(value)
  class NonTerminal(value: String) extends Entity(value)

  def nonTerminals(entities: Seq[Entity]) = entities.collect { case nt: NonTerminal => nt.value }
  def numTerminals(entities: Seq[Entity]) = entities.collect { case t: Terminal => t }.size

  def stringForm(entities: Seq[Entity]) = new Form(nonTerminals(entities), numTerminals(entities))
  
}