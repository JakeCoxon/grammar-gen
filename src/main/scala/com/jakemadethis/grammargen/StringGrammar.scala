package com.jakemadethis.grammargen;

object StringGrammar {

  def characterForm(string: String) =
    new StringForm(string.map {
      case nt if nt.isUpper => new NonTerminal(nt.toString)
      case t => new Terminal(t.toString)
    })

  sealed abstract class Entity(value: String)
  class Terminal(val value: String) extends Entity(value)
  class NonTerminal(val value: String) extends Entity(value)

  def nonTerminals(entities: Seq[Entity]) = entities.collect { case nt: NonTerminal => nt.value }
  def numTerminals(entities: Seq[Entity]) = entities.collect { case t: Terminal => t }.size

  class StringForm(entities: Seq[Entity]) extends Form(nonTerminals(entities), numTerminals(entities))
  
}