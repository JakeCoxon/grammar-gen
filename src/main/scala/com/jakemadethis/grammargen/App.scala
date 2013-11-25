package com.jakemadethis.grammargen;

object App {

  var numTests = 0

  def assertEq[T](a : T, b : T) { 
    if (a != b) throw new Error("Assertion failed " + a + " != " + b)
    numTests += 1
  }

  def main(args : Array[String]) {

    locally {
      val form1 = StringGrammar.characterForm("aAbBaaBbbbBB")
      val form2 = StringGrammar.characterForm("AAaa")
      val form3 = StringGrammar.characterForm("c")
      val form4 = StringGrammar.characterForm("C")

      assertEq(form1.isSingleton, false)
      assertEq(form2.isSingleton, false)
      assertEq(form3.isSingleton, false)
      assertEq(form4.isSingleton, true)

      assertEq(form1.isTerminal, false)
      assertEq(form2.isTerminal, false)
      assertEq(form3.isTerminal, true)
      assertEq(form4.isTerminal, false)

      assertEq(form1.headOption, Some("A"))
      assertEq(form2.headOption, Some("A"))
      assertEq(form3.headOption, None)
      assertEq(form4.headOption, Some("C"))

      assertEq(form1.nonTerminals, Seq("A","B","B","B","B"))
      assertEq(form1.numTerminals, 7)

      val derived1 = form1.deriveState(form2)

      assertEq(derived1.nonTerminals, Seq("A","A","B","B","B","B"))
      assertEq(derived1.numTerminals, 9)

      val derived2 = derived1.deriveState(form3)

      assertEq(derived2.nonTerminals, Seq("A","B","B","B","B"))
      assertEq(derived2.numTerminals, 10)
    }

    locally {
      def prods(prods : (String, String)*) =
        prods.map { case (l, r) => new ProductionRule(l, StringGrammar.characterForm(r)) }

      val init = StringGrammar.characterForm("A")
      val grammar = new Grammar(prods("A" -> "AB", "B" -> "b"), init)

      assertEq(grammar.map("A").size, 1)
      assertEq(grammar.map("B").size, 1)
      assertEq(grammar.nonTerminals, Set("A", "B"))
    }

    println(numTests + " tests pass")
  }

}