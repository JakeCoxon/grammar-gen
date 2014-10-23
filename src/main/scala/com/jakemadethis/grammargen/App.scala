package com.jakemadethis.grammargen;

object App {

  var numTests = 0

  def assertEq[T](a : T, b : T) { 
    if (a != b) throw new Error("Assertion failed " + a + " != " + b)
    numTests += 1
  }

  def time(f: => Unit) {
    val t0 = System.nanoTime(); f
    val result = (System.nanoTime() - t0) / 1000000
    println(s"Elapsed time: ${result}ms")
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

    // String grammar

    locally {

      import StringGrammar._

      def prods(prods : (String, String)*) =
        prods.map { case (l, r) => new ProductionRule(l, StringGrammar.characterForm(r)) }

      val init = StringGrammar.characterForm("A")
      val grammar = new Grammar(prods("A" -> "AB", "B" -> "b"), init)

      assertEq(grammar.map("A").size, 1)
      assertEq(grammar.map("B").size, 1)
      // assertEq(grammar.nonTerminals.size, 2)
    }


    // Graph

    locally {

      class Vertex
      class Edge(val label: String)
      val a, b, c = new Vertex
      val graph = Hypergraph[Vertex, Edge](a, b, c)
        .addEdge(new Edge("A"), List(a, b))
        .addEdge(new Edge("A"), List(b, c))


      val handle = new Handle(new Edge("A"), new Vertex :: new Vertex :: new Vertex :: Nil)

    }

    // Grammar Gen

    def grammar(prodStrings : (String, String)*) = {
      val prods = prodStrings.map { case (l, r) => 
        new ProductionRule(l, StringGrammar.characterForm(r)) 
      }

      val init = StringGrammar.characterForm("S")
      new Grammar(prods, init)
    }

    locally {

      locally {
        val enum = new GrammarEnumerator(grammar("S" -> "a", "S" -> "a"))
        assertEq(enum.count("S", 1), 2)
      }

      locally {
        val enum = new GrammarEnumerator(grammar("S" -> "a", "S" -> "aa", "S" -> "jake"))
        enum.precompute(4)
        assertEq(enum.count("S", 1), 1)
        assertEq(enum.count("S", 2), 1)
        assertEq(enum.count("S", 3), 0)
        assertEq(enum.count("S", 4), 1)
        assertEq(enum.count("S", 5), 0)
      }

      locally {
        val enum = new GrammarEnumerator(grammar("S" -> "a", "S" -> "Sa"))
        enum.precompute(100)
        assertEq(enum.count("S", 1), 1)
        assertEq(enum.count("S", 2), 1)
        assertEq(enum.count("S", 100), 1)
      }

      locally {
        val enum = new GrammarEnumerator(grammar(
          "S" -> "a", "S" -> "Sa", "S" -> "Saa"
        ))
        enum.precompute(4)
        assertEq(enum.count("S", 1), 1)
        assertEq(enum.count("S", 2), 1)
        assertEq(enum.count("S", 4), 3)
      }

      locally {
        val enum = new GrammarEnumerator(grammar(
          "S" -> "a", "S" -> "Sa", "S" -> "SSa"
        ))
        enum.precompute(5)
        assertEq(enum.count("S", 5), 9)
      }

      locally {
        val enum = new GrammarEnumerator(grammar(
          "S" -> "s", "S" -> "ss", "S" -> "SA",
          "A" -> "Aa", "A" -> "a"
        ))
        enum.precompute(6)
        assertEq(enum.count("S", 1), 1)
        assertEq(enum.count("S", 2), 2)
        assertEq(enum.count("S", 3), 3)
        assertEq(enum.count("S", 4), 6)
        assertEq(enum.count("S", 5), 12)
        assertEq(enum.count("S", 6), 24)
      }

      // locally {
      //   val enum = new GrammarEnumerator(grammar(
      //     "S" -> "s", "S" -> "ss", "S" -> "SA",
      //     "A" -> "Aa", "A" -> "a"
      //   ))
      //   time {
      //     enum.precompute(200)
      //     enum.count("S", 200)
      //   }
      // }

    }

    println(numTests + " tests pass")
  }

}