package com.jakemadethis.grammargen

import com.jakemadethis.grammargen.generators._

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
      import StringGrammar._

      val form1 = Form("aAbBaaBbbbBB")
      val form2 = Form("AAaa")
      val form3 = Form("c")
      val form4 = Form("C")

      assertEq(form1.isSingleton, false)
      assertEq(form2.isSingleton, false)
      assertEq(form3.isSingleton, false)
      assertEq(form4.isSingleton, true)

      assertEq(form1.isTerminal, false)
      assertEq(form2.isTerminal, false)
      assertEq(form3.isTerminal, true)
      assertEq(form4.isTerminal, false)

      assertEq(form1.headOption.map(_.value), Some("A"))
      assertEq(form2.headOption.map(_.value), Some("A"))
      assertEq(form3.headOption, None)
      assertEq(form4.headOption.map(_.value), Some("C"))

      assertEq(form1.nonTerminals.map(_.value), Seq("A","B","B","B","B"))
      assertEq(form1.numTerminals, 7)

      val derived1 = form1.deriveForm(form2)

      assertEq(derived1.nonTerminals.map(_.value), Seq("A","A","B","B","B","B"))
      assertEq(derived1.numTerminals, 9)

      val derived2 = derived1.deriveForm(form3)

      assertEq(derived2.nonTerminals.map(_.value), Seq("A","B","B","B","B"))
      assertEq(derived2.numTerminals, 10)
    }

    // String grammar

    locally {

      import StringGrammar._
      import StringGrammar.EntityFormGenerator

      val grammar = new WrappedGrammar(Seq("A" -> "AB", "B" -> "b"))

      assertEq(grammar.wrapKey("A").size, 1)
      assertEq(grammar.wrapKey("B").size, 1)
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

    locally {

      import StringGrammar._
      val form : Form[Entity, EntitySeq] = Form("asd")(CharacterFormConverter)

    }
    // Grammar Gen

    

    locally {

      import StringGrammar._

      val Initial = NonTerminal("S")

      def grammar(prodStrings : (String, String)*) = {

        new WrappedGrammar(prodStrings)(CharacterFormConverter)
      }


      locally {
        val enum = new GrammarEnumerator(grammar("S" -> "a", "S" -> "a"))
        assertEq(enum.count(Initial, 1), 2)
      }

      locally {
        val enum = new GrammarEnumerator(grammar("S" -> "a", "S" -> "aa", "S" -> "jake"))
        enum.precompute(4)
        assertEq(enum.count(Initial, 1), 1)
        assertEq(enum.count(Initial, 2), 1)
        assertEq(enum.count(Initial, 3), 0)
        assertEq(enum.count(Initial, 4), 1)
        assertEq(enum.count(Initial, 5), 0)
      }

      locally {
        val enum = new GrammarEnumerator(grammar("S" -> "a", "S" -> "Sa"))
        enum.precompute(100)
        assertEq(enum.count(Initial, 1), 1)
        assertEq(enum.count(Initial, 2), 1)
        assertEq(enum.count(Initial, 100), 1)
      }

      locally {
        val enum = new GrammarEnumerator(grammar(
          "S" -> "a", "S" -> "Sa", "S" -> "Saa"
        ))
        enum.precompute(4)
        assertEq(enum.count(Initial, 1), 1)
        assertEq(enum.count(Initial, 2), 1)
        assertEq(enum.count(Initial, 4), 3)
      }

      locally {
        val enum = new GrammarEnumerator(grammar(
          "S" -> "a", "S" -> "Sa", "S" -> "SSa"
        ))
        enum.precompute(5)
        assertEq(enum.count(Initial, 5), 9)
      }

      locally {
        val enum = new GrammarEnumerator(grammar(
          "S" -> "s", "S" -> "ss", "S" -> "SA",
          "A" -> "Aa", "A" -> "a"
        ))
        enum.precompute(6)
        assertEq(enum.count(Initial, 1), 1)
        assertEq(enum.count(Initial, 2), 2)
        assertEq(enum.count(Initial, 3), 3)
        assertEq(enum.count(Initial, 4), 6)
        assertEq(enum.count(Initial, 5), 12)
        assertEq(enum.count(Initial, 6), 24)
      }

      // locally {
      //   val enum = new GrammarEnumerator(grammar(
      //     "S" -> "s", "S" -> "ss", "S" -> "SA",
      //     "A" -> "Aa", "A" -> "a"
      //   ))
      //   time {
      //     // enum.precompute(200)
      //     enum.count(Initial, 2)
      //   }
      // }

      locally {
        val gram = grammar(
          "S" -> "a", "S" -> "Sa", "S" -> "SSaa"
        )
        val gen = new UnboundedDerivationGenerator(Form("S"), gram)
        assertEq(gen.derivations.size, 3)
        assertEq(gen.derivations(0).result, Seq(Terminal("a")))
        assertEq(gen.derivations(1).result, Seq(NonTerminal("S"), Terminal("a")))
        assertEq(gen.derivations(2).result, Seq(NonTerminal("S"), NonTerminal("S"), Terminal("a"), Terminal("a")))

        assertEq(gen.derivations(1).derivations.size, 3)
        assertEq(gen.derivations(1).result, Seq(NonTerminal("S"), Terminal("a")))
        assertEq(gen.derivations(1).derivations(0).result, Seq(Terminal("a"), Terminal("a")))

        assertEq(gen.derivations(2).derivations(0).result, Seq(Terminal("a"), NonTerminal("S"), Terminal("a"), Terminal("a")))

        assertEq(gen.derivations(1).derivations(1).derivations(1).derivations(1).derivations(1).
                     derivations(1).derivations(1).derivations(1).derivations(1).derivations(1).
                     derivations(1).derivations(1).derivations(1).derivations(1).derivations(1).result.size, 16)
      }

      locally {
        val gram = grammar(
          "S" -> "AAa", "A" -> "a"
        )
        val enumerator = new GrammarEnumerator(gram)
        val gen = new BoundedDerivationGenerator(Form("S"), enumerator)(3)
        assertEq(gen.derivations.size, 1)
        assertEq(gen.derivations(0).result, Seq(NonTerminal("A"), NonTerminal("A"), Terminal("a")))
        assertEq(gen.derivations(0).derivations.size, 1)
        assertEq(gen.derivations(0).derivations(0).result, Seq(Terminal("a"), NonTerminal("A"), Terminal("a")))
      }

      locally {
        val gram = grammar(
          "S" -> "a", "S" -> "Sa", "S" -> "SSaa"
        )
        val enumerator = new GrammarEnumerator(gram)
        val gen = new BoundedDerivationGenerator(Form("S"), enumerator)(5)
        assertEq(gen.derivations.size, 2)
        assertEq(gen.derivations(0).result, Seq(NonTerminal("S"), Terminal("a")))
        assertEq(gen.derivations(1).result, Seq(NonTerminal("S"), NonTerminal("S"), Terminal("a"), Terminal("a")))

        assertEq(gen.derivations(1).result, Seq(NonTerminal("S"), NonTerminal("S"), Terminal("a"), Terminal("a")))
        assertEq(gen.derivations(1).derivations.size, 2)
        assertEq(gen.derivations(1).derivations(0).result, Seq(Terminal("a"), NonTerminal("S"), Terminal("a"), Terminal("a")))
        assertEq(gen.derivations(1).derivations(1).result, Seq(NonTerminal("S"), Terminal("a"), NonTerminal("S"), Terminal("a"), Terminal("a")))

        assertEq(gen.derivations(1).derivations(0).derivations(0).result, Seq(Terminal("a"), NonTerminal("S"), Terminal("a"), Terminal("a"), Terminal("a")))

        assertEq(gen.derivations(0).derivations(0).derivations(0).derivations(0).derivations(0).result, 
            Seq(Terminal("a"), Terminal("a"), Terminal("a"), Terminal("a"), Terminal("a")))
        assertEq(gen.derivations(0).derivations(0).derivations(0).derivations(0).derivations(0).derivations.size, 0)
      }

      locally {
        val gram = grammar(
          "S" -> "ABaa", "A" -> "aa", "B" -> "bbbbbbbb", "B" -> "b"
        )
        val enumerator = new GrammarEnumerator(gram)
        val gen = new BoundedDerivationGenerator(Form("S"), enumerator)(5)
        assertEq(gen.derivations.size, 1)
        assertEq(gen.derivations(0).result, Seq(NonTerminal("A"), NonTerminal("B"), Terminal("a"), Terminal("a")))
        assertEq(gen.derivations(0).derivations.size, 1)
        assertEq(gen.derivations(0).derivations(0).result, Seq(Terminal("a"), Terminal("a"), NonTerminal("B"), Terminal("a"), Terminal("a")))
        assertEq(gen.derivations(0).derivations(0).derivations.size, 1)
        assertEq(gen.derivations(0).derivations(0).derivations(0).result, Seq(Terminal("a"), Terminal("a"), Terminal("b"), Terminal("a"), Terminal("a")))
      }

    }

    println(numTests + " tests pass")
  }

}