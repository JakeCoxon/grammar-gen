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
      val form : Form[Entity, EntitySeq] = Form("asd")(CharacterFormGenerator)

    }
    // Grammar Gen

    

    locally {

      import StringGrammar._

      val Initial = NonTerminal("S")

      def grammar(prodStrings : (String, String)*) = {

        new WrappedGrammar(prodStrings)(CharacterFormGenerator)
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
          "S" -> "a", "S" -> "Sa", "S" -> "Saa"
        )
        val gen = new InfiniteDerivationGenerator(Form("S"), gram)
        assertEq(gen.derivations(0).result, Seq(Terminal("a")))
        assertEq(gen.derivations(1).result, Seq(NonTerminal("S"), Terminal("a")))
        assertEq(gen.derivations(1).derivations(0).result, Seq(Terminal("a"), Terminal("a")))
      }

      locally {
        val list = LazyList(() => {println("LOL"); 1 }, () => {println("Second"); 2 })
        println(list.size)
        println(list(1))
        println(list(1))
        println(list(1))
      }

    }

    println(numTests + " tests pass")
  }

}