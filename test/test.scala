
abstract class Form[T, F <: Form[T, F]](val nonTerminals : List[T], val numTerminals : Int) {
  type Entity = T

  assert(numTerminals >= 0)

  private def nonTerminalsString = nonTerminals.mkString
  override def toString = s"Form($nonTerminalsString, $numTerminals)"

  def replaceHead(newForm : F) : F

}

object StringForm {
  def nonTerminals(entities: List[Char]) : List[Char] = entities.collect { case nt if nt.isUpper => nt }
  def numTerminals(entities: List[Char]) = entities.collect { case t if t.isLower => t }.size

  def apply(string : String) = new StringForm(string.toList)
}

class StringForm(val entities : List[Char]) 
  extends Form[Char, StringForm](StringForm.nonTerminals(entities), StringForm.numTerminals(entities)) {

  val nonTerminalPositions = entities.zipWithIndex.collect { 
    case (nt, index) if nt.isUpper => index
  }

  def replaceHead(form : StringForm) : StringForm =
    nonTerminalPositions match {
      case headIndex :: _ =>
        println(entities.patch(headIndex, form.entities, 1))
          new StringForm(entities.patch(headIndex, form.entities, 1))
      case _ => throw new UnsupportedOperationException("There are no non-terminals in this string")
    }

  override def toString = entities.mkString
    

}

object App {
  def main(args : Array[String]) {
    val a = StringForm("abCFdEfg")
    println(a)

    val b = a.replaceHead(StringForm("aaa"))
    println(b)

    val c = b.replaceHead(StringForm("aaa"))
    println(c)
  }
}

// val form1 = StringForm("aAabBbaB")
// val grammar1 = new StringGrammar(
//   "A" -> "aAB",
//   "A" -> "aa",
//   "B" -> "bbbb")
 
// val derivation1 = StringDerivation("aAB")
 
// form1.applyFirst(grammar1)
// form1.applyFirst(derivation1)