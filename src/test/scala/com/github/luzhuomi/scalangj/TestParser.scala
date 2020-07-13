package com.github.luzhuomi.scalangj

import com.github.luzhuomi.scalangj._
import com.github.luzhuomi.scalangj.Parser._ // To unify the base class JavaToken, otherwise === will failed
import com.github.luzhuomi.scalangj.Syntax._
import org.scalatest.{FunSuite, Matchers}


// need to add a scanner to the lexer to produce Reader[JavaToken]
// http://matt.might.net/articles/implementation-of-m-expression-parser-in-scala-combinators-without-stdlexical-stdtokenparsers/



class TestLitParser1 extends FunSuite with Matchers {
  val STRING = "1"
  val LITERAL = IntLit(1)
  test("Literal is parsed correlectly") {
    val result = parseLiteral(STRING)
    assert((result.successful) && (result.get === LITERAL))
  }
}


class TestParser1 extends FunSuite with Matchers {
  val STRING = "int x = 1;"
  val ty:Type = PrimType_(IntT)
  val vardecls:List[VarDecl] = List(VarDecl(VarId(Ident("x")), Some(InitExp(Lit(IntLit(1))))))
  val LOCALVARDECL:(List[Modifier], Type, List[VarDecl]) = (Nil, ty, vardecls)
  test(s"phrase ${STRING} is parsed correctly") {
    val result = localVarDecl(new Lexer.Scanner(STRING))
    assert((result.successful) && (result.get === LOCALVARDECL))
  }
}


class TestParser2 extends FunSuite with Matchers {
  val STRING = """
public class HelloWorld {
    public static void main() {
        System.out.println("Hello World!");
    }
} 
  """
  val CLASSDECL = null
  test(s"phrase ${STRING} is parsed correctly") {
    val result = classOrInterfaceDecl.apply(new Lexer.Scanner(STRING))
    assert((result.successful))// && (result.get === TOKENS))
  }
}


class TestParser3 extends FunSuite with Matchers {
  val STRING = """
        System.out.println("Hello World!");
  """
  val CLASSDECL = null
  test(s"phrase ${STRING} is parsed correctly") {
    val result = methodInvocationExp.apply(new Lexer.Scanner(STRING))
    assert((result.successful))// && (result.get === TOKENS))
  }
}


class TestParser4 extends FunSuite with Matchers {
  val STRING = "System.out.println"
  val CLASSDECL = null
  test(s"phrase ${STRING} is parsed correctly") {
    val result = name.apply(new Lexer.Scanner(STRING))
    assert((result.successful))// && (result.get === TOKENS))
  }
}