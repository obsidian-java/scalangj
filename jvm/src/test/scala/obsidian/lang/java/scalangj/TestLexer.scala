package obsidian.lang.java.scalangj

import obsidian.lang.java.scalangj.*
import obsidian.lang.java.scalangj.Lexer.* // To unify the base class JavaToken, otherwise === will failed


import org.scalatest.{funsuite, matchers}
class TestAnnInterfaceLexer extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val STRING = "@interface"
  val TOKEN:JavaToken = KW_AnnInterface(STRING)

  test("@interface is lexed correlectly") {
    val result = Lexer.parse_one(Lexer.p_ann_interface, STRING)
    assert((result.successful) && (result.get === TOKEN))
  }
}

class TestAbstrLexer extends funsuite.AnyFunSuite with matchers.should.Matchers  {
  val STRING = "abstract"
  val TOKEN:JavaToken = KW_Abstract(STRING)

  test("abstract is lexed correlectly") {
    val result = Lexer.parse_one(Lexer.p_abstract, STRING)
    assert((result.successful) && (result.get === TOKEN))
  }
}

class TestAssert extends funsuite.AnyFunSuite with matchers.should.Matchers  {
  val STRING = "assert"
  val TOKEN:JavaToken = KW_Assert(STRING)

  test("assert is lexed correlectly") {
    val result = Lexer.parse_one(Lexer.p_assert, STRING)
    assert((result.successful) && (result.get === TOKEN))
  }

}

class TestInt1 extends funsuite.AnyFunSuite with matchers.should.Matchers   {
  val STRING = "1234"
  val TOKEN:JavaToken  = IntTok(STRING,1234)
  test(s"int token ${STRING} is lexed correctly") {
    val result = Lexer.parse_one(Lexer.p_IntTok, STRING)
    assert((result.successful) && (result.get === TOKEN))
  }
}

class TestInt2 extends funsuite.AnyFunSuite with matchers.should.Matchers  {
  val STRING = "0x1A1"
  val TOKEN:JavaToken  = IntTok(STRING, 417)
  test(s"int token ${STRING} is lexed correctly") {
    val result = Lexer.parse_one(Lexer.p_IntTok, STRING)
    assert((result.successful) && (result.get === TOKEN))
  }
}

class TestLong1 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val STRING = "1234L"
  val TOKEN:JavaToken = LongTok(STRING,1234)
  test(s"long token ${STRING} is lexed correctly") {
    val result = Lexer.parse_one(Lexer.p_LongTok, STRING)
    assert((result.successful) && (result.get === TOKEN))
  }
}

class TestLong2 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val STRING = "0x1A1l"
  val TOKEN:JavaToken  = LongTok(STRING, 417)
  test(s"long token ${STRING} is lexed correctly") {
    val result = Lexer.parse_one(Lexer.p_LongTok, STRING)
    assert((result.successful) && (result.get === TOKEN))
  }
}

class TestCharTok1 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val STRING = "'A'"
  val TOKEN:JavaToken  = CharTok(STRING, 'A')
  test(s"char token ${STRING} is lexed correctly") {
    val result = Lexer.parse_one(Lexer.p_CharTok, STRING)
    assert((result.successful) && (result.get === TOKEN))

  }
}

class TestCharTok2 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val STRING = "'\t'"
  val TOKEN:JavaToken  = CharTok(STRING, '\t')
  test(s"char token ${STRING} is lexed correctly") {
    val result = Lexer.parse_one(Lexer.p_CharTok, STRING)
    assert((result.successful) && (result.get === TOKEN))
  }
}

class TestCharTok3 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val STRING = "'\t'"
  val TOKEN:JavaToken  = CharTok("'t'", 't')
  test(s"char token ${STRING} is lexed correctly") {
    val result = Lexer.parse_one(Lexer.p_CharTok, STRING)
    assert((result.successful) && (result.get !== TOKEN))
  }
}

class TestStringTok1 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val STRING = """"hello world!""""
  val TOKEN:JavaToken  = StringTok(STRING, "hello world!")
  test(s"string token ${STRING} is lexed correctly") {
    val result = Lexer.parse_one(Lexer.p_StringTok, STRING)
    assert((result.successful) && (result.get === TOKEN))
  }
}

class TestStringTok2 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val STRING = """"hello world!""""
  val TOKEN:JavaToken  = StringTok(""""hello world"""", "hello world")
  test(s"string token ${STRING} is lexed correctly") {
    val result = Lexer.parse_one(Lexer.p_StringTok, STRING)
    assert((result.successful) && (result.get !== TOKEN))
  }
}

class TestIdentTok1 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val STRING = "myAddr_2"
  val TOKEN:JavaToken  = IdentTok("myAddr_2")
  test(s"identifier token ${STRING} is lexed correctly") {
    val result = Lexer.parse_one(Lexer.p_JavaLetterOrDigitTok, STRING)
    assert((result.successful) && (result.get === TOKEN))
  }
}

class TestIdentTok2 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val STRING = "int_vals"
  val TOKEN:JavaToken  = IdentTok("int_vals")
  test(s"identifier token ${STRING} is lexed correctly") {
    val result = Lexer.parse_one(Lexer.p_JavaLetterOrDigitTok, STRING)
    assert((result.successful) && (result.get === TOKEN))
  }
}

class TestLexer1 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val STRING = "int x = 1;"
  val TOKENS:List[JavaToken] = List(KW_Int("int"), IdentTok("x"), Op_Equal("="), IntTok("1", 1), SemiColon(";"))
  test(s"phrase ${STRING} is lexed correctly") {
    val result = Lexer.tokenize(STRING)
    assert((result.successful) && (result.get === TOKENS))
  }
}


class TestLexer2 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val STRING = """
public class HelloWorld {
    public static void main() {
        System.out.println("Hello World!");
    }
} """
  val TOKENS:List[JavaToken] = List(KW_Public("public"), KW_Class("class"), IdentTok("HelloWorld"), OpenCurly("{"),
  KW_Public("public"), KW_Static("static"), KW_Void("void"), IdentTok("main"), OpenParen("("), CloseParen(")"), OpenCurly("{"),
  IdentTok("System"), Period("."), IdentTok("out"), Period("."), IdentTok("println"), OpenParen("("), StringTok(""""Hello World!"""", "Hello World!"), CloseParen(")"), SemiColon(";"),
  CloseCurly("}"),
  CloseCurly("}") 
  )
  test(s"phrase ${STRING} is lexed correctly") {
    val result = Lexer.tokenize(STRING)
    assert((result.successful) && (result.get === TOKENS))
  }
}

class TestLexer3 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val STRING = "int new_vals = 0;"
  val TOKENS:List[JavaToken] = List(KW_Int("int"), IdentTok("new_vals"), Op_Equal("="), IntTok("0", 0), SemiColon(";"))
  test(s"phrase ${STRING} is lexed correctly") {
    val result = Lexer.tokenize(STRING)
    assert((result.successful) && (result.get === TOKENS))
  }
}

class TestLexer4 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val STRING = "int intvals = 0;"
  val TOKENS:List[JavaToken] = List(KW_Int("int"), IdentTok("intvals"), Op_Equal("="), IntTok("0", 0), SemiColon(";"))
  test(s"phrase ${STRING} is lexed correctly") {
    val result = Lexer.tokenize(STRING)
    assert((result.successful) && (result.get === TOKENS))
  }
}

class TestLexer5 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val STRING = "boolean ifvals = true;"
  val TOKENS:List[JavaToken] = List(KW_Boolean("boolean"), IdentTok("ifvals"), Op_Equal("="), BoolTok("true", true), SemiColon(";"))
  test(s"phrase ${STRING} is lexed correctly") {
    val result = Lexer.tokenize(STRING)
    assert((result.successful) && (result.get === TOKENS))
  }
}

class TestLexer6 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val STRING = "ifvals"
  val TOKENS:List[JavaToken] = List(IdentTok("ifvals"))
  test(s"phrase ${STRING} is lexed correctly") {
    val result = Lexer.tokenize(STRING)
    assert((result.successful) && (result.get === TOKENS))
  }
}

class TestLexer7 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val STRING = "iiff"
  val TOKENS:List[JavaToken] = List(IdentTok("iiff"))
  test(s"phrase ${STRING} is lexed correctly") {
    val result = Lexer.tokenize(STRING)
    assert((result.successful) && (result.get === TOKENS))
  }
}

class TestLexer8 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val STRING = "trueif"
  val TOKENS:List[JavaToken] = List(IdentTok("trueif"))
  test(s"phrase ${STRING} is lexed correctly") {
    val result = Lexer.tokenize(STRING)
    assert((result.successful) && (result.get === TOKENS))
  }
}

class TestLexer9 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val STRING = "if"
  val TOKENS:List[JavaToken] = List(KW_If("if"))
  test(s"phrase ${STRING} is lexed correctly") {
    val result = Lexer.tokenize(STRING)
    assert((result.successful) && (result.get === TOKENS))
  }
}

class TestLexer10 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val STRING = "int x = 1;"
  val TOKENS:List[JavaToken] = List(KW_Int("int"), IdentTok("x"), Op_Equal("="), IntTok("1", 1), SemiColon(";"))
  test(s"phrase ${STRING} is lexed correctly") {
    val result = Lexer.tokenize(STRING)
    println(result)
    assert((result.successful) && (result.get === TOKENS))
  }
}

class TestLexer11 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val STRING = "boolean x = true;"
  val TOKENS:List[JavaToken] = List(KW_Boolean("boolean"), IdentTok("x"), Op_Equal("="), BoolTok("true", true), SemiColon(";"))
  test(s"phrase ${STRING} is lexed correctly") {
    val result = Lexer.tokenize(STRING)
    println(result)
    assert((result.successful) && (result.get === TOKENS))
  }
}

class TestLexer12 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val STRING = "boolean x = 1;"
  val TOKENS:List[JavaToken] = List(KW_Boolean("boolean"), IdentTok("x"), Op_Equal("="), IntTok("1", 1), SemiColon(";"))
  test(s"phrase ${STRING} is lexed correctly") {
    val result = Lexer.tokenize(STRING)
    println(result)
    assert((result.successful) && (result.get === TOKENS))
  }
}