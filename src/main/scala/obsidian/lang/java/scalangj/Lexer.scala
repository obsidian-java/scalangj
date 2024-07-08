package obsidian.lang.java.scalangj
// https://argodis.de/blog/2019-05-09/formula-parsing-with-scala-lexer/index.html
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.*
import scala.util.parsing.combinator.lexical.*
//import org.apache.commons.text.StringEscapeUtils.*
import scala.StringContext
import scala.util.parsing.input.Positional
import obsidian.lang.java.scalangj.*
import scala.util.parsing.input.CharArrayReader.EofCh

object Lexer extends Lexical with Positional with JavaTokens with RegexParsers {
  /* RegexParsers with */   
  // type JavaToken = Token
  override type Elem = Char // has to override to fix the clashing between Scanner (via Lexical) and RegexParser
  override def skipWhitespace = true
  override val whiteSpace: Regex = "[ \t\r\n\f]+".r

  // copy from StdLexical: begin
  def whitespace: Parser[Any] = rep[Any](
      whitespaceChar
    | '/' ~ '*' ~ comment
    | '/' ~ '/' ~ rep( chrExcept(EofCh, '\n') )
    | '/' ~ '*' ~ failure("unclosed comment")
    )

  protected def comment: Parser[Any] = (
    rep (chrExcept (EofCh, '*')) ~ '*' ~ '/'     ^^ { _ => ' ' }
  | rep (chrExcept (EofCh, '*')) ~ '*' ~ comment ^^ { _ => ' ' }
  )
  // copy from StdLexical: end
  val digitp: Regex = "[0-9]".r // renamed to avoid conflict with Scanner.digit
  val nonzero: Regex = "[1-9]".r
  val octdig: Regex = "[0-7]".r
  val hexdig: Regex = "[0-9A-Fa-f]".r

  val lineterm: Regex = "[\n\r] | \r\n".r
  val tradcom_in: Regex = s"( ~[\\*] | \\*+ (~[\\/\\*] | \n) | \n )* \\*+".r
  val tradcomm: Regex = s"/*${tradcom_in}/".r
  val linecomm: Regex = s"//.*${lineterm}".r
  val comm: Regex = s"${tradcomm}|${linecomm}".r

  val octEscape: Regex = s"[0123]?${octdig}{1,2}".r
  val hexEscape: Regex = s"u${hexdig}{4}".r
  val charEscape: Regex = s"""(${octEscape}|${hexEscape}|[btnfr\"\'\\\\])""".r
  // val charEscape:Regex = s"""\\\\(${octEscape}|${hexEscape})""".r
  val expsuffix: Regex = s"[+-]?${digitp}+".r
  val exponent: Regex = s"[eE]${expsuffix}".r
  val pexponent: Regex = s"[pP]${expsuffix}".r

  val javaLetter: Regex = """[a-zA-Z\_\$]""".r
  val javaDigit: Regex = digitp
  val javaLetterOrDigit: Regex = """[a-zA-Z0-9\_\$]""".r

  def p_ann_interface: Parser[JavaToken] = "@interface" ^^ { s =>
    KW_AnnInterface(s)
  }

  def p_abstract: Parser[JavaToken] = "abstract" ^^ { s => KW_Abstract(s) }

  def p_assert: Parser[JavaToken] = "assert" ^^ { s => KW_Assert(s) }

  def p_boolean: Parser[JavaToken] = "boolean" ^^ { s => KW_Boolean(s) }

  def p_break: Parser[JavaToken] = "break" ^^ { s => KW_Break(s) }

  def p_byte: Parser[JavaToken] = "byte" ^^ { s => KW_Byte(s) }

  def p_case: Parser[JavaToken] = "case" ^^ { s => KW_Case(s) }

  def p_catch: Parser[JavaToken] = "catch" ^^ { s => KW_Catch(s) }

  def p_char: Parser[JavaToken] = "char" ^^ { s => KW_Char(s) }

  def p_class: Parser[JavaToken] = "class" ^^ { s => KW_Class(s) }

  def p_const: Parser[JavaToken] = "const" ^^ { s => KW_Const(s) }

  def p_continue: Parser[JavaToken] = "continue" ^^ { s => KW_Continue(s) }

  def p_default: Parser[JavaToken] = "default" ^^ { s => KW_Default(s) }

  def p_do: Parser[JavaToken] = "do" ^^ { s => KW_Do(s) }

  def p_double: Parser[JavaToken] = "double" ^^ { s => KW_Double(s) }

  def p_else: Parser[JavaToken] = "else" ^^ { s => KW_Else(s) }

  def p_enum: Parser[JavaToken] = "enum" ^^ { s => KW_Enum(s) }

  def p_extends: Parser[JavaToken] = "extends" ^^ { s => KW_Extends(s) }

  def p_final: Parser[JavaToken] = "final" ^^ { s => KW_Final(s) }

  def p_finally: Parser[JavaToken] = "finally" ^^ { s => KW_Finally(s) }

  def p_float: Parser[JavaToken] = "float" ^^ { s => KW_Float(s) }

  def p_for: Parser[JavaToken] = "for" ^^ { s => KW_For(s) }

  def p_goto: Parser[JavaToken] = "goto" ^^ { s => KW_Goto(s) }

  def p_if: Parser[JavaToken] = "if" ^^ { s => KW_If(s) }

  def p_implements: Parser[JavaToken] = "implements" ^^ { s =>
    KW_Implements(s)
  }

  def p_import: Parser[JavaToken] = "import" ^^ { s => KW_Import(s) }

  def p_instanceof: Parser[JavaToken] = "instanceof" ^^ { s =>
    KW_Instanceof(s)
  }

  def p_int: Parser[JavaToken] = "int" ^^ { s => KW_Int(s) }

  def p_interface: Parser[JavaToken] = "interface" ^^ { s => KW_Interface(s) }

  def p_long: Parser[JavaToken] = "long" ^^ { s => KW_Long(s) }

  def p_native: Parser[JavaToken] = "native" ^^ { s => KW_Native(s) }

  def p_new: Parser[JavaToken] = "new" ^^ { s => KW_New(s) }

  def p_package: Parser[JavaToken] = "package" ^^ { s => KW_Package(s) }

  def p_private: Parser[JavaToken] = "private" ^^ { s => KW_Private(s) }

  def p_protected: Parser[JavaToken] = "proected" ^^ { s => KW_Protected(s) }

  def p_public: Parser[JavaToken] = "public" ^^ { s => KW_Public(s) }

  def p_return: Parser[JavaToken] = "return" ^^ { s => KW_Return(s) }

  def p_short: Parser[JavaToken] = "short" ^^ { s => KW_Short(s) }

  def p_static: Parser[JavaToken] = "static" ^^ { s => KW_Static(s) }

  def p_strictfp: Parser[JavaToken] = "strictfp" ^^ { s => KW_Strictfp(s) }

  def p_super: Parser[JavaToken] = "super" ^^ { s => KW_Super(s) }

  def p_switch: Parser[JavaToken] = "switch" ^^ { s => KW_Switch(s) }

  def p_synchronized: Parser[JavaToken] = "synchronized" ^^ { s =>
    KW_Synchronized(s)
  }

  def p_this: Parser[JavaToken] = "this" ^^ { s => KW_This(s) }

  def p_throw: Parser[JavaToken] = "throw" ^^ { s => KW_Throw(s) }

  def p_throws: Parser[JavaToken] = "throws" ^^ { s => KW_Throws(s) }

  def p_transient: Parser[JavaToken] = "transient" ^^ { s => KW_Transient(s) }

  def p_try: Parser[JavaToken] = "try" ^^ { s => KW_Try(s) }

  def p_void: Parser[JavaToken] = "void" ^^ { s => KW_Void(s) }

  def p_volatile: Parser[JavaToken] = "volatile" ^^ { s => KW_Volatile(s) }

  def p_while: Parser[JavaToken] = "while" ^^ { s => KW_While(s) }

  def p_IntTok: Parser[JavaToken] = {
    s"${nonzero}${javaDigit}*".r ^^ { s =>
      IntTok(s, s.toInt)
    } |
      s"0[xX]${hexdig}+".r ^^ { s => IntTok(s, java.lang.Integer.decode(s)) } |
      s"0${javaDigit}+".r ^^ { s => IntTok(s, s.toInt) } |
      "0" ^^ { s => IntTok(s, 0) }
  }

  def p_LongTok: Parser[JavaToken] = {
    s"${nonzero}${javaDigit}*[lL]".r ^^ { s =>
      LongTok(s, java.lang.Long.decode(s.take(s.length() - 1)))
    } |
      s"0[xX]${hexdig}+[lL]".r ^^ { s =>
        LongTok(s, java.lang.Long.decode(s.take(s.length() - 1)))
      } |
      s"0${javaDigit}+[lL]".r ^^ { s =>
        LongTok(s, java.lang.Long.decode(s.take(s.length() - 1)))
      } |
      "0[lL]".r ^^ { s => LongTok(s, 0) }
  }

  def p_DoubleTok: Parser[JavaToken] = {
    s"${javaDigit}+[.]${javaDigit}+${exponent}?[dD]?".r ^^ { s =>
      DoubleTok(s, s.toDouble)
    } |
      s"[.]${javaDigit}+${exponent}?[dD]?".r ^^ { s => DoubleTok(s, s.toDouble) } |
      s"${javaDigit}+${exponent}".r ^^ { s => DoubleTok(s, s.toDouble) } |
      s"${javaDigit}+${exponent}?[dD]?".r ^^ { s => DoubleTok(s, s.toDouble) } |
      s"0[xX]${hexdig}*[.]?${hexdig}*${pexponent}[dD]?".r ^^ { s =>
        DoubleTok(s, s.toDouble)
      }
  }

  def p_FloatTok: Parser[JavaToken] = {
    s"${javaDigit}+[.]${javaDigit}+${exponent}?[fF]?".r ^^ { s =>
      FloatTok(s, s.toFloat)
    } |
      s"[.]${javaDigit}+${exponent}?[fF]?".r ^^ { s => FloatTok(s, s.toFloat) } |
      s"${javaDigit}+${exponent}?[fF]?".r ^^ { s => FloatTok(s, s.toFloat) } |
      s"0[xX]${hexdig}*[.]?${hexdig}*${pexponent}[fF]?".r ^^ { s =>
        FloatTok(s, s.toFloat)
      }
  }

  def p_BoolTok: Parser[JavaToken] = {
    "true" ^^ { s =>
      BoolTok(s, true)
    } |
      "false" ^^ { s => BoolTok(s, false) }
  }

  def readCharTok(s: String): Char = {
    convChar(dropQuotes(s)).head
  }

  def readStringTok(s: String): String = {
    convChar(dropQuotes(s))
  }

  def dropQuotes(s: String): String = {
    s.tail.take(s.length() - 2)
  }

  val atof: Set[Char] = (('a' to 'f') ++ ('A' to 'F')).toSet

  val octs: Set[Char] = ('0' to '7').toSet

  def isHexDigit(c: Char): Boolean = ((c.isDigit) || (atof(c)))

  def isOctDigit(c: Char): Boolean = octs(c)

  //def toEnum(s: String): Char = unescapeJava(s).charAt(0)
  def toEnum(s: String): Char = StringContext.processEscapes(s).charAt(0)

  def convChar(s: String): String = convChar1(s.toList).mkString

  def readOct(cs: List[Char]): Int = cs match { // only consider 3 digits max
    case d1 :: d2 :: d3 :: _ => (d1 - '0') * 8 * 8 + (d2 - '0') * 8 + (d3 - '0')
    case d1 :: d2 :: Nil     => (d1 - '0') * 8 + (d2 - '0')
    case d1 :: Nil           => d1 - '0'
    case Nil =>
      lexicalError(
        "readOct is applied to an empty list of character, this should not happen."
      )
  }
  
  def lexicalError(mesg: String): Nothing = {
    sys.error(s"lexical error: ${mesg}")
  }

  def convChar1(cs: List[Char]): List[Char] = cs match {
    case '\\' :: 'u' :: d1 :: d2 :: d3 :: d4 :: rest
        if (List(d1, d2, d3, d4).forall(isHexDigit(_))) => {
      val c = toEnum(List('\\', 'u', d1, d2, d3, d4).mkString)
      val cs2 = convChar1(rest)
      c :: cs2
    }
    case '\\' :: 'u' :: rest =>
      lexicalError(s"""bad unicode escape \"\\u${rest.take(4)}\"""")
    case '\\' :: c :: rest => {
      if (isOctDigit(c)) {
        val maxRemainingOctals = if (c <= '3') { 2 }
        else { 1 }
        def convOctal(n: Int): List[Char] = {
          val octals = rest.take(n).takeWhile(isOctDigit)
          val noctals = octals.length
          def toChar(cs: List[Char]): Char = readOct(cs).toChar
          (toChar((c :: octals))) :: (convChar1(rest.drop(noctals)))
        }
        convOctal(maxRemainingOctals)
      } else {
        val hd = c match {
          case 'b'  => '\b'
          case 'f'  => '\f'
          case 'n'  => '\n'
          case 'r'  => '\r'
          case 't'  => '\t'
          case '\'' => '\''
          case '\\' => '\\'
          case '"'  => '"'
          case _    => lexicalError(s"""bad escape \"\\" ${c}"\" """)
        }
        hd :: convChar1(rest)
      }
    }
    case '\\' :: List() => lexicalError(s"""bad escape \"\\\" """)
    case (x :: s)       => x :: convChar1(s)
    case List()         => List()
  }

  def p_CharTok: Parser[JavaToken] = {
    s"'(${charEscape}|[^\\\'])'".r ^^ { s => CharTok(s, readCharTok(s)) }
    // s"'[^\\\']'".r ^^ { s => CharTok( readCharTok(s)) }
  }

  def p_StringTok: Parser[JavaToken] = {
    s"""\"(${charEscape}|[^\\\'])*\"""".r ^^ { s =>
      StringTok(s, readStringTok(s))
    }
  }

  def p_NullTok: Parser[JavaToken] = "null" ^^ { s => NullTok(s) }

  /*
  def p_IdentTok: Parser[JavaToken] = 
    s"${javaLetter}${javaLetterOrDigit}*".r ^^ { s => IdentTok(s) }
  */
  def p_JavaLetterOrDigitTok: Parser[JavaToken] = 
    s"${javaLetter}${javaLetterOrDigit}*".r ^^ { s => s match { 
      case "abstract" => KW_Abstract(s)
      case "assert" => KW_Assert(s)
      case "boolean" => KW_Boolean(s)
      case "break" => KW_Break(s)
      case "byte" => KW_Byte(s)
      case "case" => KW_Case(s)
      case "catch" => KW_Catch(s)
      case "char" => KW_Char(s)
      case "class" => KW_Class(s)
      case "const" => KW_Const(s)
      case "continue" => KW_Continue(s)
      case "default" => KW_Default(s)
      case "do" => KW_Do(s)
      case "double" => KW_Double(s)
      case "else" => KW_Else(s)
      case "enum" => KW_Enum(s)
      case "extends" => KW_Extends(s)
      case "final" => KW_Final(s)
      case "finally" => KW_Finally(s)
      case "float" => KW_Float(s)
      case "for" => KW_For(s)
      case "goto" => KW_Goto(s)
      case "if" => KW_If(s)
      case "implements" => KW_Implements(s)
      case "import" => KW_Import(s)
      case "instanceof" => KW_Instanceof(s)
      case "int" => KW_Int(s)
      case "interface" => KW_Interface(s)
      case "long" => KW_Long(s)
      case "native" => KW_Native(s)
      case "new" => KW_New(s)
      case "package" => KW_Package(s)
      case "private" => KW_Private(s)
      case "protected" => KW_Protected(s)
      case "public" => KW_Public(s)
      case "return" => KW_Return(s)
      case "short" => KW_Short(s)
      case "static" => KW_Static(s)
      case "strictfp" => KW_Strictfp(s)
      case "super" => KW_Super(s)
      case "switch" => KW_Switch(s)
      case "synchronized" => KW_Synchronized(s)
      case "this" => KW_This(s)
      case "throw" => KW_Throw(s)
      case "throws" => KW_Throws(s)
      case "transient" => KW_Transient(s)
      case "try" => KW_Try(s)
      case "void" => KW_Void(s)
      case "volatile" => KW_Volatile(s)
      case "while" => KW_While(s)
      case "true" => BoolTok(s, true)
      case "false" => BoolTok(s, false)
      case "null" => NullTok(s)
      case _ => IdentTok(s) }
    }

  def p_OpenParen: Parser[JavaToken] = "(" ^^ { s => OpenParen(s) }
  def p_CloseParen: Parser[JavaToken] = ")" ^^ { s => CloseParen(s) }
  def p_OpenSquare: Parser[JavaToken] = "[" ^^ { s => OpenSquare(s) }
  def p_CloseSquare: Parser[JavaToken] = "]" ^^ { s => CloseSquare(s) }
  def p_OpenCurly: Parser[JavaToken] = "{" ^^ { s => OpenCurly(s) }
  def p_CloseCurly: Parser[JavaToken] = "}" ^^ { s => CloseCurly(s) }
  def p_SemiColon: Parser[JavaToken] = ";" ^^ { s => SemiColon(s) }
  def p_Comma: Parser[JavaToken] = "," ^^ { s => Comma(s) }
  def p_Period: Parser[JavaToken] = "." ^^ { s => Period(s) }
  def p_LambdaArrow: Parser[JavaToken] = "->" ^^ { s => LambdaArrow(s) }
  def p_MethodRefSep: Parser[JavaToken] = "::" ^^ { s => MethodRefSep(s) }

  def p_Op_Equal: Parser[JavaToken] = "=" ^^ { s => Op_Equal(s) }
  def p_Op_GThan: Parser[JavaToken] = ">" ^^ { s => Op_GThan(s) }
  def p_Op_LThan: Parser[JavaToken] = "<" ^^ { s => Op_LThan(s) }
  def p_Op_Bang: Parser[JavaToken] = "!" ^^ { s => Op_Bang(s) }
  def p_Op_Tilde: Parser[JavaToken] = "~" ^^ { s => Op_Tilde(s) }
  def p_Op_Query: Parser[JavaToken] = "?" ^^ { s => Op_Query(s) }
  def p_Op_Colon: Parser[JavaToken] = ":" ^^ { s => Op_Colon(s) }
  def p_Op_Equals: Parser[JavaToken] = "==" ^^ { s => Op_Equals(s) }
  def p_Op_LThanE: Parser[JavaToken] = "<=" ^^ { s => Op_LThanE(s) }
  def p_Op_GThanE: Parser[JavaToken] = ">=" ^^ { s => Op_GThanE(s) }
  def p_Op_BangE: Parser[JavaToken] = "!=" ^^ { s => Op_BangE(s) }
  def p_Op_AAnd: Parser[JavaToken] = "&&" ^^ { s => Op_AAnd(s) }
  def p_Op_OOr: Parser[JavaToken] = "||" ^^ { s => Op_OOr(s) }
  def p_Op_PPlus: Parser[JavaToken] = "++" ^^ { s => Op_PPlus(s) }
  def p_Op_MMinus: Parser[JavaToken] = "--" ^^ { s => Op_MMinus(s) }
  def p_Op_Plus: Parser[JavaToken] = "+" ^^ { s => Op_Plus(s) }
  def p_Op_Minus: Parser[JavaToken] = "-" ^^ { s => Op_Minus(s) }
  def p_Op_Star: Parser[JavaToken] = "*" ^^ { s => Op_Star(s) }
  def p_Op_Slash: Parser[JavaToken] = "/" ^^ { s => Op_Slash(s) }
  def p_Op_And: Parser[JavaToken] = "&" ^^ { s => Op_And(s) }
  def p_Op_Or: Parser[JavaToken] = "|" ^^ { s => Op_Or(s) }
  def p_Op_Caret: Parser[JavaToken] = "^" ^^ { s => Op_Caret(s) }
  def p_Op_Percent: Parser[JavaToken] = "%" ^^ { s => Op_Percent(s) }
  def p_Op_LShift: Parser[JavaToken] = "<<" ^^ { s => Op_LShift(s) }
  def p_Op_PlusE: Parser[JavaToken] = "+=" ^^ { s => Op_PlusE(s) }
  def p_Op_MinusE: Parser[JavaToken] = "-=" ^^ { s => Op_MinusE(s) }
  def p_Op_StarE: Parser[JavaToken] = "*=" ^^ { s => Op_StarE(s) }
  def p_Op_SlashE: Parser[JavaToken] = "/=" ^^ { s => Op_SlashE(s) }
  def p_Op_AndE: Parser[JavaToken] = "&=" ^^ { s => Op_AndE(s) }
  def p_Op_OrE: Parser[JavaToken] = "|=" ^^ { s => Op_OrE(s) }
  def p_Op_CaretE: Parser[JavaToken] = "^=" ^^ { s => Op_CaretE(s) }
  def p_Op_PercentE: Parser[JavaToken] = "%=" ^^ { s => Op_PercentE(s) }
  def p_Op_LShiftE: Parser[JavaToken] = "<<=" ^^ { s => Op_LShiftE(s) }
  def p_Op_RShiftE: Parser[JavaToken] = ">>=" ^^ { s => Op_RShiftE(s) }
  def p_Op_RRShiftE: Parser[JavaToken] = ">>>=" ^^ { s => Op_RRShiftE(s) }
  def p_Op_AtSign: Parser[JavaToken] = "@" ^^ { s => Op_AtSign(s) }

  def parse_one(p: Parser[JavaToken], src: String): ParseResult[JavaToken] =
    parse(phrase(p), src)

  override def token: Parser[JavaToken] = {
    p_ann_interface |
      p_IntTok |
      p_LongTok |
      p_DoubleTok |
      p_FloatTok |
      p_CharTok |
      p_StringTok |
      p_JavaLetterOrDigitTok |
      p_OpenParen |
      p_CloseParen |
      p_OpenSquare |
      p_CloseSquare |
      p_OpenCurly |
      p_CloseCurly |
      p_SemiColon |
      p_Comma |
      p_Period |
      p_LambdaArrow |
      p_MethodRefSep |
      p_Op_Equals |
      p_Op_LThanE |
      p_Op_GThanE |
      p_Op_BangE |
      p_Op_AAnd |
      p_Op_OOr |
      p_Op_Equal |
      p_Op_GThan |
      p_Op_LThan |
      p_Op_Bang |
      p_Op_Tilde |
      p_Op_Query |
      p_Op_Colon |
      p_Op_PPlus |
      p_Op_MMinus |
      p_Op_PlusE |
      p_Op_MinusE |
      p_Op_StarE |
      p_Op_SlashE |
      p_Op_AndE |
      p_Op_OrE |
      p_Op_CaretE |
      p_Op_PercentE |
      p_Op_LShiftE |
      p_Op_RShiftE |
      p_Op_RRShiftE |
      p_Op_Plus |
      p_Op_Minus |
      p_Op_Star |
      p_Op_Slash |
      p_Op_And |
      p_Op_Or |
      p_Op_Caret |
      p_Op_Percent |
      p_Op_LShift |
      p_Op_AtSign
  }
  def tokens: Parser[List[JavaToken]] = phrase(
    rep1(token)
  )

  def tokenize(src: String): ParseResult[List[JavaToken]] =
    parse(tokens, src)

}
