package obsidian.lang.java.scalangj

import scala.util.parsing.combinator.token.*

trait JavaTokens extends Tokens {

  type JavaToken = Token

  case class KW_AnnInterface(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Abstract(chars:String)extends JavaToken { override def toString = chars }
  case class KW_Assert(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Boolean(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Break(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Byte(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Case(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Catch(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Char(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Class(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Const(chars:String) extends JavaToken { override def toString = chars } 
  case class KW_Continue(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Default(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Do(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Double(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Else(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Enum(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Extends(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Final(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Finally(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Float(chars:String) extends JavaToken { override def toString = chars }
  case class KW_For(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Goto(chars:String) extends JavaToken { override def toString = chars }
  case class KW_If(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Implements(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Import(chars:String) extends JavaToken{ override def toString = chars }
  case class KW_Instanceof(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Int(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Interface(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Long(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Native(chars:String) extends JavaToken { override def toString = chars }
  case class KW_New(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Package(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Private(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Protected(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Public(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Return(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Short(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Static(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Strictfp(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Super(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Switch(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Synchronized(chars:String) extends JavaToken { override def toString = chars }
  case class KW_This(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Throw(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Throws(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Transient(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Try(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Void(chars:String) extends JavaToken { override def toString = chars }
  case class KW_Volatile(chars:String) extends JavaToken { override def toString = chars }
  case class KW_While(chars:String) extends JavaToken { override def toString = chars }
// Separators
  case class OpenParen(chars:String) extends JavaToken
  case class CloseParen(chars:String) extends JavaToken
  case class OpenSquare(chars:String) extends JavaToken
  case class CloseSquare(chars:String) extends JavaToken
  case class OpenCurly(chars:String) extends JavaToken
  case class CloseCurly(chars:String) extends JavaToken
  case class SemiColon(chars:String) extends JavaToken
  case class Comma(chars:String) extends JavaToken
  case class Period(chars:String) extends JavaToken
  case class LambdaArrow(chars:String) extends JavaToken
  case class MethodRefSep(chars:String) extends JavaToken

// Literals
  case class IntTok(chars:String, v: Int) extends JavaToken
  case class LongTok(chars:String, v: Long) extends JavaToken
  case class DoubleTok(chars:String, v: Double) extends JavaToken
  case class FloatTok(chars:String, v: Float) extends JavaToken
  case class CharTok(chars:String, v: Char) extends JavaToken  { override def toString: String = s"'${chars}'" }
  case class StringTok(chars:String, v: String) extends JavaToken { override def toString: String = s""""${chars}"""" }
  case class BoolTok(chars:String, v: Boolean) extends JavaToken
  case class NullTok(chars:String) extends JavaToken

// Identifiers
  case class IdentTok(chars: String) extends JavaToken

// Operators
  case class Op_Equal(chars:String) extends JavaToken
  case class Op_GThan(chars:String) extends JavaToken
  case class Op_LThan(chars:String) extends JavaToken
  case class Op_Bang(chars:String) extends JavaToken
  case class Op_Tilde(chars:String) extends JavaToken
  case class Op_Query(chars:String) extends JavaToken
  case class Op_Colon(chars:String) extends JavaToken
  case class Op_Equals(chars:String) extends JavaToken
  case class Op_LThanE(chars:String) extends JavaToken
  case class Op_GThanE(chars:String) extends JavaToken
  case class Op_BangE(chars:String) extends JavaToken
  case class Op_AAnd(chars:String) extends JavaToken
  case class Op_OOr(chars:String) extends JavaToken
  case class Op_PPlus(chars:String) extends JavaToken
  case class Op_MMinus(chars:String) extends JavaToken
  case class Op_Plus(chars:String) extends JavaToken
  case class Op_Minus(chars:String) extends JavaToken
  case class Op_Star(chars:String) extends JavaToken
  case class Op_Slash(chars:String) extends JavaToken
  case class Op_And(chars:String) extends JavaToken
  case class Op_Or(chars:String) extends JavaToken
  case class Op_Caret(chars:String) extends JavaToken
  case class Op_Percent(chars:String) extends JavaToken
  case class Op_LShift(chars:String) extends JavaToken
  case class Op_PlusE(chars:String) extends JavaToken
  case class Op_MinusE(chars:String) extends JavaToken
  case class Op_StarE(chars:String) extends JavaToken
  case class Op_SlashE(chars:String) extends JavaToken
  case class Op_AndE(chars:String) extends JavaToken
  case class Op_OrE(chars:String) extends JavaToken
  case class Op_CaretE(chars:String) extends JavaToken
  case class Op_PercentE(chars:String) extends JavaToken
  case class Op_LShiftE(chars:String) extends JavaToken
  case class Op_RShiftE(chars:String) extends JavaToken
  case class Op_RRShiftE(chars:String) extends JavaToken
  case class Op_AtSign(chars:String) extends JavaToken

}
