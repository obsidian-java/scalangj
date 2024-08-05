package obsidian.lang.java.scalangj

import obsidian.lang.java.scalangj.*
import obsidian.lang.java.scalangj.Parser.* // To unify the base class JavaToken, otherwise === will failed
import obsidian.lang.java.scalangj.Syntax.*
import org.scalatest.{funsuite, matchers}



class TestLitParser1 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val STRING = "1"
  val LITERAL: IntLit = IntLit(1)
  test("Literal is parsed correlectly") {
    val result = parseLiteral(STRING)
    assert((result.successful) && (result.get === LITERAL))
  }
}


class TestParser1 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val STRING = "int x = 1;"
  val ty:Type = PrimType_(IntT)
  val vardecls:List[VarDecl] = List(VarDecl(VarId(Ident("x")), Some(InitExp(Lit(IntLit(1))))))
  val LOCALVARDECL:(List[Modifier], Type, List[VarDecl]) = (Nil, ty, vardecls)
  test(s"phrase ${STRING} is parsed correctly") {
    val result = localVarDecl(new Lexer.Scanner(STRING))
    assert((result.successful) && (result.get === LOCALVARDECL))
  }
}


class TestParser2 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val STRING = """
public class HelloWorld {
    public static void main() {
        System.out.println("Hello World!");
    }
} 
  """
  val CLASSDECL: ClassTypeDecl = ClassTypeDecl(ClassDecl_(List(Public),Ident("HelloWorld"),List(),None,List()
      ,ClassBody(List(MemberDecl_(MethodDecl(List(Public, Static),List(),None,Ident("main"),List(),List(),None,MethodBody(Some(Block(List(BlockStmt_(ExpStmt(MethodInv(MethodCall(Name(List(Ident("System"), Ident("out"), Ident("println"))),List(Lit(StringLit("Hello World!")))))))))))))))))
  test(s"phrase ${STRING} is parsed correctly") {
    val result = classOrInterfaceDecl(new Lexer.Scanner(STRING))
    assert((result.successful) && (result.get === CLASSDECL))
  }
}


class TestParser3 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val STRING = """
        System.out.println("Hello World!");
  """
  val METHODINV: MethodInv = MethodInv(MethodCall(Name(List(Ident("System"), Ident("out"), Ident("println"))),List(Lit(StringLit("Hello World!")))))
  test(s"phrase ${STRING} is parsed correctly") {
    val result = methodInvocationExp(new Lexer.Scanner(STRING))
    assert((result.successful) && (result.get === METHODINV))
  }
}


class TestParser4 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val STRING = "System.out.println"
  val NAME: Name = Name(List(Ident("System"), Ident("out"), Ident("println")))
  test(s"phrase ${STRING} is parsed correctly") {
    val result = name(new Lexer.Scanner(STRING))
    assert((result.successful) && (result.get === NAME))
  }
}

class TestParser5 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val STRING = """
  public class Fib
{
    public static int fib(int n)
    {
	int f1 = 0;
	int f2 = 1;
	int i=0;
	while(i<n) {
	    int t = f2;
	    f2 = f1 + f2;
	    f1 = t;
	    i++;
	}
	return f2;
    }

    public static void  main(String argv[]) {
	System.out.println(fib(10));
    }
}

  """
  val CLASSDECL:TypeDecl = ClassTypeDecl(
    ClassDecl_(
      List(Public),Ident("Fib"),List(),None,List(),
      ClassBody(List(
      MemberDecl_(MethodDecl(List(Public, Static),List(),
        Some(PrimType_(IntT)),Ident("fib"),
          List(FormalParam(List(),PrimType_(IntT),false,VarId(Ident("n")))),
          List(),None,
          MethodBody(Some(Block(List(LocalVars(List(),PrimType_(IntT),
                                            List(VarDecl(VarId(Ident("f1")),
                                              Some(InitExp(Lit(IntLit(0))))))), 
                                  LocalVars(List(),PrimType_(IntT),
                                            List(VarDecl(VarId(Ident("f2")),
                                              Some(InitExp(Lit(IntLit(1))))))), 
                                  LocalVars(List(),PrimType_(IntT),
                                            List(VarDecl(VarId(Ident("i")),
                                              Some(InitExp(Lit(IntLit(0))))))), 
                                  BlockStmt_(While(BinOp(ExpName(Name(List(Ident("i")))),
                                                      LThan,ExpName(Name(List(Ident("n"))))),
                                                  StmtBlock(Block(List(
                                                          LocalVars(List(),
                                                              PrimType_(IntT),
                                                              List(VarDecl(VarId(Ident("t")),
                                                                  Some(InitExp(ExpName(Name(List(Ident("f2"))))))))), 
                                                          BlockStmt_(ExpStmt(Assign(NameLhs(Name(List(Ident("f2")))),
                                                                                  EqualA,
                                                                                  BinOp(ExpName(Name(List(Ident("f1")))),Add,ExpName(Name(List(Ident("f2")))))))), 
                                                          BlockStmt_(ExpStmt(Assign(NameLhs(Name(List(Ident("f1")))),
                                                                                  EqualA,
                                                                                  ExpName(Name(List(Ident("t"))))))), 
                                                          BlockStmt_(ExpStmt(PostIncrement(ExpName(Name(List(Ident("i")))))))))))), 
                                  BlockStmt_(Return(Some(ExpName(Name(List(Ident("f2"))))))))))))), 
        MemberDecl_(MethodDecl(List(Public, Static),List(),None,Ident("main"),
          List(FormalParam(List(),RefType_(ClassRefType(ClassType(List((Ident("String"),List()))))),
            false,VarDeclArray(VarId(Ident("argv"))))),List(),None,
            MethodBody(Some(Block(List(
                BlockStmt_(ExpStmt(MethodInv(MethodCall(Name(List(Ident("System"), Ident("out"), Ident("println")))
                  ,List(MethodInv(MethodCall(Name(List(Ident("fib"))),List(Lit(IntLit(10))))))))))))))))))))
  test(s"phrase ${STRING} is parsed correctly") {
    val result = classOrInterfaceDecl(new Lexer.Scanner(STRING))
    assert((result.successful) && (result.get === CLASSDECL))
  }
}




class TestParser6 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val STRING = """
public class HelloWorld {
    public static void main() {
      int x = 0;
      try { x = x / 0; }
      catch (NullPointerException e) { x = 1;}
      catch (Exception e) { x = 2;}
      finally { x = 1; }
    }
} 
  """
  val CLASSDECL: ClassTypeDecl = ClassTypeDecl(ClassDecl_(List(Public),Ident("HelloWorld"),List(),None,List(),
                    ClassBody(List(MemberDecl_(MethodDecl(List(Public, Static),List(),None,Ident("main"),List(),List(),None,
                    MethodBody(Some(Block(List(LocalVars(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("x")),Some(InitExp(Lit(IntLit(0))))))), 
                    BlockStmt_(Try(Block(List(BlockStmt_(ExpStmt(Assign(NameLhs(Name(List(Ident("x")))),EqualA,BinOp(ExpName(Name(List(Ident("x")))),
                    Div,Lit(IntLit(0)))))))),List(Catch(FormalParam(List(),RefType_(ClassRefType(ClassType(List((Ident("NullPointerException"),List()))))),
                    false,VarId(Ident("e"))),Block(List(BlockStmt_(ExpStmt(Assign(NameLhs(Name(List(Ident("x")))),EqualA,Lit(IntLit(1)))))))), 
                    Catch(FormalParam(List(),RefType_(ClassRefType(ClassType(List((Ident("Exception"),List()))))),false,VarId(Ident("e"))),
                    Block(List(BlockStmt_(ExpStmt(Assign(NameLhs(Name(List(Ident("x")))),EqualA,Lit(IntLit(2))))))))),
                    Some(Block(List(BlockStmt_(ExpStmt(Assign(NameLhs(Name(List(Ident("x")))),EqualA,Lit(IntLit(1))))))))))))))))))))

  test(s"phrase ${STRING} is parsed correctly") {
    val result = classOrInterfaceDecl(new Lexer.Scanner(STRING))
    
    assert((result.successful) && (result.get === CLASSDECL))
  }
}


class TestParser7 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val STRING = "finally { x = 1; }"
  val BLOCK: Block = Block(List(BlockStmt_(ExpStmt(Assign(NameLhs(Name(List(Ident("x")))),EqualA,Lit(IntLit(1)))))))
  test("testParser7") {
    val result = finallyClause(new Lexer.Scanner(STRING))
    assert((result.successful) && (result.get === BLOCK))
  }
}


class TestParser8 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val STRING = " x == 1;" // because this is not a valid Java statement, though it is valid in C.
  test("testParser8") {
    val result = stmtExp(new Lexer.Scanner(STRING))
    assert(!result.successful)
  }
}


class TestParser9 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val STRING = " x += 1;" 
  val STMT: Assign = Assign(NameLhs(Name(List(Ident("x")))),AddA,Lit(IntLit(1)))
  test("testParser9") {
    val result = stmtExp(new Lexer.Scanner(STRING))
    assert((result.successful) && (result.get === STMT))
  }
}



class TestParser10 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val STRING = """
public static void main(String [] args) {
	int x = 0;
	do {
    } while (true);
}
  """
  val DECL: Some[MemberDecl_] = Some(MemberDecl_(MethodDecl(List(Public, Static),List(),None,Ident("main"),List(FormalParam(List(),RefType_(ArrayType(RefType_(ClassRefType(ClassType(List((Ident("String"),List()))))))),false,VarId(Ident("args")))),List(),None,MethodBody(Some(Block(List(LocalVars(List(),PrimType_(IntT),List(VarDecl(VarId(Ident("x")),Some(InitExp(Lit(IntLit(0))))))), BlockStmt_(Do(StmtBlock(Block(List())),Lit(BooleanLit(true)))))))))))
  test("testParser10") {
    val result = classBodyStatement(new Lexer.Scanner(STRING))
    // println(result.get)
    assert((result.successful) && (result.get === DECL))
  }
}

class TestParser11 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val STRING = """
        case 1: 
  """
  val SWITCHCASE: SwitchCase = SwitchCase(Lit(IntLit(1)))
  test("testParser11") {
    val result = switchLabel(new Lexer.Scanner(STRING))
    // println(result.get)
    assert((result.successful) && (result.get === SWITCHCASE))
  }
}


class TestParser12 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val STRING = """
public static boolean add(int v) {
    int [] vals=null; 
    int i=0; 
    boolean res=false; 
    try {
      if (this.cap < 1){throw new Exception();}
      else {
        if (this.size < this.cap) {
          this.vals[this.size] = v; this.size = this.size + 1;
        } else {
          vals = new int[this.cap];
          i = 0;
          while (i < this.cap-1) {
            vals[i] = this.vals[i+1];
            i = i + 1;
          }
          vals[this.cap-1] = v;
          this.vals = vals;
        }
      }
      res = true;
    } catch (Exception e) {
      println("Failed with zero capacity.");
    }
    return res;
}  """
  test("testParser12") {
    classBodyStatement(new Lexer.Scanner(STRING)) match {
      case Error(msg, next) => fail(msg)
      case Failure(msg, next) => fail(msg)
      case Success(result, next) => {
        assert(true)
      }
    }
  }
}

class TestParser13 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val STRING = "int [] new_vals=null;" 
  val ty:Type = RefType_(ArrayType(PrimType_(IntT)))
  val vardecls:List[VarDecl] = List(VarDecl(VarId(Ident("new_vals")), Some(InitExp(Lit(NullLit)))))
  val LOCALVARDECL:(List[Modifier], Type, List[VarDecl]) = (Nil, ty, vardecls)
  test(s"phrase ${STRING} is parsed correctly") {
    val result = localVarDecl(new Lexer.Scanner(STRING))
    result match {
      case Error(msg, next) => fail(msg)
      case Failure(msg, next) => fail(msg)
      case Success(dec, next) => assert((result.successful) && (result.get === LOCALVARDECL))
    }
    
  }
}

class TestParser14 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val STRING = "int []"
  test(s"phrase ${STRING} is parsed correctly") {
    val result = refType(new Lexer.Scanner(STRING)) 
    result match {
      case Error(msg, next) => fail(msg)
      case Failure(msg, next) => fail(msg)
      case Success(dec, next) => assert(result.successful) // && (result.get === LOCALVARDECL))    
    }
  }
}

class TestParser15 extends funsuite.AnyFunSuite with matchers.should.Matchers {
  val STRING = "boolean x = true;"
  test(s"phrase ${STRING} is parsed correctly") {
    val result = blockStmt(new Lexer.Scanner(STRING)) 
    result match {
      case Error(msg, next) => fail(msg)
      case Failure(msg, next) => fail(msg)
      case Success(dec, next) => assert(result.successful) // && (result.get === LOCALVARDECL))    
    }
  }
}