package obsidian.lang.java.scalangj


import obsidian.lang.java.scalangj.*
import obsidian.lang.java.scalangj.Parser.* // To unify the base class JavaToken, otherwise === will failed
import obsidian.lang.java.scalangj.Syntax.*
import obsidian.lang.java.scalangj.Pretty.*
import obsidian.lang.java.scalangj.Pretty.ops.*
import org.scalatest.{funsuite, matchers}
import scala.util.parsing.input.*
import java.io.*
import org.scalatest.CompleteLastly


class TestPretty1 extends funsuite.AnyFunSuite with matchers.should.Matchers {
    val STRING = """
public class HelloWorld {
    public static void main() {
        char c = 'a';
        System.out.println("Hello World!");
    }
} 
"""
    val tyDec:TypeDecl = classOrInterfaceDecl(new Lexer.Scanner(STRING)).get

    test(s"phrase ${STRING} is parsed correctly") {
        val o = prettyPrint(tyDec) 
        println(o)
        assert(o != null)
    }
}

class TestPretty2 extends funsuite.AnyFunSuite with matchers.should.Matchers {
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
    val tyDec:TypeDecl = classOrInterfaceDecl(new Lexer.Scanner(STRING)).get
    test(s"phrase ${STRING} is parsed correctly") {
        val o = prettyPrint(tyDec) 
        println(o)
        assert(o != null)
    }
}

class TestPretty3 extends funsuite.AnyFunSuite with matchers.should.Matchers {
    val filename = "./testinput/Fib.java"
    val file = new File(filename);
    val reader = new FileReader(file);
    val cu:CompilationUnit = compilationUnit(new Lexer.Scanner(StreamReader(reader))).get
    test(s"file ${filename} is parsed correctly") {
        val o = prettyPrint(cu) 
        println(o)
        assert(o != null)
    }
}