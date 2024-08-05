package obsidian.lang.java.scalangj

import obsidian.lang.java.scalangj.Parser.*
import obsidian.lang.java.scalangj.Pretty.*


object Main extends App {
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
    val eCU = parseCompilationUnit(STRING)
    eCU match {
        case Left(error_msg) => println(error_msg)
        case Right(cu) => println(prettyPrint(cu)) 
    }
}
