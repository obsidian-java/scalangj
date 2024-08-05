package obsidian.lang.java.scalangj

import obsidian.lang.java.scalangj.Lexer.*
import obsidian.lang.java.scalangj.Syntax.*
import obsidian.lang.java.scalangj.*
import scala.util.parsing.combinator.*
import scala.util.parsing.combinator.syntactical.*
import scala.util.parsing.input.*
import scala.language.implicitConversions


object Parser extends Parsers {
    override type Elem = JavaToken

    type ErrorMessage = String
    
    /**
    * The main function. Parsing a compilation unit. 
    * @param s
    *  
    */
    def parseCompilationUnit(s:String): Either[ErrorMessage, CompilationUnit] = {
        compilationUnit(new Lexer.Scanner(s)) match {
            case Success(cu, _) => Right(cu)
            case Error(msg, next) => Left(msg)
            case Failure(msg, next) => Left(msg)
        }
    }

    /**
    * The main function. Parsing a compilation unit. 
    * @param file
    *  
    */
    def parseCompilationUnit(file:java.io.File): Either[ErrorMessage, CompilationUnit] = {
        val reader = new java.io.FileReader(file)
        compilationUnit(new Lexer.Scanner(StreamReader(reader))) match {
            case Success(cu, rest) if rest.atEnd => Right(cu)
            //case Success(cu, rest) if !rest.atEnd => Left("Unexpected input at the of the input file.")
            case Success(cu, rest) => Left("Unexpected input at the of the input file.")
            case Error(msg, next) => Left(msg)
            case Failure(msg, next) => Left(msg)
        }
    }


    def compilationUnit: Parser[CompilationUnit] = {
        opt(packageDecl) ~ rep(importDecl) ~ rep(typeDecl) ^^ { 
            case mpd ~ ids ~ tds  => CompilationUnit(mpd, ids, tds.flatMap(x => x)) 
        }
    }
    /*
    def compilationUnit: Parser[CompilationUnit] = {
        opt(packageDecl) ~ rep(importDecl) ~ rep(typeDecl) ~ EOF ^^ { 
            case mpd ~ ids ~ tds ~ _ => CompilationUnit(mpd, ids, tds.flatMap(x => x)) 
        }
    }
    */
    
    def packageDecl:Parser[PackageDecl] = { 
        tok(KW_Package("package")) ~ name ~ semiColon ^^  {
            case pkg ~ n ~ _ => PackageDecl(n)
        }    
     }
    
    def importDecl:Parser[ImportDecl] = {
        tok(KW_Package("import")) ~ bopt(tok(KW_Static("static"))) ~ name ~ bopt(period ~> tok(Op_Star("*"))) ~ semiColon ^^ {
            case imp ~ st ~ n ~ ds ~ semiCol => ImportDecl(st,n,ds)
        }
    }

    def typeDecl:Parser[Option[TypeDecl]] = { 
        def someDecls:Parser[Option[TypeDecl]] = {
            classOrInterfaceDecl ^^ { decl => Some(decl) }
        }

        def noDecl:Parser[Option[TypeDecl]] = {
            semiColon ^^ { _ => None }
        }
        someDecls | noDecl
    }

    //----------------------------------------------------------------------------
    // -- Declarations
    // -- Class declarations
    def classOrInterfaceDecl:Parser[TypeDecl] = {
        rep(modifier) ~ (classDecl_Internal | interfaceDecl_Internal) ^^ {
            case (md ~ de) => de(md)
        }
    }

    def classDecl_Internal:Parser[Mod[TypeDecl]] = {
        classDecl ^^ { (cd:Mod[ClassDecl]) => ((ms:List[Modifier]) => ClassTypeDecl(cd(ms))) }        
    }

    def interfaceDecl_Internal:Parser[Mod[TypeDecl]] = {
        (annInterfaceDecl | interfaceDecl) ^^  { (id:Mod[InterfaceDecl]) => ((ms:List[Modifier]) => InterfaceTypeDecl(id(ms)))}
    }

    type Mod[A] = List[Modifier] => A

    def classDecl:Parser[Mod[ClassDecl]] = normalClassDecl | enumClassDecl

    def normalClassDecl:Parser[Mod[ClassDecl]] = {
        tok(KW_Class("class")) ~ ident ~ lopt(typeParams) ~ opt(exts) ~ lopt(impls) ~ classBody ^^ {
            case (_ ~ i ~ tps ~ mex ~ imp ~ bod)  => ((ms:List[Modifier]) => ClassDecl_(ms, i, tps, mex.map(_.head), imp, bod))
        }
    }

    def exts:Parser[List[RefType]] = { 
        tok(KW_Extends("extends")) ~> refTypeList ^^ { rts => rts }
    }


    def impls:Parser[List[RefType]] = { 
        tok(KW_Implements("implements")) ~> refTypeList ^^ { rts => rts }
    }

    def enumClassDecl:Parser[Mod[ClassDecl]] = {
        tok(KW_Enum("enum")) ~ ident ~ lopt(impls) ~ enumBody ^^ {
            case (_ ~ i ~ imp ~ bod) => {
                ms => EnumDecl(ms, i, imp, bod)
            }
        }
    }


    def classBody:Parser[ClassBody] = {
        braces(classBodyStatements) ^^ { b => ClassBody(b)}
    }

    def enumBody:Parser[EnumBody] = {
        def inner:Parser[EnumBody] = {
            seplist(enumConst, comma) ~ opt(comma) ~ lopt(enumBodyDecls) ^^ {
                case ecs ~ _ ~ eds => EnumBody(ecs,eds)
            }
        }
        braces(inner)
    } 

    def enumConst:Parser[EnumConstant] = {
        ident ~ lopt(args) ~ opt(classBody) ^^ { 
            case (id ~ ars ~ mcb) => {
                EnumConstant(id, ars, mcb)
            }
        }
    }

    def enumBodyDecls:Parser[List[Decl]] = semiColon ~> classBodyStatements

    def classBodyStatements:Parser[List[Decl]] = {
        list(classBodyStatement) ^^ { mcbs => mcbs.flatMap(x => x) }
    }


    def annInterfaceDecl:Parser[Mod[InterfaceDecl]] = {
        tok(KW_AnnInterface("@interface")) ~ ident ~ lopt(typeParams) ~ lopt(exts) ~ interfaceBody ^^ {
            case (_ ~ id ~ tps ~ exs ~ bod) => { ms => InterfaceDecl(InterfaceAnnotation, ms, id, tps, exs, bod)}
        }
    }

    def interfaceDecl:Parser[Mod[InterfaceDecl]] = {
        tok(KW_Interface("interface")) ~ ident ~ lopt(typeParams) ~ lopt(exts) ~ interfaceBody ^^ {
            case (_ ~ id ~ tps ~ exs ~ bod) => { ms => InterfaceDecl(InterfaceNormal, ms, id, tps, exs, bod)}
        }
    }

    def interfaceBody:Parser[InterfaceBody] = {
        braces(list(interfaceBodyDecl)) ^^ { decls => InterfaceBody(decls.flatMap(x => x))}
    }

    // Declarations 

    def classBodyStatement:Parser[Option[Decl]] = {
        def pNothing:Parser[Option[Decl]] = {
            list1(semiColon) ^^ { x => None }
        }
        def pInit:Parser[Option[Decl]] = {
            bopt(tok(KW_Static("static"))) ~ block ^^ {
                case ( mst ~ blk) => Some(InitDecl(mst,blk))
            }
        }
        def pMember:Parser[Option[Decl]] = {
            list(modifier) ~ memberDecl ^^ {
                case ( ms ~ dec ) => Some(MemberDecl_(dec(ms)))
            }
        }
        pNothing | pInit | pMember
    }

    def memberDecl:Parser[Mod[MemberDecl]] = {
        def mClassDecl:Parser[Mod[MemberDecl]] = {
            classDecl ^^ { cd => ms => MemberClassDecl(cd(ms))}
        }

        def mInterfaceDecl:Parser[Mod[MemberDecl]] = {
            (annInterfaceDecl | interfaceDecl) ^^ { id => ms => MemberInterfaceDecl(id(ms))} 
        }

        mClassDecl | mInterfaceDecl | fieldDecl | methodDecl | constrDecl
    }

    def fieldDecl:Parser[Mod[MemberDecl]] = {
        def p = {
            ttype ~ varDecls ^^ {
                case typ ~ vds => { (ms: List[Modifier]) => FieldDecl(ms,typ,vds)}
            }
        }
        endSemi(p)
    }

    def methodDecl:Parser[Mod[MemberDecl]] = {
        lopt(typeParams) ~ resultType ~ ident ~ formalParams ~ lopt(throws) ~ methodBody ^^ {
            case (tps ~ rt ~ id ~ fps ~ thr ~ bod ) => {
                ms => MethodDecl(ms,tps,rt,id,fps,thr,None,bod)
            }
        }
    }

    def methodBody:Parser[MethodBody] = {
        def pNothing = {
            semiColon ^^ { _ => None }
        }
        def pSome = {
            block ^^ { b => Some(b) }
        }
        (pNothing | pSome) ^^ {
            mb => MethodBody(mb)
        }
    }

    def constrDecl:Parser[Mod[MemberDecl]] = {
        lopt(typeParams) ~ ident ~ formalParams ~ lopt(throws) ~ constrBody ^^ {
            case (tps ~ id ~ fps ~ thr ~ bod) => {
                ms => ConstructorDecl(ms,tps,id,fps,thr,bod)
            }
        }
    }

    def constrBody:Parser[ConstructorBody] = {
        def p = {
            opt(explConstrInv) ~ list(blockStmt) ^^ {
                case mec ~ bss => { ConstructorBody(mec, bss) }
            }
        }
        braces(p)
    }

    def explConstrInv:Parser[ExplConstrInv] = {
        def p1 = {
            lopt(refTypeArgs) ~ tok(KW_This("this")) ~ args ^^ {
                case tas ~ _ ~ arguments => ThisInvoke(tas,arguments)
            }
        }
        def p2 = {
            lopt(refTypeArgs) ~ tok(KW_Super("super")) ~ args ^^ {
                case tas ~ _ ~ arguments => SuperInvoke(tas,arguments)
            }
        }
        def p3 = {
            primary ~ period ~ lopt(refTypeArgs) ~ tok(KW_Super("super")) ~ args ^^ {
                case pri ~ _ ~ tas ~ _ ~ arguments => PrimarySuperInvoke(pri, tas, arguments) 
            }
        }
        endSemi[ExplConstrInv](p1|p2|p3)
    }

    //  TODO: This should be parsed like class bodies, and post-checked.
    //       That would give far better error messages.
    def interfaceBodyDecl:Parser[Option[MemberDecl]] = {
        def pNone = semiColon ^^ { _ => None }
        def pSome = list(modifier) ~ interfaceMemberDecl ^^ { case ms ~ imd => Some(imd(ms)) }
        pNone | pSome
    }

    def interfaceMemberDecl:Parser[Mod[MemberDecl]] = {
        def p1 = classDecl ^^ { cd => { (ms:List[Modifier]) => MemberClassDecl(cd(ms))}} 
        def p2 = (annInterfaceDecl | interfaceDecl) ^^ { id => { (ms:List[Modifier]) => MemberInterfaceDecl(id(ms))}} 
        p1 | p2 | fieldDecl | absMethodDecl 
    }

    def absMethodDecl:Parser[Mod[MemberDecl]] = {
        lopt(typeParams) ~ resultType ~ ident ~ formalParams ~ lopt(throws) ~ opt(defaultValue) ~ semiColon ^^ {
            case tps ~ rt ~ id ~ fps ~ thr ~ defv ~ _ => { ms => MethodDecl(ms,tps,rt,id,fps,thr,defv, (MethodBody(None)))}
        }
    }

    def defaultValue:Parser[Exp] = tok(KW_Default("default")) ~> exp


    // Formal parameters

    def formalParams:Parser[List[FormalParam]] = {
        def validateFPs(fps:List[FormalParam]):Boolean = fps match 
        {
            case Nil => true 
            case (_::Nil) => true 
            case (FormalParam(_,_,b,_)::xs) => !b
        }
        def p:Parser[List[FormalParam]] = {
            seplist(formalParam,comma) >> { 
                fps => if (validateFPs(fps)) {
                    success(fps)
                } else {
                    failure("Only the last formal parameter may be of variable arity")
                }
            }
        }
        parens(p)
    }

    def formalParam:Parser[FormalParam] = {
        list(modifier) ~ ttype ~ bopt(ellipsis) ~ varDeclId ^^ {
            case ms ~ typ ~ vr ~ vid => FormalParam(ms, typ, vr, vid)
        }
    }

    def ellipsis:Parser[Unit] = {
        period ~> period ~> period ^^ { _ => () }
    }

    def modifier:Parser[Modifier] = {
        def p_public = tok(KW_Public("public")) ^^ { _ => Public }
        def p_protected = tok(KW_Protected("protected")) ^^ { _ => Protected }
        def p_private = tok(KW_Private("private")) ^^ { _ => Private } 
        def p_abstract = tok(KW_Abstract("abstract")) ^^ { _ => Abstract }
        def p_static = tok(KW_Static("static")) ^^ { _ => Static }
        def p_strictfp = tok(KW_Strictfp("strictfp")) ^^ { _ => StrictFP }
        def p_final = tok(KW_Final("final")) ^^ { _ => Final } 
        def p_native = tok(KW_Native("native")) ^^ { _ => Native }
        def p_transient = tok(KW_Transient("transient")) ^^ { _ => Transient }
        def p_volatile = tok(KW_Volatile("volatile")) ^^ { _ => Volatile }
        def p_synchronized = tok(KW_Synchronized("synchronized")) ^^ { _ => Synchronized }
        def p_annotation = annotation ^^ { ann => Annotation_(ann) }

        p_public | p_protected | p_private | p_abstract | p_static | p_strictfp | p_final | p_final | p_native | p_transient | p_volatile | p_synchronized | p_annotation
    }

    def annotation:Parser[Annotation] = {
        def normAnn = {
            parens(evlist) ^^ { ps => { (n: Name) => NormalAnnotation(n,ps)}}
        }
        def singleElemAnn = {
            parens(elementValue) ^^ { p => { (n: Name) => SingleElementAnnotation(n,p)} }
        } 
        def markerAnn = {
            success(()) ^^ { _ => { (n: Name) => MarkerAnnotation(n)}}
        }

        tok(Op_AtSign("@")) ~ name ~ (normAnn|singleElemAnn|markerAnn) ^^ { 
            case _ ~ n ~ c => {c(n)}
        }
    }

    def evlist:Parser[List[(Ident,ElementValue)]] = {
        seplist1(elementValuePair, comma)
    }

    def elementValuePair:Parser[(Ident, ElementValue)] = {
        ident ~ tok(Op_Equal("=")) ~ elementValue ^^ {
            case (id ~ _ ~ ev) => (id,ev)
        }
    }

    def elementValue:Parser[ElementValue] = {
        def arrInit = { arrayInit ^^ { ai => InitArray(ai)}}
        def expInit = { condExp ^^ { e => InitExp(e)}}
        def evval = (arrInit | expInit) ^^ ( vi => EVVal(vi)) 
        def evann = annotation ^^ { ann => EVAnn(ann) }
        evval | evann
    }

    def throws:Parser[List[RefType]] = {
        tok(KW_Throws("throws")) ~> refTypeList
    }

    
    // ------------------------------------------------------------------
    // Variable declarations

    def varDecls:Parser[List[VarDecl]] = seplist1(varDecl,comma)

    def varDecl:Parser[VarDecl] = {
        varDeclId ~ opt(tok(Op_Equal("=")) ~> varInit) ^^ {
            case vid ~ mvi => VarDecl(vid,mvi)
        }
    }

    def varDeclId:Parser[VarDeclId] = {
        ident ~ list(arrBrackets) ^^ {
            case id ~ abs => {
                val vid:VarDeclId = VarId(id)
                abs.foldLeft(vid)((f:VarDeclId,_) => VarDeclArray(f))
            }
        }
    }

    def arrBrackets:Parser[Unit] = brackets(success(()))

    def localVarDecl:Parser[(List[Modifier], Type, List[VarDecl])] = {
        list(modifier) ~ ttype ~ varDecls ^^ { 
            case ms ~ typ ~ vds => (ms, typ, vds)
        }
    }

    def varInit:Parser[VarInit] = {
        def arrInit = arrayInit ^^ { ai => InitArray(ai) }
        def expInit = exp ^^ { e => InitExp(e) }
        arrInit | expInit
    }

    def arrayInit:Parser[ArrayInit] = {
        def p:Parser[ArrayInit] = {
            // have to commit! otherwise there will be infinite recursion
            commit(seplist(varInit,comma) ~ opt(comma)) ^^ { case (vis ~ o) => ArrayInit(vis) }
            
        }
        braces(p)
    }

    // --------------------------------------------------------------------------------
    // Statements

    def block:Parser[Block] = {
        def blk = list(blockStmt) ^^ { bs => Block(bs) }
        braces(blk)
    }

    def blockStmt:Parser[BlockStmt] = {
        def localClass:Parser[BlockStmt] = {
            list(modifier) ~ classDecl ^^ { case ms ~ cd => LocalClass(cd(ms)) }
        }
        def localVars:Parser[BlockStmt] = {
            endSemi(localVarDecl) ^^ { case (m,t,vds) => LocalVars(m,t,vds) }
        }
        def pStmt:Parser[BlockStmt] = stmt ^^ { s => BlockStmt_(s) }
        localClass | localVars | pStmt
    }

    def stmt:Parser[Stmt] = {
        def thEl = {
            stmtNSI ~ tok(KW_Else("else")) ~ stmt ^^ {
                case th ~ _ ~ el => { (e: Exp) => IfThenElse(e,th,el)}
            } 
        }
        def thN = {
            stmt ^^ { 
                case th => { (e: Exp) => IfThen(e,th )}
            }
        }
        def ifStmt = {
            tok(KW_If("if")) ~ parens(exp) ~ (thEl | thN) ^^ {
                case _ ~ e ~ th => {th(e)}
            }
        }

        def whileStmt = {
            tok(KW_While("while")) ~ parens(exp) ~ stmt ^^ {
                case _ ~ e ~ s => {While(e,s)}
            }
        }

        def basicFor = {
            opt(forInit) ~ semiColon ~ opt(exp) ~ semiColon ~ opt(forUp) ^^ {
                case fi ~ _ ~ e ~ _ ~ fu => { (s: Stmt) =>  BasicFor(fi,e,fu,s) }
            }
        }

        def enhancedFor = {
            list(modifier) ~ ttype ~ ident ~ colon ~ exp ^^ {
                case ms ~ t ~ i ~ _ ~ e => { (s: Stmt) => EnhancedFor(ms,t,i,e,s)}
            }
        }

        def forStmt = {
            tok(KW_For("for")) ~ parens(basicFor|enhancedFor) ~ stmt ^^ {
                case _ ~ f ~ s => f(s)
            }
        }

        def labeledStmt = {
            ident ~ colon ~ stmt ^^ { case lbl ~ _ ~ s => Labeled(lbl,s) }
        }

        ifStmt | whileStmt | forStmt | labeledStmt | stmtNoTrail 
    }

    def stmtNSI:Parser[Stmt] = {
        def ifStmt = {
            tok(KW_If("if")) ~ parens(exp) ~ stmtNSI ~ tok(KW_Else("else")) ~ stmtNSI  ^^ {
                case _ ~ e ~ th ~ _ ~ el => {IfThenElse(e,th,el)}
            }
        }
        def whileStmt = {
            tok(KW_While("while")) ~ parens(exp) ~ stmtNSI ^^ {
                case _ ~ e ~ s => {While(e,s)}
            }
        }

        def basicFor = {
            opt(forInit) ~ semiColon ~ opt(exp) ~ semiColon ~ opt(forUp) ^^ {
                case fi ~ _ ~ e ~ _ ~ fu => { (s: Stmt) =>  BasicFor(fi,e,fu,s) }
            }
        }

        def enhancedFor = {
            list(modifier) ~ ttype ~ ident ~ colon ~ exp ^^ {
                case ms ~ t ~ i ~ _ ~ e => { (s: Stmt) => EnhancedFor(ms,t,i,e,s)}
            }
        }

        def forStmt = {
            tok(KW_For("for")) ~ parens(basicFor|enhancedFor) ~ stmtNSI ^^ {
                case _ ~ f ~ s => f(s)
            }
        }

        def labeledStmt = {
            ident ~ colon ~ stmtNSI ^^ { case lbl ~ _ ~ s => Labeled(lbl,s) }
        }
        ifStmt | whileStmt | forStmt | labeledStmt | stmtNoTrail 
    }
    def stmtNoTrail:Parser[Stmt] = {
        def empty = semiColon ^^ { _ => Empty }
        def stmtBlock = block ^^ { b => StmtBlock(b) }
        def assertStmt = endSemi({
            tok(KW_Assert("assert")) ~ exp ~ opt(colon ~> exp) ^^ {
                case _ ~ e ~ me2 => Assert(e,me2)
            }
        })
        def switchStmt = {
            tok(KW_Switch("switch")) ~ parens(exp) ~ switchBlock ^^ {
                case _ ~ e ~ sb => { Switch(e,sb) }
            }
        }
        
        def doWhile = endSemi({
            tok(KW_Do("do")) ~ stmt ~ tok(KW_While("while")) ~ parens(exp) ^^ {
                case _ ~ s ~ _ ~ e => { Do(s,e) }
            }
        })

        def breakStmt = endSemi({
            tok(KW_Break("break")) ~ opt(ident) ^^ {
                case _ ~ mi => Break(mi)
            }
        })
        def continueStmt = endSemi({
            tok(KW_Continue("continue")) ~ opt(ident) ^^ {
                case _ ~ mi => Continue(mi)
            }
        })
        def returnStmt = endSemi({
            tok(KW_Return("return")) ~ opt(exp) ^^ {
                case _ ~ me => Return(me) 
            }
        })
        def synchronizedStmt = {
            tok(KW_Synchronized("synchronized")) ~ parens(exp) ~ block ^^ {
                case _ ~ e ~ b => Synchronized(e,b) 
            }
        }
        def throwStmt = endSemi({
            tok(KW_Throw("throw")) ~ exp ^^ {
                case _ ~ e => Throw(e)
            }
        })

        
        def tryCatch = {
            tok(KW_Try("try")) ~ block ~ list(catchClause) ~ opt(finallyClause) ^^ {
                case _ ~ b ~ c~ mf => Try(b,c,mf)
            }  
        }
        /*
        def tryCatch = {
            tok(KW_Try("try")) ~ block ~ list(catchClause) ~ opt(finallyClause) ^^ {
                case _ ~ b ~ c ~ mf => Try(b,c,mf)
            }  
        }
        */

        def expStmt = endSemi({
            stmtExp ^^ { e => ExpStmt(e) }
        })
        empty | stmtBlock | assertStmt | switchStmt | doWhile | breakStmt | continueStmt | returnStmt | synchronizedStmt | throwStmt | tryCatch | expStmt
    }
    def forInit:Parser[ForInit] = {
        def forLocalVars = {
            localVarDecl ^^ { 
                case (m,t,vds) => ForLocalVars(m,t,vds)
            }
        }
        def forInitExps = {
            seplist1(stmtExp,comma) ^^ { case es => ForInitExps(es) }
        }
        forLocalVars | forInitExps
    }
    def forUp:Parser[List[Exp]] = seplist1(stmtExp,comma)

    def switchBlock:Parser[List[SwitchBlock]] = braces(list(switchStmt))

    def switchStmt:Parser[SwitchBlock] = {
        switchLabel ~ list(blockStmt) ^^ {
            case lbl ~ bss => SwitchBlock(lbl, bss)
        }
    }

    def switchLabel:Parser[SwitchLabel] = {
        def pDefault = {
            tok(KW_Default("default")) ~> colon ^^^ { Default }
        }
        def pCase = {
            tok(KW_Case("case")) ~> exp <~ colon ^^ { e => SwitchCase(e) }
        }
        /*
        def pCase = {
            tok(KW_Case("case")) ~ exp ~ colon ^^ { 
                case _ ~ e ~ _ => SwitchCase(e)
            }
        }*/
        pDefault | pCase 
    }

    // --------------------------------------------------------------------------------
    // Try-catch clauses
    
    def catchClause:Parser[Catch] = {
        tok(KW_Catch("catch")) ~ parens(formalParam) ~ block ^^ {
            case _ ~ fp ~ b => { Catch(fp, b) }
        }
    }

    def finallyClause:Parser[Block] = {
        tok(KW_Finally("finally")) ~ block ^^ { 
            case _ ~ b => b
        }
    }
    // --------------------------------------------------------------------------------
    // Expressions

    def stmtExp:Parser[Exp] = preIncDec | postIncDec | assignment | methodInvocationExp | lambdaExp | methodRef | instanceCreation 

    def preIncDec:Parser[Exp] = {
        preIncDecOp ~ unaryExp ^^ {
            case op ~ e => op(e)
        }
    }

    def postIncDec:Parser[Exp] = {
        postfixExpNES ~ list1(postfixOp) ^^ {
            case e ~ ops => ops.foldLeft(e)((a,s) => s(a))
        }
    }

    def assignment:Parser[Exp] = {
        lhs ~ assignOp ~ assignExp ^^ { 
            case lh ~ op ~ e => Assign(lh,op,e)
        }
    }

    def lhs:Parser[Lhs] = {
        def pFieldLhs = fieldAccess ^^ (x => FieldLhs(x))
        def pArrayLhs = arrayAccess ^^ (x => ArrayLhs(x))
        def pNameLhs = name ^^ (x => NameLhs(x))
        pFieldLhs | pArrayLhs | pNameLhs
    }

    def exp:Parser[Exp] = assignExp 


    // note scala parsec | is default backtracking operator, with explicit commit
    def assignExp:Parser[Exp] = methodRef | lambdaExp | assignment | condExp

    def condExp:Parser[Exp] = {
        infixExp ~ list(condExpSuffix) ^^ {
            case ie ~ ces => ces.foldLeft(ie)((a,s) => s(a))
        }
    }

    def condExpSuffix:Parser[Exp=>Exp] = {
        tok(Op_Query("?")) ~ exp  ~ colon  ~ condExp ^^ {
            case _ ~ th ~ _ ~ el => { ce => Cond(ce,th,el) }
        }
    }

    def infixExp:Parser[Exp] = {
        unaryExp ~ list(infixExpSuffix) ^^ {
            case ue ~ ies => ies.foldLeft(ue)((a,s) => s(a))
        }
    }

    def infixExpSuffix:Parser[Exp => Exp] = {
        def pBinOp1 = {
            infixCombineOp ~ infixExp ^^ {
                case op ~ ie2 => { (ie1: Exp) => BinOp(ie1,op,ie2)}
            }
        }
        def pBinOp2 = {
            infixOp ~ unaryExp ^^ {
                case op ~ e2 => { (e1: Exp) => BinOp(e1,op,e2)}
            }
        }
        def pInstanceOf = {
            tok(KW_Instanceof("instanceof")) ~ refType ^^ {
                case _ ~ t => { (e1: Exp) => InstanceOf(e1,t)}
            }
        }
        pBinOp1 | pBinOp2 | pInstanceOf
    }

    def unaryExp:Parser[Exp] = {
        def pPrefixOp = prefixOp ~ unaryExp ^^ { case op ~ ue => op(ue) }
        def pCast = parens(ttype) ~ unaryExp ^^ { case t ~ e => Cast(t,e) }
        preIncDec | pPrefixOp | pCast | postfixExp
    }

    def postfixExpNES:Parser[Exp] = // postIncDec | 
        primary | { name ^^ (x => ExpName(x)) }

    def postfixExp:Parser[Exp] = {
        postfixExpNES ~ list(postfixOp) ^^ { 
            case pe ~ ops => ops.foldLeft(pe)((a,s) => s(a))
        }
    }
    def primary:Parser[Exp] = startSuff(primaryNPS, primarySuffix)

    def primaryNPS:Parser[Exp] = arrayCreation | primaryNoNewArrayNPS

    def primaryNoNewArray: Parser[Exp] = startSuff(primaryNoNewArrayNPS, primarySuffix)

    def primaryNoNewArrayNPS:Parser[Exp] = {
        def pLit = literal ^^ (x => Lit(x))
        def pThis = tok(KW_This("this")) ^^^ This
        def pClassLit = resultType ~ period ~ tok(KW_Class("class")) ^^ {
            case rt ~ _ ~ _ => ClassLit(rt)
        }
        def pThisClass = name ~ period ~ tok(KW_This("this")) ^^ {
            case n ~ _ ~ _ => ThisClass(n)
        }
        pLit | pThis | parens(exp) | pClassLit | pThisClass | instanceCreationNPS | (methodInvocationNPS ^^ (x => MethodInv(x))) | (fieldAccessNPS ^^ (x => FieldAccess_(x))) | (arrayAccessNPS ^^ (x => ArrayAccess(x)))
    }

    def primarySuffix:Parser[Exp => Exp] = {
        def pArrayAccess = { arrayAccessSuffix ^^ {f => { (e:Exp) => ArrayAccess(f(e))}} }
        def pMethodInv = { methodInvocationSuffix ^^ { f => { (e:Exp) => MethodInv(f(e))}} }
        def pFieldAcccess = { fieldAccessSuffix ^^ { f => { (e:Exp) => FieldAccess_(f(e))}}}
        instanceCreationSuffix | pArrayAccess | pMethodInv | pFieldAcccess
    }

    def instanceCreationNPS:Parser[Exp] = {
        tok(KW_New("new")) ~ lopt(typeArgs) ~ typeDeclSpecifier ~ args ~ opt(classBody) ^^ {
            case _ ~ tas ~ tds ~ ars ~ mcb => {InstanceCreation(tas,tds,ars, mcb)}
        }
    }

    def typeDeclSpecifier: Parser[TypeDeclSpecifier] = {
        def pWithDiamond = {
            classType ~ period ~ ident ~ tok(Op_LThan("<")) ~ tok(Op_GThan(">")) ^^ {
                case ct ~ _ ~ i ~ _ ~ _  => {TypeDeclSpecifierWithDiamond(ct, i, Diamond())}
            }
        }
        def pWithUnQualDiamond = {
            ident ~ tok(Op_LThan("<")) ~ tok(Op_GThan(">")) ^^ {
                case i ~ _ ~ _ => {TypeDeclSpecifierUnqualifiedWithDiamond(i, Diamond())}
            }
        }
        pWithDiamond | pWithUnQualDiamond | { classType ^^ (x => TypeDeclSpecifier_(x)) }
    }

    def instanceCreationSuffix:Parser[Exp => Exp] = {
        period ~ tok(KW_New("new")) ~ lopt(typeArgs) ~ ident ~ args ~ opt(classBody) ^^ {
            case _ ~ _ ~ tas ~ i ~ ars ~ mcb => { (p:Exp) => QualInstanceCreation(p,tas,i,ars,mcb) }
        }
    }

    def instanceCreation:Parser[Exp] = {
        def pQualInstance = {
            primaryNPS ~ list(primarySuffix) >> {
                case p ~ ss => {
                    val icp = ss.foldLeft(p)((a,s) => s(a))
                    icp match {
                        case QualInstanceCreation(_,_,_,_,_) => success(icp)
                        case _ => failure("Failed when parsing instanceCreation")
                    }
                }
            }
        }
        instanceCreationNPS | pQualInstance
    }

    def lambdaParams :Parser[LambdaParams] = {
        def pSingleParam = ident ^^ (x => LambdaSingleParam(x))
        def pFormalParams = seplist(formalParam,comma) ^^ (x => LambdaFormalParams(x))
        def pInferredParams = seplist(ident,comma) ^^ (x => LambdaInferredParams(x))
        pSingleParam | parens(pFormalParams) | parens(pInferredParams)
    }

    def lambdaExp:Parser[Exp] = {
        def pLambdaBody = (block ^^ (x => LambdaBlock(x))) | (exp ^^ (x => LambdaExpression_(x))) 
        lambdaParams ~ tok(LambdaArrow("->")) ~ pLambdaBody ^^ {
            case ps ~ _ ~ b => Lambda(ps,b)
        }
    }


    def methodRef:Parser[Exp] = {
        name ~ tok(MethodRefSep("::")) ~ ident ^^ {
            case n ~ _ ~ i => MethodRef(n,i)
        }
    }

    def fieldAccessNPS:Parser[FieldAccess] = {
        def pSuperFieldAcccess = {
            tok(KW_Super("super")) ~ period ~ ident ^^ {
                case _ ~ _ ~ i => SuperFieldAccess(i)
            }
        }
        def pClassfieldAccess = {
            name ~ period ~ tok(KW_Super("super")) ~ period ~ ident ^^ {
                case n ~ _ ~ _ ~ _ ~ i => ClassFieldAccess(n,i)
            }
        }
        pSuperFieldAcccess | pClassfieldAccess
    }

    def fieldAccessSuffix:Parser[Exp => FieldAccess] = {
        period ~ ident ^^ { 
            case _ ~ i  => { p => PrimaryFieldAccess(p,i) }
        }
    }


    def fieldAccess:Parser[FieldAccess] = {
        def pFieldAccess = {
            primaryNPS ~ list(primarySuffix) >> {
                case p ~ ss => {
                    val fap = ss.foldLeft(p)((a,s) => s(a))
                    fap match {
                        case FieldAccess_(fa) => success(fa)
                        case _ => failure("Failed when passing FieldAccess")
                    }
                }
            }
        }
        fieldAccessNPS | pFieldAccess
    }

    def maybe[A,B](default:B, f:A => B, mb:Option[A]):B = mb match {
        case None => default
        case Some(a) => f(a)
    }

    def const[A,B](a:A, b:B):A = a
    def methodInvocationNPS:Parser[MethodInvocation] = {
        def pSuperMethCall = {
            tok(KW_Super("super")) ~ period ~ lopt(refTypeArgs) ~ ident ~ args ^^ {
                case _ ~ _~ rts ~ i ~ ars => SuperMethodCall(rts, i, ars) 
            }
        }
        def pMethCall = {
            args ^^ { ars => { (n: Name) => MethodCall(n,ars)}}
        }
        def pTypeMethCall = {
            period ~ opt(tok(KW_Super("super"))) ~ period ~ lopt(refTypeArgs) ~ ident ~ args ^^ {
                case _ ~ msp ~ _ ~ rts ~ i ~ ars => {
                    val mc = maybe(TypeMethodCall(_,_,_,_), (x:JavaToken) => const(ClassMethodCall(_,_,_,_),x),msp)
                    (n:Name) => mc(n,rts,i,ars)
                }
            }
        }
        pSuperMethCall | (( name ~ (pMethCall|pTypeMethCall) ^^ {
            case n ~ f => f(n)
        }))
    }

    def methodInvocationSuffix:Parser[Exp => MethodInvocation] = {
        period ~ lopt(refTypeArgs) ~ ident ~ args ^^ {
            case _ ~ rts ~ i ~ ars => { p => PrimaryMethodCall(p, Nil,i, ars)}
        }
    }

    def methodInvocationExp:Parser[Exp] = {
        def pMethodInv = {
            primaryNPS ~ list(primarySuffix) >> {
                case p ~ ss => {
                    val mip = ss.foldLeft(p)((a,s)=>s(a))
                    mip match {
                        case MethodInv(_) => success(mip)
                        case _ => failure("Failed when parsing methodInvocationExp")
                    }
                }
            }
        }
        pMethodInv | { methodInvocationNPS ^^ { x => MethodInv(x) } }
    }


    def args:Parser[List[Argument]] = parens(seplist(exp,comma))

    // --------------------------------------------------------------------------------
    // Arrays

    def arrayAccessNPS:Parser[ArrayIndex] = {
        name ~ list1((brackets(exp))) ^^ {
            case n ~ e => ArrayIndex(ExpName(n), e)
        }
    }

    def arrayAccessSuffix:Parser[Exp => ArrayIndex] = {
        list1(brackets(exp)) ^^ {
            e => { ref => ArrayIndex(ref,e)}
        }
    }


    def arrayAccess:Parser[ArrayIndex] = {
        def p = {
            primaryNoNewArrayNPS ~ list(primarySuffix) >> {
                case p ~ ss => {
                    val aap = ss.foldLeft(p)((a,s) => s(a)) 
                    aap match {
                        case ArrayAccess(ain) => success(ain)
                        case _ => failure("Failed when parsing arrayAccess")
                    }
                }
            }
        }
        arrayAccessNPS | p
    }

    def arrayCreation:Parser[Exp] = {
        def pArrayCreateInit = {
            list1(brackets(empty)) ~ arrayInit ^^ {
                case ds ~ ai => { (t: Type) => ArrayCreateInit(t,ds.length,ai)}
            }
        }
        def pArrayCreate = {
            list1(brackets(exp)) ~ list(brackets(empty)) ^^ {
                case des ~ ds => { (t: Type) => ArrayCreate(t,des,ds.length)}
            }
        }
        tok(KW_New("new")) ~ nonArrayType ~ (pArrayCreateInit|pArrayCreate) ^^ {
            case _ ~ t ~ f => f(t)
        }
    }

    def literal: Parser[Literal] = 
    accept(
        "literal", {
            case IntTok(s, i)    => IntLit(i)
            case LongTok(s, i)   => LongLit(i)
            case DoubleTok(s, d) => DoubleLit(d)
            case FloatTok(s, f)  => FloatLit(f)
            case CharTok(s, c)   => CharLit(c)
            case StringTok(s, v) => StringLit(v)
            case BoolTok(s, b)   => BooleanLit(b)
            case NullTok(s)      => NullLit
        }
    )
    /*
    def parseLiteral(s: String): Literal = {
        lit(new Lexer.Scanner(s)) match {
            case Success(result, _) => result
        }
    }
    */
    def parseLiteral(s: String): ParseResult[Literal] = {
        literal(new Lexer.Scanner(s)) 
    }

 
    // ------------------------------------------------------------------
    // Operators

    def preIncDecOp:Parser[Exp => Exp] = {
        def pp = tok(Op_PPlus("++")) ^^^ { (e: Exp) => PreIncrement(e) }
        def mm = tok(Op_MMinus("--")) ^^^ { (e: Exp) => PreDecrement(e) }
        pp | mm
    }
    def prefixOp:Parser[Exp => Exp] = {
        def pn = tok(Op_Bang("!")) ^^^ { (e: Exp) => PreNot(e) }
        def pbc = tok(Op_Tilde("~")) ^^^ { (e: Exp) => PreBitCompl(e) } 
        def pp = tok(Op_Plus("+")) ^^^ { (e: Exp) => PrePlus(e) }
        def pm = tok(Op_Minus("-")) ^^^ { (e: Exp) => PreMinus(e) }
        pn | pbc | pp | pm
    }
    def postfixOp:Parser[Exp => Exp] = {
        def pp = tok(Op_PPlus("++")) ^^^ { (e: Exp) => PostIncrement(e) }
        def mm = tok(Op_MMinus("--")) ^^^ { (e: Exp) => PostDecrement(e) }
        pp | mm 
    }


    def assignOp:Parser[AssignOp] = {
        def ea = tok(Op_Equal("=")) ^^^ { EqualA }
        def ma = tok(Op_StarE("*=")) ^^^ { MultA }
        def da = tok(Op_SlashE("/=")) ^^^ { DivA }
        def ra = tok(Op_PercentE("%=")) ^^^ { RemA }
        def aa = tok(Op_PlusE("+=")) ^^^ { AddA }
        def sa = tok(Op_MinusE("-=")) ^^^ { SubA }
        def lsa = tok(Op_LShiftE("<<=")) ^^^ { LShiftA }
        def rsa = tok(Op_RShiftE(">>=")) ^^^ { RShiftA }
        def rrsa = tok(Op_RRShiftE(">>>=")) ^^^ { RRShiftA }
        def ae = tok(Op_AndE("&=")) ^^^ { AndA } 
        def ce = tok(Op_CaretE("^=")) ^^^ { XorA }
        def oe = tok(Op_OrE("|=")) ^^^ { OrA }
        ea | ma | da | ra | aa | sa | lsa | rsa | rrsa | ae | ce | oe 
    }

    def infixCombineOp:Parser[Op] = {
        def and = tok(Op_And("&")) ^^^ { And } 
        def caret = tok(Op_Caret("^")) ^^^ { Xor } 
        def or = tok(Op_Or("|")) ^^^ { Or }
        def aand = tok(Op_AAnd("&&")) ^^^ { CAnd }
        def oor = tok(Op_OOr("||")) ^^^ { COr }
        and | caret | or | aand | oor 
    }

    def infixOp:Parser[Op] = 
        tok(Op_Star("*")) ^^^ { Mult } |
        tok(Op_Slash("/")) ^^^ { Div } | 
        tok(Op_Percent("%")) ^^^ { Rem } |
        tok(Op_Plus("+")) ^^^ { Add } |
        tok(Op_Minus("-")) ^^^ { Sub } | 
        tok(Op_LShift("<<")) ^^^ { LShift } |
        tok(Op_LThan("<")) ^^^ { LThan } |
        tok(Op_GThan(">")) ~> tok(Op_GThan(">")) ~> tok(Op_GThan(">")) ^^^ { RRShift } |
        tok(Op_GThan(">")) ~> tok(Op_GThan(">")) ^^^ { RShift } |
        tok(Op_GThan(">")) ^^^ { GThan } | 
        tok(Op_LThanE("<=")) ^^^ { LThanE } |
        tok(Op_GThanE(">=")) ^^^ { GThanE } |
        tok(Op_Equals("==")) ^^^ { Equal } | 
        tok(Op_BangE("!=")) ^^^ { NotEq }
        

    // ------------------------------------------------------------------
    // Types

    def ttype:Parser[Type] = {
        def rType:Parser[Type] = refType ^^ {t => RefType_(t)}
        def pType:Parser[Type] = primType ^^ {t => PrimType_(t)}
        rType | pType
    }

    def primType:Parser[PrimType] = {
        def boolType:Parser[PrimType] = tok(KW_Boolean("boolean")) ^^ { _ => BooleanT }
        def byteType:Parser[PrimType] = tok(KW_Byte("byte")) ^^ { _ => ByteT }
        def shortType:Parser[PrimType] = tok(KW_Short("short")) ^^ { _ => ShortT }
        def intType:Parser[PrimType] = tok(KW_Int("int")) ^^ { _ => IntT }
        def longType:Parser[PrimType] = tok(KW_Long("long")) ^^ { _ => LongT }
        def charType:Parser[PrimType] = tok(KW_Char("char")) ^^ { _ => CharT }
        def floatType:Parser[PrimType] = tok(KW_Float("float")) ^^ { _ => FloatT }
        def doubleType:Parser[PrimType] = tok(KW_Double("double")) ^^ { _ => DoubleT }
        boolType | byteType | shortType | intType | longType | charType | floatType | doubleType 
    }

    def refType:Parser[RefType] = {
        def primArrType:Parser[RefType] = {
            primType ~ list1(arrBrackets) ^^ {
                case (pt ~ (_::bs)) =>
                {
                    val base:RefType = ArrayType(PrimType_(pt))
                    bs.foldLeft(base)((f,_) => ArrayType(RefType_(f)))
                    /* 
                    // this works
                    val arrTyConstr:(PrimType => RefType) = 
                        bs.foldLeft((t:PrimType)=>ArrayType(PrimType_(t)))((f,_) => t => ArrayType(RefType_(f(t))))
                    arrTyConstr(pt)
                    */
                }
                case (pt ~ Nil) => sys.error("refType:error, empty list returned from list1()") 

            }
        }

        def clType:Parser[RefType] = {
            classType ~ list(arrBrackets) ^^ {
                case (ct ~ bs) => {
                    val base:RefType = ClassRefType(ct)
                    bs.foldLeft(base)((f,_) => ArrayType(RefType_(f)))
                    /*
                    // this does not type check in scala
                    val arrTyConstr:(ClassType => RefType) = 
                        bs.foldLeft((t:ClassType)=>ClassRefType(t))((f,_) => t => ArrayType(RefType_(f(t))))
                    arrTyConstr(ct)
                    */
                }
            }
        }
        (primArrType | clType).withErrorMessage("refType")
    }   

    def nonArrayType:Parser[Type] = {
        primType ^^ { t => PrimType_(t) } | 
        classType ^^ { t => RefType_(ClassRefType(t)) }
    }

    def classType:Parser[ClassType] = seplist1(classTypeSpec,period) ^^ {
        ct => ClassType(ct)
    }

    def classTypeSpec:Parser[(Ident, List[TypeArgument])] = {
        ident ~ lopt(typeArgs) ^^ { 
            case i ~ tas  => (i, tas)
        }
    }

    def resultType:Parser[Option[Type]] = {
        def pNone:Parser[Option[Type]] = {
            tok(KW_Void("void")) ^^ { _ => None } 
        }
        def pSome:Parser[Option[Type]] = {
            ttype ^^ {
                t => Some(t)
            }
        }
        (pNone | pSome).withErrorMessage("resultType") 
    }

    def refTypeList:Parser[List[RefType]] = seplist1(refType,comma)

   

    // --------------------------------------------------------------------------------
    // Type parameters and arguments

    def typeParams:Parser[List[TypeParam]] = angles(seplist1(typeParam,comma))

    def typeParam:Parser[TypeParam] = {
        ident ~ lopt(bounds) ^^ { case i ~ bs => TypeParam(i,bs) }
    }

    def bounds:Parser[List[RefType]] = tok(KW_Extends("extends")) ~> seplist1(refType, (tok(Op_And("&"))))

    def typeArgs:Parser[List[TypeArgument]] = angles(seplist1(typeArg,comma))


    def typeArg:Parser[TypeArgument] = {
        def wcTyArg:Parser[TypeArgument] = {
            tok(Op_Query("?")) ~> opt(wildcardBound) ^^ {
                owcb => Wildcard(owcb)
            }
        }
        def actTyArg:Parser[TypeArgument] = {
            refType ^^ {
                rt => ActualType(rt)
            }
        }
        wcTyArg | actTyArg
    }

    def wildcardBound:Parser[WildcardBound] = {
        def extendsBound:Parser[WildcardBound] = {
            tok(KW_Extends("extends")) ~> refType ^^ {
                rt => ExtendsBound(rt)
            }
        }

        def superBound:Parser[WildcardBound] = {
            tok(KW_Super("super")) ~> refType ^^ {
                rt => SuperBound(rt)
            }
        }
        extendsBound | superBound
    }

    def refTypeArgs:Parser[List[RefType]] = angles(refTypeList)





    // ------------------------------------------------------------------
    // Names

    def name:Parser[Name] = {
        seplist1(ident,period) ^^ { ids => Name(ids) }
    }

    def ident:Parser[Ident] = {
        accept("identifier", {
            case IdentTok(s) => Ident(s)
        })
    }



    // ------------------------------------------------------------------


    def comma:Parser[JavaToken] = tok(Comma(","))
    def period:Parser[JavaToken] = tok(Period("."))
    def semiColon:Parser[JavaToken] = tok(SemiColon(";"))
    def colon:Parser[JavaToken] = tok(Op_Colon(":"))

    def tok(e:Elem):Parser[Elem] = elem(e)

    def empty:Parser[Unit] = success(())

    def bopt[A](p:Parser[A]):Parser[Boolean] = {
        opt(p) ^^ { mb => mb match {
            case Some(a) => true
            case None => false
        }}
    }

    def lopt[A](p:Parser[List[A]]):Parser[List[A]] = {
        opt(p) ^^ { mas => mas match {
            case None => List()
            case Some(as) => as
        }}
    }

    def list[A](p:Parser[A]):Parser[List[A]] = {
        val e:List[A] = Nil
        option(e,list1(p))
    }

    def list1[A](p:Parser[A]):Parser[List[A]] = rep1(p)

    def seplist[A,SEP](p:Parser[A], sep:Parser[SEP]):Parser[List[A]] = {
        val e:List[A] = Nil
        option(e, seplist1(p,sep))
    }
    def seplist1[A,SEP](p:Parser[A], sep:Parser[SEP]):Parser[List[A]] = {
        p >> { 
            a => 
                { sep ~ seplist(p, sep) ^^ { case (_ ~ aas) => a::aas } } | success(List(a)) 
            }
    }


    def option[A](a:A,p:Parser[A]):Parser[A] = p | success(a)


    def between[O,A,C](open:Parser[O], close:Parser[C], p:Parser[A]):Parser[A] = {
        open ~ p ~ close ^^ { case o ~ a ~ c => a }
    }

    def parens[A](p:Parser[A]):Parser[A] = between(tok(OpenParen("(")),tok(CloseParen(")")), p)

    def braces[A](p:Parser[A]):Parser[A] = between(tok(OpenCurly("{")),tok(CloseCurly("}")), p)

    def brackets[A](p:Parser[A]):Parser[A] = between(tok(OpenSquare("[")),tok(CloseSquare("]")), p)

    def angles[A](p:Parser[A]):Parser[A] = between(tok(Op_LThan("<")),tok(Op_GThan(">")), p)

    def endSemi[A](p:Parser[A]):Parser[A] = p <~ semiColon

    def startSuff[A](start:Parser[A], suffix:Parser[A=>A]):Parser[A] = {
        start ~ list(suffix) ^^ { 
            case x ~ ss => ss.foldLeft(x)((a,s) => s(a))
        }
    }
}