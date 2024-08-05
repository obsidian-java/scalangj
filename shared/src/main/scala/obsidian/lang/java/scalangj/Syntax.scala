package obsidian.lang.java.scalangj


object Syntax {

  // Packages
  /**
    * A compilation unit is the top level syntactic goal symbol of a Java program.
    *
    * @param pkg_decl the optional package declaration
    * @param imp_decls the list of import declarations
    * @param type_decls the list of type declarations
    */
  case class CompilationUnit(
      pkg_decl: Option[PackageDecl],
      imp_decls: List[ImportDecl],
      type_decls: List[TypeDecl]
  )

  /**
    * A package declaration appears within a compilation unit to indicate the package to which the compilation unit belongs.
    *
    * @param name the name of the package
    */
  case class PackageDecl(name: Name)

  /**
    * An import declaration allows a static member or a named type to be referred to by a single unqualified identifier.
    * @param is_static signals whether the declaration only imports static members.
    * @param name name of the imported module
    * @param all_in_scope signals whether the declaration brings all names in the named type or package, or only brings a single name into scope.
    */
  case class ImportDecl(is_static: Boolean, name: Name, all_in_scope: Boolean)

  // Declarations
  /**
    * A type declaration declares a class type or an interface type.
    */
  sealed trait TypeDecl
  case class ClassTypeDecl(class_decl: ClassDecl) extends TypeDecl
  case class InterfaceTypeDecl(iface_decl: InterfaceDecl) extends TypeDecl

  /**
    * A class declaration specifies a new named reference type.
    */
  sealed trait ClassDecl { def setMods(ms:List[Modifier]):ClassDecl }
  case class ClassDecl_(
      modifiers: List[Modifier],
      id: Ident,
      type_params: List[TypeParam],
      ref_type: Option[RefType],
      ref_types: List[RefType],
      body: ClassBody
  ) extends ClassDecl {
    def setMods(ms:List[Modifier]): ClassDecl_ = this.copy(modifiers = ms)
  }
  case class EnumDecl(
      modifiers: List[Modifier],
      id: Ident,
      ref_types: List[RefType],
      body: EnumBody
  ) extends ClassDecl { 
    def setMods(ms:List[Modifier]): EnumDecl = this.copy(modifiers = ms)
  }

  /**
    * A class body may contain declarations of members of the class, that is,
    * fields, classes, interfaces and methods.
    * A class body may also contain instance initializers, static
    * initializers, and declarations of constructors for the class.
    * @param decls declarations in the class
    */
  case class ClassBody(decls: List[Decl])

  /**
    * he body of an enum type may contain enum constants.
    *
    * @param constants constants in the enum
    * @param decls declarations in the enum
    */
  case class EnumBody(constants: List[EnumConstant], decls: List[Decl])

  /**
    * An enum constant defines an instance of the enum type
    *
    * @param id
    * @param args
    * @param body
    */
  case class EnumConstant(
      id: Ident,
      args: List[Argument],
      body: Option[ClassBody]
  )

  /**
    * An interface declaration introduces a new reference type whose members
    * are classes, interfaces, constants and abstract methods. This type has
    * no implementation, but otherwise unrelated classes can implement it by
    * providing implementations for its abstract methods.
    * @param kind
    * @param modifiers
    * @param id
    * @param type_params
    * @param ref_types
    * @param body
    */
  case class InterfaceDecl(
      kind: InterfaceKind,
      modifiers: List[Modifier],
      id: Ident,
      type_params: List[TypeParam],
      ref_types: List[RefType],
      body: InterfaceBody
  ) {
    def setMods(ms:List[Modifier]): InterfaceDecl = {
      this.copy(modifiers = ms)
    }
  }

  /**
    * Interface can declare either a normal interface or an annotation
    */
  sealed trait InterfaceKind
  case object InterfaceNormal extends InterfaceKind
  case object InterfaceAnnotation extends InterfaceKind

  /**
    * The body of an interface may declare members of the interface.
    *
    * @param member_decls
    */
  case class InterfaceBody(member_decls: List[MemberDecl])

  /**
    * A declaration is either a member declaration, or a declaration of an
    * initializer, which may be static.
    */
  sealed trait Decl
  case class MemberDecl_(member: MemberDecl) extends Decl
  case class InitDecl(is_static: Boolean, blk: Block) extends Decl

  /**
    * A class or interface member can be an inner class or interface, a field or
    * constant, or a method or constructor. An interface may only have as members
    * constants (not fields), abstract methods, and no constructors.
    */
  sealed trait MemberDecl

  /**
    * The variables of a class type are introduced by field declarations.
    * @param modifiers
    * @param ty
    * @param var_decls
    */
  case class FieldDecl(
      modifiers: List[Modifier],
      ty: Type,
      var_decls: List[VarDecl]
  ) extends MemberDecl

  /**
    * A method declares executable code that can be invoked, passing a fixed number of values as arguments.
    *
    * @param modifiers
    * @param type_params
    * @param ty
    * @param id
    * @param formal_params
    * @param ex_types
    * @param exp
    * @param body
    */
  case class MethodDecl(
      modifiers: List[Modifier],
      type_params: List[TypeParam],
      ty: Option[Type],
      id: Ident,
      formal_params: List[FormalParam],
      ex_types: List[ExceptionType],
      exp: Option[Exp],
      body: MethodBody
  ) extends MemberDecl

  /**
    * A constructor is used in the creation of an object that is an instance of a class.
    *
    * @param modifiers
    * @param type_params
    * @param id
    * @param formal_parms
    * @param ex_types
    * @param body
    */
  case class ConstructorDecl(
      modifiers: List[Modifier],
      type_params: List[TypeParam],
      id: Ident,
      formal_parms: List[FormalParam],
      ex_types: List[ExceptionType],
      body: ConstructorBody
  ) extends MemberDecl

  /**
    * A member class is a class whose declaration is directly enclosed in another class or interface declaration.
    *
    * @param class_decl
    */
  case class MemberClassDecl(class_decl: ClassDecl) extends MemberDecl

  /**
    * A member interface is an interface whose declaration is directly enclosed in another class or interface declaration.
    *
    * @param iface_decl
    */
  case class MemberInterfaceDecl(iface_decl: InterfaceDecl) extends MemberDecl

  /**
    * A declaration of a variable, which may be explicitly initialized.
    *
    * @param id
    * @param var_init
    */
  case class VarDecl(id: VarDeclId, var_init: Option[VarInit])
  /**
    * The name of a variable in a declaration, which may be an array.
    */
  sealed trait VarDeclId
  case class VarId(id: Ident) extends VarDeclId

  /**
    * Multi-dimensional arrays are represented by nested applications of 'VarDeclArray'.
    *
    * @param var_decl_id
    */
  case class VarDeclArray(var_decl_id: VarDeclId) extends VarDeclId

  /**
    * Explicit initializer for a variable declaration.
    */
  sealed trait VarInit
  case class InitExp(exp: Exp) extends VarInit
  case class InitArray(array_init: ArrayInit) extends VarInit

  /**
    * A formal parameter in method declaration. The last parameter
    * for a given declaration may be marked as variable arity,
    * indicated by the boolean argument.
    * @param modifiers
    * @param ty
    * @param has_arity
    * @param var_decl_id
    */
  case class FormalParam(
      modifiers: List[Modifier],
      ty: Type,
      has_arity: Boolean,
      var_decl_id: VarDeclId
  )

  /**
    * A method body is either a block of code that implements the method or simply a
    *  semicolon, indicating the lack of an implementation (modelled by 'Nothing').
    * @param block
    */
  case class MethodBody(block: Option[Block])

  /**
    * The first statement of a constructor body may be an explicit invocation of
    * another constructor of the same class or of the direct superclass.
    * @param expl_Constr_inv
    * @param blk_stmts
    */
  case class ConstructorBody(
      expl_Constr_inv: Option[ExplConstrInv],
      blk_stmts: List[BlockStmt]
  )

  /**
    * An explicit constructor invocation invokes another constructor of the
    * same class, or a constructor of the direct superclass, which may
    * be qualified to explicitly specify the newly created object's immediately
    */
  sealed trait ExplConstrInv

  case class ThisInvoke(ref_types: List[RefType], args: List[Argument])
      extends ExplConstrInv
  case class SuperInvoke(ref_type: List[RefType], args: List[Argument])
      extends ExplConstrInv
  case class PrimarySuperInvoke(
      exp: Exp,
      ref_type: List[RefType],
      args: List[Argument]
  ) extends ExplConstrInv

  /**
    * A modifier specifying properties of a given declaration. In general only
    * a few of these modifiers are allowed for each declaration type, for instance
    * a member type declaration may only specify one of public, private or protected.
    */
  sealed trait Modifier
  case object Public extends Modifier
  case object Protected extends Modifier
  case object Private extends Modifier
  case object Abstract extends Modifier
  case object Final extends Modifier
  case object Static extends Modifier
  case object StrictFP extends Modifier
  case object Transient extends Modifier
  case object Volatile extends Modifier
  case object Native extends Modifier
  case class Annotation_(ann: Annotation) extends Modifier
  case object Synchronized extends Modifier

  /**
    * Annotations have three different forms: no-parameter, single-parameter or key-value pairs
    */
  sealed trait Annotation
  // Not type because not type generics not allowed
  case class NormalAnnotation(annName: Name, annKV: List[(Ident, ElementValue)])
      extends Annotation
  case class SingleElementAnnotation(annName: Name, annValue: ElementValue)
      extends Annotation
  case class MarkerAnnotation(annName: Name) extends Annotation


  def desugarAnnotation(a: Annotation): (Name, List[(Ident, ElementValue)]) =
    a match {
      case NormalAnnotation(annName, annKV) => (annName, annKV)
      case SingleElementAnnotation(annName, annValue) =>
        (annName, List((Ident("value"), annValue)))
      case MarkerAnnotation(annName) => (annName, List())
    }

  /**
    * Annotations may contain  annotations or (loosely) expressions
    */
  sealed trait ElementValue
  case class EVVal(var_init: VarInit) extends ElementValue
  case class EVAnn(ann: Annotation) extends ElementValue

  // Statements

  /**
    * A block is a sequence of statements, local class declarations
    * and local variable declaration statements within braces.
    * @param stmts
    */
  case class Block(stmts: List[BlockStmt])

  /**
    * A block statement is either a normal statement, a local
    * class declaration or a local variable declaration.
    */
  sealed trait BlockStmt
  case class BlockStmt_(stmt: Stmt) extends BlockStmt
  case class LocalClass(class_decl: ClassDecl) extends BlockStmt
  case class LocalVars(
      modifiers: List[Modifier],
      ty: Type,
      var_decls: List[VarDecl]
  ) extends BlockStmt

  /**
    * A Java statement.
    */
  sealed trait Stmt

  /**
    * A statement can be a nested block.
    *
    * @param blk
    */
  case class StmtBlock(blk: Block) extends Stmt

  /**
    * The @if-then@ statement allows conditional execution of a statement.
    *
    * @param exp
    * @param stmt
    */
  case class IfThen(exp: Exp, stmt: Stmt) extends Stmt

  /**
    * The @if-then-else@ statement allows conditional choice of two statements, executing one or the other but not both.
    *
    * @param exp
    * @param then_stmt
    * @param else_stmt
    */
  case class IfThenElse(exp: Exp, then_stmt: Stmt, else_stmt: Stmt) extends Stmt

  /**
    * The @while@ statement executes an expression and a statement repeatedly until the value of the expression is false.
    *
    * @param exp
    * @param stmt
    */
  case class While(exp: Exp, stmt: Stmt) extends Stmt

  /**
    * The basic @for@ statement executes some initialization code, then executes an expression, a statement, and some
    * update code repeatedly until the value of the expression is false.
    * @param init
    * @param loop_cond
    * @param post_update
    * @param stmt
    */
  case class BasicFor(
      init: Option[ForInit],
      loop_cond: Option[Exp],
      post_update: Option[List[Exp]],
      stmt: Stmt
  ) extends Stmt

  /**
    * The enhanced @for@ statement iterates over an array or a value of a class that implements the @iterator@ interface.
    *
    * @param modifiers
    * @param ty
    * @param id
    * @param exp
    * @param stmt
    */
  case class EnhancedFor(
      modifiers: List[Modifier],
      ty: Type,
      id: Ident,
      exp: Exp,
      stmt: Stmt
  ) extends Stmt

  /**
    * An empty statement does nothing.
    */
  case object Empty extends Stmt

  /**
    * Certain kinds of expressions may be used as statements by following them with semicolons:
    * assignments, pre- or post-inc- or decrementation, method invocation or class instance
    * creation expressions.
    *
    * @param exp
    */
  case class ExpStmt(exp: Exp) extends Stmt

  /**
    * An assertion is a statement containing a boolean expression, where an error is reported if the expression
    * evaluates to false.
    * @param exp
    * @param msg
    */
  case class Assert(exp: Exp, msg: Option[Exp]) extends Stmt

  /**
    * The switch statement transfers control to one of several statements depending on the value of an expression.
    *
    * @param exp
    * @param blocks
    */
  case class Switch(exp: Exp, blocks: List[SwitchBlock]) extends Stmt

  /**
    * The @do@ statement executes a statement and an expression repeatedly until the value of the expression is false.
    *
    * @param stmt
    * @param exp
    */
  case class Do(stmt: Stmt, exp: Exp) extends Stmt

  /**
    * A @break@ statement transfers control out of an enclosing statement.
    *
    * @param id
    */
  case class Break(id: Option[Ident]) extends Stmt

  /**
    * A @continue@ statement may occur only in a while, do, or for statement. Control passes to the loop-continuation
    * point of that statement.
    * @param id
    */
  case class Continue(id: Option[Ident]) extends Stmt

  /**
    * A @return@ statement returns control to the invoker of a method or constructor.
    *
    * @param exp
    */
  case class Return(exp: Option[Exp]) extends Stmt

  /**
    * A @synchronized@ statement acquires a mutual-exclusion lock on behalf of the executing thread, executes a block,
    * then releases the lock. While the executing thread owns the lock, no other thread may acquire the lock.
    * @param exp
    * @param blk
    */
  case class Synchronized(exp: Exp, blk: Block) extends Stmt

  /**
    * A @throw@ statement causes an exception to be thrown.
    *
    * @param exp
    */
  case class Throw(exp: Exp) extends Stmt

  /**
    * A try statement executes a block. If a value is thrown and the try statement has one or more catch clauses that
    * can catch it, then control will be transferred to the first such catch clause. If the try statement has a finally
    * clause, then another block of code is executed, no matter whether the try block completes normally or abruptly,
    * and no matter whether a catch clause is first given control.
    *
    * @param try_blk
    * @param catches
    * @param finally_blk
    */
  case class Try(
      try_blk: Block,
      catches: List[Catch],
      finally_blk: Option[Block]
  ) extends Stmt

  /**
    * Statements may have label prefixes.
    *
    * @param id
    * @param stmt
    */
  case class Labeled(id: Ident, stmt: Stmt) extends Stmt

  /**
    * If a value is thrown and the try statement has one or more catch clauses that can catch it, then control will be
    * transferred to the first such catch clause.
    *
    * @param params
    * @param blk
    */
  case class Catch(params: FormalParam, blk: Block)

  /**
    * A block of code labelled with a @case@ or @default@ within a @switch@ statement.
    *
    * @param label
    * @param blk_stmts
    */
  case class SwitchBlock(label: SwitchLabel, blk_stmts: List[BlockStmt])

  /**
    * A label within a @switch@ statement.
    */
  sealed trait SwitchLabel
  case class SwitchCase(exp: Exp) extends SwitchLabel
  case object Default extends SwitchLabel

  /**
    * Initialization code for a basic @for@ statement.
    */
  sealed trait ForInit
  case class ForLocalVars(
      modifiers: List[Modifier],
      ty: Type,
      var_decls: List[VarDecl]
  ) extends ForInit
  case class ForInitExps(exps: List[Exp]) extends ForInit

  /**
    * An exception type has to be a class type or a type variable.
    */
  type ExceptionType = RefType // restricted to ClassType or TypeVariable

  /**
    * Arguments to methods and constructors are expressions.
    */
  type Argument = Exp

  /**
    * A Java expression
    */
  sealed trait Exp

  /**
    * A literal denotes a fixed, unchanging value.
    */
  case class Lit(lit: Literal) extends Exp

  /**
    * A class literal, which is an expression consisting of the name of a class, interface, array,
    * or primitive type, or the pseudo-type void (modelled by 'Nothing'), followed by a `.' and the token class.
    */
  case class ClassLit(ty: Option[Type]) extends Exp

  /**
    * The keyword @this@ denotes a value that is a reference to the object for which the instance method
    * was invoked, or to the object being constructed.
    */
  case object This extends Exp

  /**
    * Any lexically enclosing instance can be referred to by explicitly qualifying the keyword this.
    *
    * @param name
    */
  case class ThisClass(name: Name) extends Exp

  /**
    * A class instance creation expression is used to create new objects that are instances of classes.
    * The first argument is a list of non-wildcard type arguments to a generic constructor.
    * What follows is the type to be instantiated, the list of arguments passed to the constructor, and
    * optionally a class body that makes the constructor result in an object of an /anonymous/ class.
    * @param type_args
    * @param type_decl
    * @param args
    * @param body
    */
  case class InstanceCreation(
      type_args: List[TypeArgument],
      type_decl: TypeDeclSpecifier,
      args: List[Argument],
      body: Option[ClassBody]
  ) extends Exp

  /**
    * A qualified class instance creation expression enables the creation of instances of inner member classes
    * and their anonymous subclasses.
    * @param exp
    * @param type_args
    * @param id
    * @param args
    * @param body
    */
  case class QualInstanceCreation(
      exp: Exp,
      type_args: List[TypeArgument],
      id: Ident,
      args: List[Argument],
      body: Option[ClassBody]
  ) extends Exp

  /**
    * An array instance creation expression is used to create new arrays. The last argument denotes the number
    * of dimensions that have no explicit length given. These dimensions must be given last.
    * @param ty
    * @param exps
    * @param num_dims
    */
  case class ArrayCreate(ty: Type, exps: List[Exp], num_dims: Int) extends Exp

  /**
    * An array instance creation expression may come with an explicit initializer. Such expressions may not
    * be given explicit lengths for any of its dimensions.
    * @param ty
    * @param size
    * @param init
    */
  case class ArrayCreateInit(ty: Type, size: Int, init: ArrayInit) extends Exp

  /**
    * A field access expression.
    * @param access
    */
  case class FieldAccess_(access: FieldAccess) extends Exp

  /**
    * A method invocation expression.
    *
    * @param methodInv
    */
  case class MethodInv(methodInv: MethodInvocation) extends Exp

  /**
    * An array access expression refers to a variable that is a component of an array.
    * @param idx
    */
  case class ArrayAccess(idx: ArrayIndex) extends Exp

  /**
    * An expression name, e.g. a variable.
    * @param name
    */
  case class ExpName(name: Name) extends Exp

  /**
    * Post-incrementation expression, i.e. an expression followed by @++@.
    * @param exp
    */
  case class PostIncrement(exp: Exp) extends Exp

  /**
    * Post-decrementation expression, i.e. an expression followed by @--@.
    * @param exp
    */
  case class PostDecrement(exp: Exp) extends Exp

  /**
    * Pre-incrementation expression, i.e. an expression preceded by @++@.
    * @param exp
    */
  case class PreIncrement(exp: Exp) extends Exp

  /**
    * Pre-decrementation expression, i.e. an expression preceded by @--@.
    * @param exp
    */
  case class PreDecrement(exp: Exp) extends Exp

  /**
    * Unary plus, the promotion of the value of the expression to a primitive numeric type.
    * @param exp
    */
  case class PrePlus(exp: Exp) extends Exp

  /**
    * Unary minus, the promotion of the negation of the value of the expression to a primitive numeric type.
    * @param exp
    */
  case class PreMinus(exp: Exp) extends Exp

  /**
    * Unary bitwise complementation: note that, in all cases, @~x@ equals @(-x)-1@.
    * @param exp
    */
  case class PreBitCompl(exp: Exp) extends Exp

  /**
    * Logical complementation of boolean values.
    * @param exp
    */
  case class PreNot(exp: Exp) extends Exp

  /**
    * A cast expression converts, at run time, a value of one numeric type to a similar value of another
    * numeric type; or confirms, at compile time, that the type of an expression is boolean; or checks,
    * at run time, that a reference value refers to an object whose class is compatible with a specified
    * reference type.
    * @param ty
    * @param exp
    */
  case class Cast(ty: Type, exp: Exp) extends Exp

  /**
    * The application of a binary operator to two operand expressions.
    * @param e1
    * @param op
    * @param e2
    */
  case class BinOp(e1: Exp, op: Op, e2: Exp) extends Exp

  /**
    * Testing whether the result of an expression is an instance of some reference type.
    * @param e
    * @param ref_type
    */
  case class InstanceOf(e: Exp, ref_type: RefType) extends Exp

  /**
    * The conditional operator @? :@ uses the boolean value of one expression to decide which of two other
    * expressions should be evaluated.
    * @param cond
    * @param true_exp
    * @param false_exp
    */
  case class Cond(cond: Exp, true_exp: Exp, false_exp: Exp) extends Exp

  /**
    * Assignment of the result of an expression to a variable.
    * @param lhs
    * @param op
    * @param rhs
    */
  case class Assign(lhs: Lhs, op: AssignOp, rhs: Exp) extends Exp

  /**
    * Lambda expression
    * @param params
    * @param body
    */
  case class Lambda(params: LambdaParams, body: LambdaExpression) extends Exp

  /**
    * Method reference
    * @param name
    * @param id
    */
  case class MethodRef(name: Name, id: Ident) extends Exp


  /**
    * The left-hand side of an assignment expression. This operand may be a named variable, such as a local
    * variable or a field of the current object or class, or it may be a computed variable, as can result from
    * a field access or an array access.
    */
  sealed trait Lhs

  /**
    * Assign to a variable
    *
    * @param name
    */
  case class NameLhs(name: Name) extends Lhs

  /**
    * Assign through a field access
    *
    * @param field_access
    */
  case class FieldLhs(field_access: FieldAccess) extends Lhs

  /**
    * Assign to an array
    *
    * @param array_idx
    */
  case class ArrayLhs(array_idx: ArrayIndex) extends Lhs

  /**
    * Array access
    *
    * @param e index into an array
    * @param es
    */
  case class ArrayIndex(e: Exp, es: List[Exp])

  /**
    * A field access expression may access a field of an object or array, a reference to which is the value
    * of either an expression or the special keyword super.
    */
  sealed trait FieldAccess

  /**
    * Accessing a field of an object or array computed from an expression.
    *
    * @param e
    * @param id
    */
  case class PrimaryFieldAccess(e: Exp, id: Ident) extends FieldAccess

  /**
    * Accessing a field of the superclass.
    *
    * @param id
    */
  case class SuperFieldAccess(id: Ident) extends FieldAccess

  /**
    * Accessing a (static) field of a named class.
    *
    * @param name
    * @param id
    */
  case class ClassFieldAccess(name: Name, id: Ident) extends FieldAccess

  /**
    * A lambda parameter can be a single parameter, or mulitple formal or mulitple inferred parameters
    */
  sealed trait LambdaParams
  case class LambdaSingleParam(id: Ident) extends LambdaParams
  case class LambdaFormalParams(formal_params: List[FormalParam])
      extends LambdaParams
  case class LambdaInferredParams(ids: List[Ident]) extends LambdaParams

  /**
    * Lambda expression, starting from java 8
    */
  sealed trait LambdaExpression
  case class LambdaExpression_(e: Exp) extends LambdaExpression
  case class LambdaBlock(blk: Block) extends LambdaExpression

  /**
    * A method invocation expression is used to invoke a class or instance method.
    */
  sealed trait MethodInvocation

  /**
    * Invoking a specific named method.
    *
    * @param name
    * @param args
    */
  case class MethodCall(name: Name, args: List[Argument])
      extends MethodInvocation

  /**
    * Invoking a method of a class computed from a primary expression, giving arguments for any generic type parameters.
    *
    * @param e
    * @param ref_types
    * @param id
    * @param args
    */
  case class PrimaryMethodCall(
      e: Exp,
      ref_types: List[RefType],
      id: Ident,
      args: List[Argument]
  ) extends MethodInvocation

  /**
    * Invoking a method of the super class, giving arguments for any generic type parameters.
    *
    * @param ref_types
    * @param id
    * @param args
    */
  case class SuperMethodCall(
      ref_types: List[RefType],
      id: Ident,
      args: List[Argument]
  ) extends MethodInvocation

  /**
    * Invoking a method of the superclass of a named class, giving arguments for any generic type parameters.
    *
    * @param name
    * @param ref_types
    * @param id
    * @param args
    */
  case class ClassMethodCall(
      name: Name,
      ref_types: List[RefType],
      id: Ident,
      args: List[Argument]
  ) extends MethodInvocation

  /**
    * Invoking a method of a named type, giving arguments for any generic type parameters.
    *
    * @param name
    * @param ref_types
    * @param id
    * @param args
    */
  case class TypeMethodCall(
      name: Name,
      ref_types: List[RefType],
      id: Ident,
      args: List[Argument]
  ) extends MethodInvocation

  /**
    * An array initializer may be specified in a declaration, or as part of an array creation expression, creating an
    * array and providing some initial values
    *
    * @param var_inits
    */
  case class ArrayInit(var_inits: List[VarInit])

  // Exp Misc
  /**
    * A literal denotes a fixed, unchanging value.
    */
  sealed trait Literal
  case class IntLit(i: Int) extends Literal
  case class LongLit(l: Long) extends Literal
  case class FloatLit(f: Float) extends Literal
  case class DoubleLit(d: Double) extends Literal
  case class BooleanLit(b: Boolean) extends Literal
  case class CharLit(c: Char) extends Literal
  case class StringLit(s: String) extends Literal
  case object NullLit extends Literal

  /**
    * A binary infix operator.
    */
  sealed trait Op
  case object Mult extends Op
  case object Div extends Op
  case object Rem extends Op
  case object Add extends Op
  case object Sub extends Op
  case object LShift extends Op
  case object RShift extends Op
  case object RRShift extends Op
  case object LThan extends Op
  case object GThan extends Op
  case object LThanE extends Op
  case object GThanE extends Op
  case object Equal extends Op
  case object NotEq extends Op
  case object And extends Op
  case object Or extends Op
  case object Xor extends Op
  case object CAnd extends Op
  case object COr extends Op

  /**
    * An assignment operator.
    */
  sealed trait AssignOp
  case object EqualA extends AssignOp
  case object MultA extends AssignOp
  case object DivA extends AssignOp
  case object RemA extends AssignOp
  case object AddA extends AssignOp
  case object SubA extends AssignOp
  case object LShiftA extends AssignOp
  case object RShiftA extends AssignOp
  case object RRShiftA extends AssignOp
  case object AndA extends AssignOp
  case object XorA extends AssignOp
  case object OrA extends AssignOp

  // Type Misc

  /**
    * There are two kinds of types in the Java programming language: primitive types and reference types.
    */
  sealed trait Type
  case class PrimType_(p_ty: PrimType) extends Type
  case class RefType_(r_ty: RefType) extends Type

  /**
    * There are three kinds of reference types: class types, interface types, and array types.
    * Reference types may be parameterized with type arguments.
    * Type variables cannot be syntactically distinguished from class type identifiers,
    * and are thus represented uniformly as single ident class types.
    */
  sealed trait RefType
  case class ClassRefType(c: ClassType) extends RefType
  case class ArrayType(t: Type) extends RefType

  /**
    * A class or interface type consists of a type declaration specifier,
    * optionally followed by type arguments (in which case it is a parameterized type).
    *
    * @param ty_decl
    */
  case class ClassType(ty_decl: List[(Ident, List[TypeArgument])])

  /**
    * Type arguments may be either reference types or wildcards.
    */
  sealed trait TypeArgument
  case class Wildcard(bnd: Option[WildcardBound]) extends TypeArgument
  case class ActualType(ref_type: RefType) extends TypeArgument

  sealed trait TypeDeclSpecifier
  case class TypeDeclSpecifier_(c_type: ClassType) extends TypeDeclSpecifier
  case class TypeDeclSpecifierWithDiamond(
      c_type: ClassType,
      id: Ident,
      diamond: Diamond
  ) extends TypeDeclSpecifier
  case class TypeDeclSpecifierUnqualifiedWithDiamond(
      id: Ident,
      diamond: Diamond
  ) extends TypeDeclSpecifier

  case class Diamond()

  /**
    * Wildcards may be given explicit bounds, either upper (@extends@) or lower (@super@) bounds.
    */
  sealed trait WildcardBound
  case class ExtendsBound(ref_type: RefType) extends WildcardBound
  case class SuperBound(ref_type: RefType) extends WildcardBound

  /**
    * A primitive type is predefined by the Java programming language and named by its reserved keyword.
    */
  sealed trait PrimType
  case object BooleanT extends PrimType
  case object ByteT extends PrimType
  case object ShortT extends PrimType
  case object IntT extends PrimType
  case object LongT extends PrimType
  case object CharT extends PrimType
  case object FloatT extends PrimType
  case object DoubleT extends PrimType

  /**
    * A class is generic if it declares one or more type variables. These type variables are known
    * as the type parameters of the class.
    *
    * @param id
    * @param ref_types
    */
  case class TypeParam(id: Ident, ref_types: List[RefType])

  // Names and identifier

  /**
    * A single identifier.
    *
    * @param name
    */
  case class Ident(name: String)


  /**
    * A name, i.e. a period-separated list of identifiers.
    *
    * @param ids
    */
  case class Name(ids: List[Ident])
}
