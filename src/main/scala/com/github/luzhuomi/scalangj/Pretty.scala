package com.github.luzhuomi.scalangj

import org.typelevel.paiges._
import org.typelevel.paiges.Doc._
import com.github.luzhuomi.scalangj.Syntax._
import cats.implicits._
import cats._
import scala.collection.immutable.IndexedSeq.Impl



object Pretty {
    def prettyPrint[A](a:A)(implicit pa:Pretty[A]) : String = {
        ""
    }

    trait Pretty[A] {
        def pretty(a:A): Doc = prettyPrec(0,a)
        def prettyPrec(prec:Int, a:A):Doc = pretty(a)
 
    }
    
    
    implicit def compilationUnitPretty(implicit pdPty:Pretty[PackageDecl]
                                    , ipPty: Pretty[ImportDecl]
                                    , tdPty:Pretty[TypeDecl]) = new Pretty[CompilationUnit] { 
        override def prettyPrec(p:Int, cu:CompilationUnit) = cu match {
            case CompilationUnit(mpd, ids, tds) => {
                vcat( (maybePP(p,mpd)(pdPty)::ids.map(id=>ipPty.prettyPrec(p,id))) ++ tds.map(td => tdPty.prettyPrec(p,td)))
            }
        }
    }

    implicit def packageDeclPretty(implicit namePty:Pretty[Name]) = new Pretty[PackageDecl] {
        override def prettyPrec(p:Int, pdecl:PackageDecl) =  pdecl match {
            case PackageDecl(name) => 
                spread(List(text("package"), namePty.prettyPrec(p,name)))+semi 
        }
    }

    implicit def importDeclPretty(implicit namePty:Pretty[Name]) = new Pretty[ImportDecl]  {
        override def prettyPrec(p:Int, idecl:ImportDecl) = idecl match {
            case ImportDecl(st,name,wc) =>
                spread(List(text("import"), 
                            opt(st,text("static")), 
                            namePty.prettyPrec(p,name))) + opt(wc, text(".*")) + semi
        }
    }

    // ------------------------------------------------
    // Declarations

    implicit def typeDeclPretty(implicit cdPty:Pretty[ClassDecl]
                               , idPty:Pretty[InterfaceDecl]) = new Pretty[TypeDecl] {
        override def prettyPrec(p:Int, tdecl:TypeDecl) = tdecl match {
            case ClassTypeDecl(cd) => cdPty.prettyPrec(p,cd)
            case InterfaceTypeDecl(id) => idPty.prettyPrec(p,id)
        }
    }

    implicit def classDeclPretty(implicit modPty:Pretty[Modifier]
                                , idPty:Pretty[Ident]
                                , ebdPty:Pretty[EnumBody]
                                , cbdPty:Pretty[ClassBody]) = new Pretty[ClassDecl] {
        override def prettyPrec(p:Int, cdecl:ClassDecl) = cdecl match {
            case EnumDecl(mods,ident,impls,body) => {
                val p1 = hsep(mods.map(modPty.prettyPrec(p,_)))
                val p2 = text("enum")
                val p3 = idPty.prettyPrec(p,ident)
                val p4 = ppImplements(p,impls)
                val p5 = ebdPty.prettyPrec(p,body)
                stack(List(hsep(List(p1,p2,p3,p4)), p5))
            } 
            case ClassDecl_(mods,ident,tParams,mSuper,impls,body) => {
                val p1 = hsep(mods.map(modPty.prettyPrec(p,_)))
                val p2 = text("class")
                val p3 = ppTypeParams(p,tParams)
                val p4 = ppExtends(p, maybe(Nil, (x:RefType)=>List(x), mSuper))
                val p5 = ppImplements(p,impls)
                val p6 = cbdPty.prettyPrec(p,body)
                stack(List(hsep(List(p1,p2,p3,p4,p5)),p6))
            }
        }
    }

    implicit def classBodyPretty(implicit declPty:Pretty[Decl]) = new Pretty[ClassBody] {
        override def prettyPrec(p:Int, cbody:ClassBody) = cbody match {
            case ClassBody(ds) => braceBlock( ds.map(declPty.prettyPrec(p,_))) 
        }
    }

    implicit def enumBodyPretty(implicit ecPty:Pretty[EnumConstant], declPty:Pretty[Decl]) = new Pretty[EnumBody] {
        override def prettyPrec(p:Int, ebody:EnumBody) = ebody match {
            case EnumBody(cs,ds) => braceBlock( 
                punctuate(comma, cs.map(ecPty.prettyPrec(p, _))) ++ 
                (opt((ds.length > 0), semi)::(ds.map(declPty.prettyPrec(p, _))))
            )
        }
    }

    implicit def enumConstantPretty(implicit idPty:Pretty[Ident]
                                    , cbPty:Pretty[ClassBody]) = new Pretty[EnumConstant] {
        override def prettyPrec(p:Int, ec:EnumConstant) = ec match {
            case EnumConstant(ident,args,mBody) => {
                stack(List(idPty.prettyPrec(p,ident) + opt(args.length > 0, ppArgs(p,args))
                         , maybePP(p,mBody))) 
            }
        }
    }

    implicit def interfaceDeclPretty(implicit modPty:Pretty[Modifier]
                                    , idPty:Pretty[Ident]
                                    , ifBodyPty:Pretty[InterfaceBody]) = new Pretty[InterfaceDecl] 
    {
        override def prettyPrec(p:Int, ifd:InterfaceDecl) = ifd match {
            case InterfaceDecl(kind,mods, ident, tParams, impls, body) => {
                val p1 = hsep(mods.map(modPty.prettyPrec(p,_)))
                val p2 = text(if (kind == InterfaceNormal) "interface" else "@interface")
                val p3 = idPty.prettyPrec(p,ident)
                val p4 = ppTypeParams(p,tParams)
                val p5 = ppExtends(p,impls)
                val p6 = ifBodyPty.prettyPrec(p,body)
                stack(List(hsep(List(p1,p2,p3,p4,p5)), p6))
            }
        }

    }

    def ppImplements(prec:Int,impls:List[RefType]):Doc = empty
    def ppTypeParams(prec:Int,typeParams:List[TypeParam]):Doc = empty
    def ppExtends(prec:Int,exts:List[RefType]):Doc = empty
    def ppArgs(prec:Int, args:List[Argument]):Doc = empty

    implicit val interfaceDeclPretty = new Pretty[InterfaceDecl] {
        override def prettyPrec(p:Int, idecl:InterfaceDecl) = empty
    }

    implicit val namePretty = new Pretty[Name] {
        override def prettyPrec(p:Int, name:Name) = empty
    }

    
    val semi:Doc = char(';')

    // ---------------------------------------------------------------------
    // Help functionality 

    def vcat(ds:List[Doc]):Doc = stack(ds)

    def opt(x:Boolean, a:Doc):Doc = if (x) a else empty

    def braceBlock(xs:List[Doc]):Doc = { // TODO: shall we use bracketBy
        stack(List(char('{'), nest(2,vcat(xs)), char('}')))
    }

    def nest(k:Int, p:Doc):Doc = p.indent(k)


    def maybePP[A](p:Int, mba:Option[A])(implicit ppa:Pretty[A]):Doc = mba match {
        case None => empty
        case Some(a) => ppa.prettyPrec(p,a)
    }
    def maybe[A,B](default:B,f:(A => B), mba:Option[A]):B = mba match {
        case None => default
        case Some(a) => f(a)
    }

    def hsep(ds:List[Doc]):Doc = spread(ds.filter(!_.isEmpty))

    def punctuate(p:Doc, ds:List[Doc]):List[Doc] = ds match {
        case Nil => Nil
        case (x::xs) => { 
            def go(y:Doc,zs:List[Doc]):List[Doc] = zs match {
                case Nil => List(y)
                case (z::zss) => (y+p) :: (go(z,zss))
            }
            go(x,xs)
        }
    }

}