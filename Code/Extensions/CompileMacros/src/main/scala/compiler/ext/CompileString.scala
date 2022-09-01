package compiler.ext

import clangast.*
import clangast.given
import clangast.CASTNode
import clangast.decl.{CFunctionDecl, CParmVarDecl, CVarDecl}
import clangast.expr.*
import clangast.expr.binaryop.CPlusExpr
import clangast.stmt.{CCompoundStmt, CExprStmt, CReturnStmt, CStmt}
import clangast.stubs.{CJSONH, StdBoolH, StdIOH, StdLibH, StringH}
import clangast.types.{CCharType, CIntegerType, CPointerType, CType}
import compiler.context.{FunctionDeclTC, TranslationContext}
import compiler.CompilerCascade
import compiler.base.*

import scala.quoted.*

object CompileString extends TermPC with ApplyPC with TypePC with StringPC with SerializationPC {
  override def compileLiteral(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Literal, CExpr] = {
      import quotes.reflect.*

      {
        case Literal(StringConstant(x)) => CStringLiteral(x)
      }
    }

  override def compileTerm(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Term, CASTNode] = {
      import quotes.reflect.*

      {
        case Apply(Ident("print"), List(arg)) =>
          cascade.dispatch(_.compilePrint)(
            cascade.dispatch(_.compileTermToCExpr)(arg),
            arg.tpe
          )
        case Apply(Ident("println"), List(arg)) =>
          CCompoundStmt(List(
            cascade.dispatch(_.compilePrint)(
              cascade.dispatch(_.compileTermToCExpr)(arg),
              arg.tpe
            ),
            printf("\\n")
          ))
      }
    }

  override def compileApply(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Apply, CExpr] = {
      import quotes.reflect.*

      {
        case Apply(Select(expr, "toString"), List()) =>
          cascade.dispatch(_.compileToString)(
            cascade.dispatch(_.compileTermToCExpr)(expr),
            expr.tpe
          )
        case Apply(Select(expr, "length"), List()) if expr.tpe <:< TypeRepr.of[String] =>
          CCallExpr(
            StringH.strlen.ref,
            List(cascade.dispatch(_.compileTermToCExpr)(expr))
          )
      }
    }

  override def compileTypeRepr(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CType] = {
      import quotes.reflect.*

      {
        case ConstantType(_: StringConstant) => CPointerType(CCharType)
        case tpe if tpe =:= TypeRepr.of[String] => CPointerType(CCharType)
      }
    }

  override def defaultValue(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CExpr] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[String] => CStringLiteral("")
      }
    }

  override def compilePrint(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CStmt] = {
      import quotes.reflect.*

      {
        case (expr, tpe) if tpe <:< TypeRepr.of[Boolean] =>
          printf(
            "%s",
            CConditionalOperator(
              expr,
              CStringLiteral("true"),
              CStringLiteral("false")
            )
          )
        case (expr, tpe) if tpe <:< TypeRepr.of[Byte | Short | Int | Long] =>
          printf("%d", expr)
        case (expr, tpe) if tpe <:< TypeRepr.of[Char] =>
          printf("%c", expr)
        case (expr, tpe) if tpe <:< TypeRepr.of[Float | Double] =>
          printf("%f", expr)
        case (expr, tpe) if tpe <:< TypeRepr.of[String] =>
          printf("\\\"%s\\\"", expr)
      }
    }

  def compileToStringImpl(using Quotes)(using ctx: FunctionDeclTC, cascade: CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CExpr] = {
      import quotes.reflect.*

      {
        case (expr, tpe) if tpe <:< TypeRepr.of[Boolean] =>
          val funName = toStringFunName(tpe)
          val f = ctx.nameToFunctionDecl.getOrElseUpdate(funName, {
            val parmDecl = CParmVarDecl("p", StdBoolH.bool)

            val strDecl = stringDecl("str", 5.lit)

            val body = CCompoundStmt(List(
              strDecl,
              sprintf(
                strDecl.ref,
                "%s",
                CConditionalOperator(parmDecl.ref, CStringLiteral("true"), CStringLiteral("false"))
              ),
              CReturnStmt(Some(strDecl.ref))
            ))

            CFunctionDecl(funName, List(parmDecl), CPointerType(CCharType), Some(body))
          })

          CCallExpr(f.ref, List(expr))
        case (expr, tpe) if tpe <:< TypeRepr.of[Byte | Short | Int | Long] =>
          toStringWithUnknownLength("%d", expr, tpe)
        case (expr, tpe) if tpe <:< TypeRepr.of[Char] =>
          val funName = toStringFunName(tpe)
          val f = ctx.nameToFunctionDecl.getOrElseUpdate(funName, {
            val parmDecl = CParmVarDecl("p", CCharType)

            val strDecl = stringDecl("str", 2.lit)

            val body = CCompoundStmt(List(
              strDecl,
              sprintf(strDecl.ref, "%c", parmDecl.ref),
              CReturnStmt(Some(strDecl.ref))
            ))

            CFunctionDecl(funName, List(parmDecl), CPointerType(CCharType), Some(body))
          })

          CCallExpr(f.ref, List(expr))
        case (expr, tpe) if tpe <:< TypeRepr.of[Float | Double] =>
          toStringWithUnknownLength("%f", expr, tpe)
        case (expr, tpe) if tpe <:< TypeRepr.of[String] =>
          val funName = toStringFunName(tpe)
          val f = ctx.nameToFunctionDecl.getOrElseUpdate(funName, {
            val parmDecl = CParmVarDecl("p", CPointerType(CCharType))

            val strDecl = stringDecl("str", CCallExpr(StringH.strlen.ref, List(parmDecl.ref)))

            val body = CCompoundStmt(List(
              strDecl,
              sprintf(strDecl.ref, "%s", parmDecl.ref),
              CReturnStmt(Some(strDecl.ref))
            ))

            CFunctionDecl(funName, List(parmDecl), CPointerType(CCharType), Some(body))
          })

          CCallExpr(f.ref, List(expr))
      }
    }

  override def compileToString(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CExpr] = ensureCtx[FunctionDeclTC](compileToStringImpl)

  override def compileDeserialize(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[String] =>
          getStringDeserialize()
      }
    }

  private def getStringDeserialize()(using ctx: TranslationContext): CFunctionDecl = {
    ctx.valueDeclList.collect {
      case funDecl @ CFunctionDecl("deserialize_String", _, _, _, _) => funDecl
    } match {
      case List(funDecl) => funDecl
      case _ =>
        val funDecl = buildStringDeserialize()
        ctx.addValueDecl(funDecl)
        funDecl
    }
  }

  private def buildStringDeserialize()(using ctx: TranslationContext): CFunctionDecl = {
    val name = "deserialize_String"

    val jsonParam = CParmVarDecl("json", CPointerType(CJSONH.cJSON))

    val lengthDecl = CVarDecl(
      "length",
      CIntegerType,
      Some(CCallExpr(StringH.strlen.ref, List(CJSONH.valuestring(jsonParam.ref))))
    )
    val copyDecl = CVarDecl(
      "copy",
      CPointerType(CCharType),
      Some(CCastExpr(CCallExpr(StdLibH.calloc.ref, List(lengthDecl.ref, CSizeofExpr(Left(CCharType)))), CPointerType(CCharType)))
    )

    val body = CCompoundStmt(List(
      lengthDecl,
      copyDecl,
      CCallExpr(StringH.strcpy.ref, List(copyDecl.ref, CJSONH.valuestring(jsonParam.ref))),
      CReturnStmt(Some(copyDecl.ref))
    ))

    CFunctionDecl(name, List(jsonParam), CPointerType(CCharType), Some(body))
  }

  def printf(format: String, args: CExpr*)(using TranslationContext): CExpr =
    CCallExpr(
      StdIOH.printf.ref,
      CStringLiteral(format) :: args.toList
    )

  def toStringFunName(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: TranslationContext, cascade: CompilerCascade): String =
    "toString_" + cascade.dispatch(_.typeName)(tpe)

  def sprintf(dest: CExpr, format: String, args: CExpr*)(using ctx: TranslationContext): CExpr =
    CCallExpr(
      StdIOH.sprintf.ref,
      dest :: CStringLiteral(format) :: args.toList
    )

  def stringDecl(name: String, length: CExpr)(using ctx: TranslationContext): CVarDecl =
    CVarDecl(
      name,
      CPointerType(CCharType),
      Some(CCastExpr(
        CCallExpr(StdLibH.calloc.ref, List(CPlusExpr(length, 1.lit), CSizeofExpr(Left(CCharType)))),
        CPointerType(CCharType)
      ))
    )

  def toStringLength(format: String, args: CExpr*)(using ctx: TranslationContext): CExpr =
    CCallExpr(
      StdIOH.snprintf.ref,
      CNullLiteral :: 0.lit :: CStringLiteral(format) :: args.toList
    )

  def toStringWithUnknownLength(using Quotes)(format: String, expr: CExpr, tpe: quotes.reflect.TypeRepr)(using ctx: FunctionDeclTC, cascade: CompilerCascade): CExpr =
    val funName = toStringFunName(tpe)
    val f = ctx.nameToFunctionDecl.getOrElseUpdate(funName, {
      val parmDecl = CParmVarDecl("p", cascade.dispatch(_.compileTypeRepr)(tpe))

      val strDecl = stringDecl("str", toStringLength(format, parmDecl.ref))

      val body = CCompoundStmt(List(
        strDecl,
        sprintf(strDecl.ref, format, parmDecl.ref),
        CReturnStmt(Some(strDecl.ref))
      ))

      CFunctionDecl(funName, List(parmDecl), CPointerType(CCharType), Some(body))
    })

    CCallExpr(f.ref, List(expr))
}
