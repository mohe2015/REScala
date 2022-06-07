import compiler.base.*
import compiler.ext.*

package object compiler {
  val minimalCascade: CompilerCascade = CompilerCascade(
    CompileApply,
    CompileDefinition,
    CompileMatch,
    CompileRef,
    CompileSelect,
    CompileStatement,
    CompileTerm,
    CompileTree,
    CompileType
  )

  val standardCascade: CompilerCascade =
    CompileHelperFun
      ~>: CompileArray
      ~>: CompileProduct
      ~>: minimalCascade
}
