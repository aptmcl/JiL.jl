module JiL
export debug_lisp_to_julia, install_jil_parser, restore_julia_parser, tojulia, @jil_str, @jilm_str, @julia_str
include("List.jl")
include("ToJulia.jl")
include("JiLParser.jl")

clean_expr!(ex) = begin
  if ex isa Expr
    if ex.head === :block || ex.head === :quote
      filter!(ex.args) do x
          isa(x, Expr) && x.head === :line && return false
          isa(x, LineNumberNode) && return false
          return true
      end
    end
    for subex in ex.args
      subex isa Expr && clean_expr!(subex)
    end
    for (i, subex) in enumerate(ex.args)
      if subex isa Expr && subex.head === :block && length(subex.args) == 1
        ex.args[i] = subex.args[1]
      end
    end
  elseif ex isa CodeInfo
      ex.debuginfo = Core.DebugInfo(ex.debuginfo.def) # TODO: filter partially, but keep edges
  end
  return ex
end

macro jil_str(str)
  let (ast, offset) = jil_parse(str, "nofile", 0, 0, false)
    ast isa Expr && ast.head == :toplevel && length(ast.args) == 1 ?
      QuoteNode(clean_expr!(ast.args[1])) :
      QuoteNode(clean_expr!(ast))
  end
end

macro jilm_str(str)
  let (ast, offset) = jil_parse(str, "nofile", 0, 0, false)
    ast isa Expr && ast.head == :toplevel && length(ast.args) == 1 ?
      QuoteNode(clean_expr!(macroexpand(JiL, ast.args[1]))) :
      QuoteNode(clean_expr!(macroexpand(JiL, ast)))
  end
end


macro julia_str(str)
  QuoteNode(clean_expr!(Meta.parse(str)))
end

__init__() = begin
  install_jil_parser() # Let's have the parser ready
  include(joinpath(@__DIR__, "Boot0.jil")) # Primitive operations (car, cdr, null?, defmacro, etc)
  include(joinpath(@__DIR__, "Boot1.jil")) # Useful macros that use the primitives (cond, macroexpand, etc)
  include(joinpath(@__DIR__, "Boot2.jil")) # More useful macros (when, etc)
end
end
