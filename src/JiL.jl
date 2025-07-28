module JiL
using Unicode

export debug_lisp_to_julia, install_jil_parser, restore_julia_parser, tojulia, @jil_str, @jilm_str, @jilEval_str, @julia_str
include("List.jl")
include("ToJulia.jl")
include("JiLParser.jl")
include("LangParser.jl")

install_jil_parser() # Let's have the parser ready
include("Boot0.jil") # Primitive operations (car, cdr, null?, defmacro, etc)
include("Boot1.jil") # Useful macros that use the primitives (cond, macroexpand, etc)
include("Boot2.jil") # More useful macros (define, when, etc)
restore_julia_parser()

macro jil_str(str)
  let (ast, offset) = jil_parse(str, "nofile", 0, 0, false)
    ast isa Expr && ast.head == :toplevel && length(ast.args) == 1 ?
      :(clean_expr!((@macroexpand($(ast.args[1]))))) :
      :(clean_expr!((@macroexpand($ast))))
  end
end

macro jilEval_str(str)
  let (ast, offset) = jil_parse(str, "nofile", 0, 0, false)
    esc(ast)
  end
end

macro jilm_str(str)
  let (ast, offset) = jil_parse(str, "nofile", 0, 0, false)
    esc(ast isa Expr && ast.head == :toplevel && length(ast.args) == 1 ?
          QuoteNode(clean_expr!(macroexpand(__module__, ast.args[1]))) :
          QuoteNode(clean_expr!(macroexpand(__module__, ast))))
  end
end


macro julia_str(str)
  esc(QuoteNode(clean_expr!(Meta.parse(str))))
end

macro current_module()
  __module__
end

__init__() = begin
  install_jil_parser() # Let's have the parser ready
  # The module that loaded this thing first (presumably, Main)
  # needs to have its own copy of JiL's JiL_scope
   
end
end