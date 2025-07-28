#=
The convention is that an opening parenthesis at the beginning of a 
line means we have JiL code.

This means that, in order to parse Julia code that starts with an
open parenthesis, e.g., to create a tuple, one needs to preceed
the code with some whitespaces.
=#

looks_like_lisp(text) = 
  startswith(text, "(")

lisp_read(text, offset=1) =
  let parser = JiLIO(input=text, offset=offset)
    try
      let lisp_form = read(nil, parser)
        (lisp_form, parser.offset)
      end
    catch e
      if e === EOFError()
        (Expr(:incomplete, "Input error"), parser.offset)
      else
        rethrow(e)
      end
    end
  end

get_parser_for(str) =
  if str == "jil"
    jil_parse
  elseif str == "julia"
    julia_parser
  else
    nothing
  end

#=
// Parse `text` starting at 0-based `offset` and attributing the content to
// `filename`. Return an svec of (parsed_expr, final_offset)
=#
lang_parse(text::Union{Core.SimpleVector,String}, filename::String, lineno, offset, options) =
  let matched = offset == 0 ? match(r"^#lang:(.+?)\r?\n(.+)$"s, text) : nothing
    isnothing(matched) ?
      julia_jil_parse(text, filename, lineno, offset, options) :
      let (lang, text) = matched,
          parser = get_parser_for(lang)
        isnothing(parser) ?
          (Expr(:incomplete, "No parser found for language '$lang'"), offset) :
          parser(text, filename, lineno, offset, options)
      end
  end

addform!(forms, form) =
  form isa Expr && form.head === :toplevel ?
    append!(forms, form.args) :
    push!(forms, form)

maybe_wrap_toplevel(forms) =
  length(forms) == 1 ?
    forms[1] :
    Expr(:toplevel, forms...)

jil_parse(text::Union{Core.SimpleVector,String}, filename::String, lineno, offset, options) =
  let forms = []
    offset += 1
    while offset <= length(text)
      if isspace(text[offset])
        offset += 1
      else
        let (form, new_offset) = jil_parse_1(text, offset)
          (addform!(forms, form); offset = new_offset + 1)
        end
      end
    end
    (Expr(:toplevel, forms...), offset)
  end

debug_lisp_to_julia = false

jil_parse_1(text, offset) =
  let (lisp_form, new_offset) = lisp_read(text, offset)
    lisp_form isa Expr ?
      (lisp_form, new_offset) :
      let julia_form = :(@tojulia_toplevel_wrapper($lisp_form))
        debug_lisp_to_julia && println(" <=> ", julia_form)
        (julia_form, new_offset)
      end
  end

export @tojulia_toplevel_wrapper
macro tojulia_toplevel_wrapper(lisp_form)
  let julia_expr = tojulia_toplevel(lisp_form, __module__)
    debug_lisp_to_julia && println(" => ", julia_expr)
    esc(julia_expr)
  end
end
  

julia_jil_parse(text::Union{Core.SimpleVector,String}, filename::String, lineno, offset, options) =
  let forms = []
    while offset < length(text) && isspace(text[offset + 1])
      offset += 1
    end
    while offset < length(text)
      if looks_like_lisp(SubString(text, offset + 1))
        let (ast, new_offset) = jil_parse_1(text, offset + 1)
          if ast isa Expr && ast.head === :incomplete
            return (ast, new_offset)
          else
            (addform!(forms, ast); offset = new_offset + 1)
          end
        end
        while offset < length(text) && isspace(text[offset + 1])
          offset += 1
        end
      else
        let next_offset = findnext(r"\r?\n\("s, text, offset + 1),
            (ast, new_offset) = isnothing(next_offset) ? 
                                  julia_parser(text, filename, lineno, offset, options) :
                                  julia_parser(SubString(text, 1, first(next_offset)), filename, lineno, offset, options)
          if ast isa Expr && ast.head === :toplevel
            (append!(forms, ast.args); offset = new_offset + 1)
          else
            return (ast, new_offset)
          end
        end
      end
    end
    (Expr(:toplevel, forms...), offset)
  end

lang_parse(text::AbstractString, filename::AbstractString, lineno, offset, options) =
  lang_parse(String(text), String(filename), lineno, offset, options)

jil_parse(text::AbstractString, filename::AbstractString, lineno, offset, options) =
  jil_parse(String(text), String(filename), lineno, offset, options)

julia_jil_parse(text::AbstractString, filename::AbstractString, lineno, offset, options) =
  julia_jil_parse(String(text), String(filename), lineno, offset, options)

# To save/restore the Julia parser
const julia_parser = getglobal(Core, :_parse)

install_jil_parser() = begin
    Core._setparser!(lang_parse)
    :jil
end

restore_julia_parser() = begin
    Core._setparser!(julia_parser)
    :julia
end
#restore_julia_parser()
