# To restore the Julia parser
const julia_parser = getglobal(Core, :_parse)

const EOF_CHAR = '\x03' # End of text :-)
const NULL_CHAR = ' '
const DOT = Symbol(".")
const DOT_COMMA = Symbol("")
const OPEN_PARENTHESIS = Symbol("(")
const CLOSE_PARENTHESIS = Symbol(")")
const END_OF_FILE = Symbol("THIS IS THE END OF FILE")

@kwdef mutable struct JiLIO <: IO
    io::IO
    shared::Dict=Dict()
    sharedSyntax::Bool=false
    pushedToken=nothing
    pushedChar::Char=NULL_CHAR
    offset::Int=0
end

pushToken(newToken, io::JiLIO) =
  isnothing(io.pushedToken) ?
    io.pushedToken = newToken :
    error("Token already pushed!!!")

popToken(io::JiLIO) =
  isnothing(io.pushedToken) ?
    error("No token available to pop!!!") :
    let token = io.pushedToken
      io.pushedToken = nothing
      token
    end

pushedTokenP(io::JiLIO) =
  ! isnothing(io.pushedToken)

pushChar(newChar, io::JiLIO) =
  io.pushedChar == NULL_CHAR ?
    io.pushedChar = newChar :
    error("Char already pushed!!!")

popChar(io::JiLIO) =
  io.pushedChar == NULL_CHAR ?
    error("No char available to pop!!!") :
    let ch = io.pushedChar
      io.pushedChar = NULL_CHAR
      ch
    end

pushedCharP(io::JiLIO) =
  io.pushedChar != NULL_CHAR

read(eofValue, io::JiLIO) =
  let token = readToken(io)
    if token == CLOSE_PARENTHESIS
      println("Extra ')' ignored")
      read(nil, io)
    elseif token == OPEN_PARENTHESIS
      readTail(false, io)
    elseif token == END_OF_FILE
      eofValue
    else
      token
    end
  end

readTail(dotOkP, io::JiLIO) =
  let token = readToken(io)
    if token == END_OF_FILE
      error("End of file during read")
    elseif token == CLOSE_PARENTHESIS
      list()
    elseif token == DOT && dotOkP
      let result = read(nil, io),
          newToken = readToken(io)
        if newToken == CLOSE_PARENTHESIS
          result
        else
          error("Where's the ')'? Got $newToken instead!")
        end
      end
    elseif token == DOT
      error("'.' immediately after '('")
    else
      pushToken(token, io)
      let car = read(nil, io),
          cdr = readTail(true, io)
        cons(car, cdr)
      end
    end
  end

Base.eof(io::JiLIO) = eof(io.io)

readChar(io::JiLIO) =
  eof(io.io) ?
    EOF_CHAR :
    let c = Base.read(io.io, Char)
      io.offset += 1
      c
    end


readToken(io::JiLIO) =
  if pushedTokenP(io)
    popToken(io)
  else
    let ch = pushedCharP(io) ? popChar(io) : readChar(io)
      while (whitespaceP(ch))
         ch = readChar(io)
      end
      if ch == '('
        OPEN_PARENTHESIS
      elseif ch == ')'
        CLOSE_PARENTHESIS
      elseif ch == '"'
        readString(io)
      elseif ch == ';'
        readComment(io)
        readToken(io)
      elseif ch == '#'
        readDispatch(io)
      elseif ch == '\''
        list(Symbol("quote"), read(nil, io))
      elseif ch == EOF_CHAR
        END_OF_FILE
      else
        readNumberOrSymbol(ch, io)
      end
    end
  end  

readDispatch(io::JiLIO) =
  let dispatch = readChar(io)
    if dispatch == 'S'
      readStructure()
    elseif dispatch == '('
      let elems = readTail(false, io)
        makeVectorFromList(elems)
      end
    else
      pushChar(dispatch, io)
      let sharedSyntax = true,
          key = readToken(io),
          termChar = popChar(io)
        if termChar == '='
          sharedSyntax = false
          shared = read(nil, JiLIO)
          if !(key in io.sharedTable)
            io.sharedTable[key] = shared
          else
           error("Duplicate read definition name #" + key)
          end
          shared
        elseif termChar == '#'
          sharedSyntax = false
          io.sharedTable[key]
        else
          error("Unknown terminating circle dispatch char #$dispatch$termChar")
        end
      end
    end
  end

  #=
makeVectorFromList(Cons elems)
        size = elems.length()
        Object[] array = new Object[size]
        i = 0
        Cons listE = elems
        for ( ! listE.endp() listE = listE.rest())
            e = listE.first()
            array[i] = e
            ++i
    

        array

=#

readString(io::JiLIO) =
  let buffer = [], 
      ch
    while (!eof(io) && (ch = readChar(io)) != '"')
      push!(buffer, ch)
    end
    string(buffer...)
  end

readComment(io::JiLIO) =
  let ch
    while (!eof(io) && (ch = readChar(io)) != '\n') end
  end

readNumberOrSymbol(ch, io::JiLIO) =
  let c = ch,
      buffer = []
    while (true)
      push!(buffer, ch)
      ch = readChar(io)
      if delimiterP(ch, io)
        break
      end
    end
    pushChar(ch, io)
    if c == '.' && length(buffer) == 1
      DOT
    else
      let token = string(buffer...)
        if c in ".+-0123456789"
          try
            parse(Int, token)
          catch e
            try
              parse(Float64, token)
            catch e
              Symbol(token)
            end
          end
        else
          Symbol(token)
        end
      end
    end
  end


#=


readStructure(io::JiLIO)
        parseStructure((Cons)read(null))



parseStructure(Cons structDescr, io::JiLIO)
        String className = linjNameToJavaTypeName(_package, (Symbol)structDescr.first())
        try
            Class _class = Class.forName(className)
            Method method = _class.getMethod("parse", new Class[] Class.forName("linj.Cons") })
            method.setAccessible(true)
            method.invoke(null, new Cons[] structDescr })
        catch (InvocationTargetException ite)
            ite.getTargetException().printStackTrace()
            error("Couldn't parse the structure #S" + structDescr)
        catch (Exception e)
            System.err.print(e)
            e.printStackTrace()
            error("Couldn't parse the structure #S" + structDescr)

=#

delimiterP(ch, io) =
  if io.sharedSyntax
    ch in "=#"
  elseif ch == EOF_CHAR
    true
  else
    ch in "()'\" \t\n\r"
  end

whitespaceP(ch) =
  ch in " \t\n\r"

restore_julia_parser() = begin
    Core._setparser!(julia_parser)
    :julia
end


looks_like_lisp(text) = 
  startswith(text, r"\(") # this looks like lisp


lisp_read(text) =
  let parser = JiLIO(io=IOBuffer(text))
    try
      let lisp_form = read(nil, parser)
        (lisp_form, parser.offset)
      end
    catch e
      (Expr(:incomplete, "Input error"), parser.offset)
    end
  end
#=
// Parse `text` starting at 0-based `offset` and attributing the content to
// `filename`. Return an svec of (parsed_expr, final_offset)
=#

debug_lisp_to_julia = false

jil_parse(text::Union{Core.SimpleVector,String}, filename::String, lineno, offset, options) =
  if looks_like_lisp(text)
    let (lisp_form, offset) = lisp_read(text),
        julia_form = tojulia(lisp_form)
      if debug_lisp_to_julia
        println(julia_form)
      end
      (Expr(:toplevel, julia_form), offset)
    end
  else
    julia_parser(text, filename, lineno, offset, options)
  end

jil_parse(text::AbstractString, filename::AbstractString, lineno, offset, options) =
  jil_parse(String(text), String(filename), lineno, offset, options)

install_jil_parser() = begin
    Core._setparser!(jil_parse)
    :jil
end

#restore_julia_parser()



