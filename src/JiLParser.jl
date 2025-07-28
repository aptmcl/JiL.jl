
const EOF_CHAR = '\x03' # End of text :-)
const NULL_CHAR = ' '
const DOT = Symbol(".")
const DOT_COMMA = Symbol("")
const OPEN_PARENTHESIS = Symbol("(")
const CLOSE_PARENTHESIS = Symbol(")")
const END_OF_FILE = Symbol("THIS IS THE END OF FILE")

@kwdef mutable struct JiLIO <: IO
    input::String
    shared::Dict{Int, Any}=Dict{Int, Any}()
    sharedSyntax::Bool=false
    pushedToken=nothing
    pushedChar::Char=NULL_CHAR
    offset::Int=1
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
      throw(EOFError())
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
      #error("'.' immediately after '('")
      # Special treatment of (. inst field)
      let car = read(nil, io),
          cdr = readTail(true, io)
        cons(:(.), cons(car, cdr))
      end
    else
      pushToken(token, io)
      let car = read(nil, io),
          cdr = readTail(true, io)
        cons(car, cdr)
      end
    end
  end

Base.eof(io::JiLIO) = io.offset > lastindex(io.input)

readChar(io::JiLIO) =
  eof(io) ?
    EOF_CHAR :
    let c = io.input[io.offset]
      io.offset = nextind(io.input, io.offset)
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
      elseif ch == '`'
        list(Symbol("quasiquote"), read(nil, io))
      elseif ch == ','
        let nextch = readChar(io)
          if nextch == '@'
            list(Symbol("unquote-splicing"), read(nil, io))
          else
            pushChar(nextch, io)
            list(Symbol("unquote"), read(nil, io))
          end
        end
      elseif ch == EOF_CHAR
        END_OF_FILE
      else
        readNumberOrSymbol(ch, io)
      end
    end
  end  

readDispatch(io::JiLIO) =
  let dispatch = readChar(io)
    if dispatch == '\\' # Character
      readChar(io)
    elseif dispatch == 'b' # binary
      readNumberOrSymbol(readChar(io), io, 2)
    elseif dispatch == 'o' # octal
      readNumberOrSymbol(readChar(io), io, 8)
    elseif dispatch == 'd' # decimal
      readNumberOrSymbol(readChar(io), io, 10)
    elseif dispatch == 'x' # hexadecimal
      readNumberOrSymbol(readChar(io), io, 16)
    elseif dispatch == 'e' # exact
      reduce_fraction(rationalize(read(nil, io)))
    elseif dispatch == 'i' # inexact
      read(nil, io) # HACK: This needs to be made inexact
    elseif dispatch == 't' # Just to directly handle #t and #f
      true
    elseif dispatch == 'f'
      false
    elseif dispatch == 'S'
      readStructure()
    elseif dispatch == '(' # Vector
      let elems = readTail(false, io)
        vcat(elems...)
      end
    else
      pushChar(dispatch, io)
      let sharedSyntax = true,
          key = string(readToken(io)),
          termChar = key[end],
          key = parse(Int, key[1:end-1])
        if termChar == '='
          sharedSyntax = false
          shared = read(nil, io)
          if !(haskey(io.shared, key))
            io.shared[key] = shared
          else
           error("Duplicate read definition name #" + key)
          end
          shared
        elseif termChar == '#'
          sharedSyntax = false
          io.shared[key]
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
    while !eof(io) && (ch = readChar(io)) != '"'
      push!(buffer, ch)
    end
    string(buffer...)
  end

readComment(io::JiLIO) =
  let ch
    while (!eof(io) && (ch = readChar(io)) != '\n') end
  end

const complex_rectangular_regex = r"[+-]?(((\d+\.\d*|\d*\.\d+|\d+)[+-])?((\d+\.\d*|\d*\.\d+|\d+)i|i(\d+\.\d*|\d*\.\d+|\d+)|i))"
const complex_polar_regex = r"[+-]?((\d+\.\d*|\d*\.\d+|\d+)?e(\([+-]?|[+-]?\()((\d+\.\d*|\d*\.\d+|\d+)i|i(\d+\.\d*|\d*\.\d+|\d+)|i)\))"
#const float_regex = r"^(([+-]?(?:\d*(?:\.(?=\d)?)?\d*))(?:(?:(?<=\d)[Ee])?((?2)))(?<=\d))?$"
const float_regex = r"^[+-]?((\d+\.\d*)|(\.\d+)|(\d+))([esfdlESFDL][+-]?\d+)?$"
const integer_regex = r"^[+-]?\d+$"
const fraction_regex = r"^([+-]?\d+)/(\d+)$"

const scheme_julia_translations = Dict(
  "true"=>true, "#t"=>true, 
  "false"=>false, "#f"=>false,
  "+inf.0"=>Inf,
  "-inf.0"=>-Inf,
  "+nan.0"=>NaN,
  "-nan.0"=>NaN)

reduce_fraction(f) = denominator(f) == 1 ? numerator(f) : f

readNumberOrSymbol(ch, io::JiLIO, base=10) =
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
      let token = string(buffer...),
          val = get(scheme_julia_translations, token, missing),
          m = nothing
        if ismissing(val)
          if c in ".+-0123456789"
            if !isnothing(match(integer_regex, token))
              parse(Int, token, base=base)
            elseif (m = match(fraction_regex, token); !isnothing(m))
              reduce_fraction(parse(Int, m[1])//parse(Int, m[2]))
            elseif !isnothing(match(float_regex, token))
              parse(Float64, token)
            elseif !isnothing(match(complex_rectangular_regex, token))
              error("Must handle the complex number '$token'. See https://github.com/JuliaLang/julia/issues/22250")
            elseif !isnothing(match(complex_polar_regex, token))
              error("Must handle the complex number '$token'. See https://github.com/JuliaLang/julia/issues/22250")
            else
                Symbol(token)
            end
          else
            Symbol(token)
          end
        else
          val
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



