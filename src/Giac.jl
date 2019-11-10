__precompile__()
module Giac

using Libdl
using SpecialFunctions

import Base: string, show, write, collect 
import Base: diff, sum, zeros, length, size, getindex
import Base: +, -, (*), /, ^, ==, >, <, >=, <=
import Base: convert, one, oneunit, zero, inv
import Base: round, floor, ceil, trunc
import Base: real, imag, conj, adjoint, abs, sign
import Base: sqrt, exp, log, log10, sin, cos, tan, sec, csc, cot
import Base: sinh, cosh, tanh, asin, acos, atan, acot, asec, acsc
import Base: asinh, acosh, atanh, expm1, log1p
import Base: factorial, binomial, numerator, denominator
import SpecialFunctions: gamma, beta, zeta, besselj, bessely, erfc, erf
import SpecialFunctions: airyai, airybi, sinint, cosint, polygamma, digamma


export @giac, @vars, giac, giac_identifier, type, subtype, isgiacnumber, isgiacreal
export evaluate, evaluatef, evalf, simplify, expand, to_julia, set!, purge!, giac_vars
export unapply, latex, prettyprint, head, args, lambdify 
export infinity, plus_inf, minus_inf

export partfrac, subst, left, right
export ⩦, equal
export Ei, Psi
export integrate, limit, series, curl, grad, divergence, hessian
export preval, sum_riemann, taylor
export solve, cSolve, cZeros, fSolve, deSolve, linsolve 
export logcollect, powexpand,texpand, exp2pow, pow2exp, exp2trig, expexpand
export asin2acos, asin2atan, acos2asin, acos2atan
export sin2costan, cos2sintan
export atan2asin, atan2acos, tan2sincos, halftan
export trigsin, trigcos, trigtan, atrig2log, tlin, tcollect, trigexpand
export trig2exp
export gbasis, greduce, factor



mutable struct giac
    g::Ptr{Nothing}
    function giac(g::Ptr{Nothing})
        gg = new(g)
        finalizer(_delete, gg)
        gg
    end
end


const libgiac_c = Ref{Ptr{Cvoid}}(0)
const context_ptr = Ref{Ptr{Cvoid}}()
const giac_undef = Ref{giac}()
const giac_one = Ref{giac}()
const giac_zero = Ref{giac}()
const giac_pi = Ref{giac}()
const giac_e = Ref{giac}()

function __init__()
    libgiac_c[] = try
        dlopen(joinpath("/usr","lib","libgiac.$dlext"))
    catch
        dlopen(joinpath(@__DIR__, "..", "deps", "lib","libgiac_c.$dlext"))
    end
    context_ptr[] = ccall(dlsym(libgiac_c[], "giac_context_ptr"), Ptr{Nothing}, () )
    giac_undef[] = giac("undef") 
    global infinity = giac("infinity")
    global plus_inf = giac("plus_inf")
    global minus_inf = giac("minus_inf")
    giac_e[] = giac("e")
    giac_pi[] = giac("pi")
    giac_one[] = giac(1)
    giac_zero[] = giac(0)
end

const config_vars = (:Digits, :epsilon, :approx_mode, :complex_mode, :complex_variables)

const giac_julia_dictionay = Dict(
    :ln      => :log, 
    :Gamma   => :gamma,    
    :Beta    => :beta,
    :BesselJ => :besselj,
    :BesselY => :bessely,
    :Si      => :sinint,
    :Ci      => :cosint,
    :re      => :real,
    :im      => :imag,
    )

function type(g::giac) 
    t = unsafe_load(Ptr{UInt8}(g.g), 1) & 31 
    if t<0 || t>21
        return :UNKNOWN
    end
    [
    # from giac/dispatch.h:  
    :INT,        #  0 # int val
    :DOUBLE,     #  1 # double _DOUBLE_val
    :ZINT,       #  2 # mpz_t * _ZINTptr
    :REAL,       #  3 # mpf_t * _REALptr
    :CPLX,       #  4 # gen * _CPLXptr
    :POLY,       #  5 # polynome * _POLYptr
    :IDNT,       #  6 # identificateur * _IDNTptr
    :VECT,       #  7 # vecteur * _VECTptr
    :SYMB,       #  8 # symbolic * _SYMBptr
    :SPOL1,      #  9 # sparse_poly1 * _SPOL1ptr
    :FRAC,       # 10 # fraction * _FRACptr
    :EXT,        # 11 # gen * _EXTptr
    :STRNG,      # 12 # string * _STRNGptr
    :FUNC,       # 13 # unary_fonction_ptr * _FUNCptr
    :ROOT,       # 14 # real_complex_rootof *_ROOTptr
    :MOD,        # 15 # gen * _MODptr
    :USER,       # 16 # giac_user * _USERptr
    :MAP,        # 17 # map<gen.gen> * _MAPptr
    :EQW,        # 18 # eqwdata * _EQWptr
    :GROB,       # 19 # grob * _GROBptr
    :POINTER,    # 20 # void * _POINTER_val
    :FLOAT,      # 21 # immediate, _FLOAT_val
    ][t+1]
end

isgiacreal(g::giac) = type(g) in [:INT, :DOUBLE, :ZINT, :REAL, :FLOAT]
isgiacnumber(g::giac)  = type(g) in [:INT, :DOUBLE, :ZINT, :REAL, :FLOAT, :CPLX]

subtype(g::giac) = unsafe_load(Ptr{UInt8}(g.g), 2) & 31


function change_subtype(g::giac, subtype::Integer)
    ccall( dlsym(libgiac_c[], "giac_change_subtype"), Nothing, (Ptr{Nothing},Cint), g.g, subtype)
end


function _delete(g::giac)
    ccall( dlsym(libgiac_c[], "giac_delete"), Nothing, (Ptr{Nothing},), g.g)
end

giac(x) = giac_undef[]

function giac(g::giac)
    giac(ccall(dlsym(libgiac_c[], "giac_copy_gen"), Ptr{Nothing}, (Ptr{Nothing},), g.g))
end   

function giac(val::Bool)
    g = giac(ccall(dlsym(libgiac_c[], "giac_new_int"), Ptr{Nothing}, (Cint,), val ? 1 : 0))
    change_subtype(g, 6)
    g
end    

function giac(val::Cint)
    giac(ccall(dlsym(libgiac_c[], "giac_new_int"), Ptr{Nothing}, (Cint,), val))
end

function giac(val::Int64)
    giac(ccall(dlsym(libgiac_c[], "giac_new_int64_t"), Ptr{Nothing}, (Int64,), val))
end

function giac(val::BigInt)
    giac(ccall(dlsym(libgiac_c[], "giac_new_bigint"), Ptr{Nothing}, (Ref{BigInt},), val))
end

function giac(val::Rational)
    giac(ccall(dlsym(libgiac_c[], "giac_new_rational"), Ptr{Nothing}, (Ptr{Nothing},Ptr{Nothing}), giac(val.num).g, giac(val.den).g))
end

function giac(val::Int128)
    giac(ccall(dlsym(libgiac_c[], "giac_new_int128_t"), Ptr{Nothing}, (Int128,), val))
end

function giac(val::Cdouble)
    giac(ccall(dlsym(libgiac_c[], "giac_new_double"), Ptr{Nothing}, (Cdouble,), val))
end

function giac(val::BigFloat)
    giac(ccall(dlsym(libgiac_c[], "giac_new_bigfloat"), Ptr{Nothing}, (Ref{BigFloat},), val))
end


function giac(val::Complex)
    giac(ccall(dlsym(libgiac_c[], "giac_new_complex"), Ptr{Nothing}, (Ptr{Nothing},Ptr{Nothing}), giac(real(val)).g, giac(imag(val)).g))
end


function giac(val::Complex{Cint})  
    giac(ccall(dlsym(libgiac_c[], "giac_new_complex_int"), Ptr{Nothing}, (Cint, Cint), real(val), real(val)))
end


function giac(val::Complex{Cdouble})  
    giac(ccall(dlsym(libgiac_c[], "giac_new_complex_double"), Ptr{Nothing}, (Cdouble, Cdouble), real(val), real(val)))
end

function giac_identifier(s::AbstractString)
    giac(ccall(dlsym(libgiac_c[], "giac_new_ident"), Ptr{Nothing}, (Cstring,), s))
end


function giac(s::AbstractString)
    giac(ccall(dlsym(libgiac_c[], "giac_new_symbolic"), Ptr{Nothing}, (Cstring,Ptr{Nothing}), s, context_ptr[]))
end

function giac(v::Array{T,1}; subtype::Integer=0) where T
    v1 = Ptr{Nothing}[giac(i).g for i in v]
    giac(ccall(dlsym(libgiac_c[], "giac_new_vector"), Ptr{Nothing}, 
              (Ptr{Ptr{Nothing}},Cint,Cshort), 
               v1, length(v1), subtype))
end

function giac(A::Array{T,2}) where T
    giac([reshape(A[i,:], size(A,2)) for i in 1:size(A,1)])
end    

giac(x::AbstractRange{T}) where T = giac(collect(x))

giac(::Irrational{:ℯ}) = giac_e[]
giac(::Irrational{:π}) = giac_pi[]

one(::Type{giac}) = giac_one[]
one(::giac) = giac_one[]
oneunit(::Type{giac}) = giac_one[]
oneunit(::giac) = giac_one[]
zero(::Type{giac}) = giac_zero[]
zero(::giac) = giac_zero[]

const _r_giac_julia = Regex(string("\\b(",join(keys(giac_julia_dictionay),"|"),")\\("))
_f_giac_julia(x) = string(giac_julia_dictionay[Symbol(x[1:end-1])], x[end])

function string(g::giac)
   cs = ccall(dlsym(libgiac_c[], "giac_to_string"), Ptr{UInt8}, (Ptr{Nothing},Ptr{Nothing}), g.g, context_ptr[]) 
   s = unsafe_string(cs)
   ccall(dlsym(libgiac_c[], "giac_free"), Nothing, (Ptr{Nothing},), cs)
   replace(s, _r_giac_julia => _f_giac_julia)
end   

function latex(g::giac)
   cs = ccall(dlsym(libgiac_c[], "giac_to_latex"), Ptr{UInt8}, (Ptr{Nothing},Ptr{Nothing}), g.g, context_ptr[]) 
   s = unsafe_string(cs)
   ccall(dlsym(libgiac_c[], "giac_free"), Nothing, (Ptr{Nothing},), cs)
   s
end

prettyprint(ex::giac) = display("text/latex", string("\$\$",latex(ex),"\$\$"))

#show(io::IO, g::giac) = print(io, string(g))
const giac_color_on = Base.text_colors[:light_blue]
const giac_color_off = Base.text_colors[:normal]
show(io::IO, g::giac) = print(io, giac_color_on, string(g), giac_color_off)
#show(io::IO, g::giac) = print(io, "giac(\"", string(g), "\")")


#function writemime(io::IO, mime::MIME"text/latex", ex::giac) 
#    write(io, "\$\$", latex(ex), "\$\$")
#end

   
function +(a::giac, b::giac)
    giac(ccall(dlsym(libgiac_c[], "giac_plus"), Ptr{Nothing}, (Ptr{Nothing},Ptr{Nothing}), a.g, b.g))
end   
+(a::giac, b::Number) = a+giac(b)
+(a::Number, b::giac) = giac(a)+b
+(a::giac) = a

function -(a::giac, b::giac)
    giac(ccall(dlsym(libgiac_c[], "giac_minus"), Ptr{Nothing}, (Ptr{Nothing},Ptr{Nothing}), a.g, b.g))
end   
-(a::giac, b::Number) = a-giac(b)
-(a::Number, b::giac) = giac(a)-b

function -(a::giac)
    giac(ccall(dlsym(libgiac_c[], "giac_uminus"), Ptr{Nothing}, (Ptr{Nothing},), a.g))
end   

function *(a::giac, b::giac)
    giac(ccall(dlsym(libgiac_c[], "giac_times"), Ptr{Nothing}, (Ptr{Nothing},Ptr{Nothing}), a.g, b.g))
end   
*(a::giac, b::Number) = a*giac(b)
*(a::Number, b::giac) = giac(a)*b

function /(a::giac, b::giac)
    giac(ccall(dlsym(libgiac_c[], "giac_rdiv"), Ptr{Nothing}, (Ptr{Nothing},Ptr{Nothing}), a.g, b.g))
end   
/(a::giac, b::Number) = a/giac(b)
/(a::Number, b::giac) = giac(a)/b
inv(a::giac) = giac_one[]/a

function ^(a::giac, b::giac)
    giac(ccall(dlsym(libgiac_c[], "giac_pow"), Ptr{Nothing}, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing}), a.g, b.g, context_ptr[]))
end   
^(a::giac, b::Integer) = a^giac(b)  # to ovverride ^(::Any, Integer) which is already defined
^(a::giac, b::Number) = a^giac(b)
^(a::Number, b::giac) = giac(a)^b

function ==(a::giac, b::giac)
    ccall(dlsym(libgiac_c[], "giac_equal_bool"), Cint, (Ptr{Nothing},Ptr{Nothing}), a.g, b.g)!=0
end   
==(a::giac, b::Number) = a==giac(b)
==(a::Number, b::giac) = giac(a)==b

function >(a::giac, b::giac)
    ccall(dlsym(libgiac_c[], "giac_greater_than"), Cint, (Ptr{Nothing},Ptr{Nothing}), a.g, b.g) !=0
end   

function >=(a::giac, b::giac)
    ccall(dlsym(libgiac_c[], "giac_greater_equal"), Cint, (Ptr{Nothing},Ptr{Nothing}), a.g, b.g) !=0
end   

>(a::giac, b::Real) = a>giac(b)
>(a::Real, b::giac) = giac(a)>b
>=(a::giac, b::Real) = a>=giac(b)
>=(a::Real, b::giac) = giac(a)>=b
<(a::giac, b::giac) = b>a
<(a::Real, b::giac) = giac(a)<b
<(a::giac, b::Real) = a<giac(b)
<=(a::giac, b::giac) = b>=a
<=(a::giac, b::Real) = a<=giac(b)
<=(a::Real, b::giac) = giac(a)<=b

function size(g::giac)
    ccall(dlsym(libgiac_c[], "giac_size1"), Cint, (Ptr{Nothing},), g.g)
end

length(g::giac) = size(g) # 

function getindex(g::giac, i)
    giac(ccall(dlsym(libgiac_c[], "giac_getindex"), Ptr{Nothing}, (Ptr{Nothing},Cint), g.g, i-1))
end

function (g::giac)(x)
    giac(ccall(dlsym(libgiac_c[], "giac_call"), Ptr{Nothing}, 
    (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing}), g.g, giac(x).g, context_ptr[]))
end


function (g::giac)(x...)
    giac(ccall(dlsym(libgiac_c[], "giac_call"), Ptr{Nothing}, 
    (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing}), g.g, giac([x...],subtype=1).g, context_ptr[]))
end




# unary functions with context_ptr:
for F in (:real, :imag, :conj, :abs,
          :sqrt, :exp, :log, :sin, :cos, :tan,
          :sinh, :cosh, :tanh, :asin, :acos, :atan,
          :asinh, :acosh, :atanh)
   @eval begin
       function ($F)(a::giac)
           giac(ccall(dlsym(libgiac_c[], $(string("giac_",F))), Ptr{Nothing}, (Ptr{Nothing},Ptr{Nothing}), a.g, context_ptr[]))
       end   
   end
end

adjoint(x::giac) = conj(x)


function giac(f::Symbol, arg)
    giac(ccall(dlsym(libgiac_c[], string("giac_", f)), Ptr{Nothing}, (Ptr{Nothing},Ptr{Nothing}), giac(arg).g, context_ptr[]))
end   

giac(f::Symbol, args...) = giac(f, giac(Any[args...], subtype=1))

include("library.jl")


unapply(ex::giac,var) = giac(:unapply, ex, var)
set!(var::giac, val) = giac(:sto, val, var)
purge!(var::giac) = giac(:purge, var)
giac_vars() = [Pair(left(x), right(x)) for x in to_julia(giac(:VARS, 1))] 

#This magic code inspired by SymPy, cf.
#https://github.com/jverzani/SymPy.jl/blob/master/src/utils.jl
macro giac(x...) 
    q=Expr(:block)
    r=Expr(:tuple)
    if length(x) == 1 && isa(x[1],Expr)
        if (x[1].head === :tuple)
            x = x[1].args
        end    
    end
    for s in x
        if isa(s,Symbol)
            push!(q.args, Expr(:(=), s, Expr(:call, :giac, Expr(:quote, string(s)))))
            # Use here :giac (:giac_identifier should also work, but this leads to 
            # strange behavior...
            if length(x)>1
                push!(r.args, s)
            end    
        elseif isa(s, Expr)&&s.head==:(=)&&isa(s.args[1],Symbol)
            push!(q.args, Expr(:(=), s.args[1], Expr(:call, :giac, 
                  Expr(:quote, string(s.args[1])))))
            if s.args[1] in config_vars
                push!(q.args, Expr(:call, s.args[1], s.args[2]))
            else
                push!(q.args, Expr(:call, :set!, s.args[1], s.args[2]))
            end    
            if length(x)>1
                push!(r.args, s.args[2])
            end    
        elseif (isa(s, Expr)&&s.head==:(=)&&isa(s.args[1], Expr)
              &&s.args[1].head==:(call)&&isa(s.args[1].args[1], Symbol))
            push!(q.args, Expr(:(=), s.args[1].args[1], 
                 Expr(:call, :giac, Expr(:quote, string(s.args[1].args[1])))))
            if length(s.args[1].args)>2
                push!(q.args, Expr(:call, :set!, s.args[1].args[1], Expr(:call, :unapply, s.args[2], 
                      Expr(:vect, s.args[1].args[2:end]...))))
            else
                push!(q.args, Expr(:call, :set!, s.args[1].args[1], Expr(:call, :unapply, s.args[2], 
                      s.args[1].args[2])))
            end
            if length(x)>1
                push!(r.args, s.args[1].args[2])
            end
        else
            @assert false "@giac expected a list of symbols or var=val expressions"
        end
    end
    if length(x)>1
        push!(q.args, r)
    end    
    Core.eval(Main, q)
end


@eval const $(Symbol("@vars")) = $(Symbol("@giac"))


to_julia(x) = x

function to_julia(g::giac)
  T = type(g)
  if T == :INT 

   z = ccall(dlsym(libgiac_c[], "giac_get_int"), Cint, (Ptr{Nothing},), g.g)
   if subtype(g)==6 #Bool
       return z==1
   else
       return z
   end    

  elseif T == :DOUBLE || T == :FLOAT
    return ccall(dlsym(libgiac_c[], "giac_get_double"), Cdouble, (Ptr{Nothing},), g.g)

  elseif T == :ZINT
    z = BigInt()
    m = ccall(dlsym(libgiac_c[], "giac_get_bigint"), Ptr{BigInt}, (Ptr{Nothing},), g.g)
    ccall((:__gmpz_set,:libgmp), Nothing, (Ref{BigInt}, Ptr{BigInt}), z, m)
    return z

  elseif T == :REAL
    z = BigFloat()
    m = ccall(dlsym(libgiac_c[], "giac_get_bigfloat"), Ptr{BigFloat}, (Ptr{Nothing},), g.g)
    ccall((:mpfr_set, :libmpfr), Int32, (Ref{BigFloat}, Ptr{BigFloat}, Int32),
          z, m, 0)
    return z      

  elseif T == :CPLX
    return complex(to_julia(real(g)), to_julia(imag(g))) 

  elseif T == :FRAC
    return to_julia(numerator(g))//to_julia(denominator(g))

  elseif T == :VECT
    return [to_julia(g[i]) for i=1:length(g)]
  else
    return g
  end    
end

convert(::Type{giac}, x) = giac(x)
convert(::Type{giac}, x::giac) = x # to avoid some ambiguities

function convert(::Type{T}, ex::giac) where T<:Number
    if isgiacnumber(ex)
        return convert(T,to_julia(ex))
    end
    ex = evaluate(ex)
    if isgiacnumber(ex)
       return convert(T,to_julia(ex))
    end   
    ex = evaluatef(ex)
    if isgiacnumber(ex)
       return convert(T,to_julia(ex))
    end
    error("Could not convert giac to Number")
end   


_head(ex::giac) = Symbol(string(head(ex))[2:end-1])

function _args(ex)
    a = args(ex)
    if isa(a, giac) && type(a)==:VECT && subtype(a)==1
        return to_julia(a)
    else
        return Any[to_julia(a)]
    end
end

_expr(x) = x

function _expr(ex::giac)
  T = type(ex)
  if T == :IDNT
    return Symbol(string(ex))
  elseif  T == :SYMB
    h = _head(ex)
    if h==:id
        return _expr(args(ex))
    end    
    if h==:of
        ex = evaluate(ex)
        h = _head(ex)
        @assert h!=:of "could not evaluate user-defined function"
    end
    h = get(giac_julia_dictionay, h, h)
    return Expr(:call, h, [_expr(arg) for arg in _args(ex)]...)
  else
    return ex
  end
end

"""
    lambdify(ex, vars...)

Take a symbolic expression and return an anonymous `Julia` function

"""    
function lambdify(ex::giac, var::giac, vars...) 
    @assert type(ex)==:SYMB && type(var)==:IDNT
    if length(vars) == 0
        eval(Expr(:->, _expr(var), _expr(evaluate(ex))))
    else
        eval(Expr(:->, Expr(:tuple, _expr(var), 
            [_expr(v) for v in vars]...), _expr(evaluate(ex))))
    end
end


function convert(::Type{Function}, f::giac) 
    if type(f)==:SYMB || type(f)==:IDNT
         a = args(evaluate(f))
         return lambdify(to_julia(a[3]),to_julia(a[1])...)
    else
         error("Could not convert giac to Function")
    end
end   

end # module
