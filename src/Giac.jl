__precompile__()
module Giac

using Libdl

import Base: string, show, write, expand, collect 
import Base: diff, sum, zeros, length, size, getindex, endof
import Base: +, -, (*), /, ^, ==, >, <, >=, <=
import Base: ctranspose, convert, one, zero
import Base: round, floor, ceil, trunc
import Base: real, imag, conj, abs, sign
import Base: sqrt, exp, log, log10, sin, cos, tan, sec, csc, cot
import Base: sinh, cosh, tanh, asin, acos, atan, acot, asec, acsc
import Base: asinh, acosh, atanh, expm1, log1p
import Base: factorial, binomial, numerator, denominator
import SpecialFunctions: gamma, beta, zeta, besselj, bessely, erfc, erf
import SpecialFunctions: airyai, airybi

export @giac, giac, giac_identifier
export evaluate, evaluatef, evalf, simplify, to_julia, set!, purge!, giac_vars
export unapply, latex, prettyprint, head, args 
export infinity, plus_inf, minus_inf, undef

export partfrac, subst, left, right
export ⩦, equal
export Ei, Si, Ci
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


abstract type giac end

const libgiac_c = Ref{Ptr{Cvoid}}(0)
const context_ptr = Ref{Ptr{Cvoid}}()
const giac_undef = Ref{giac}()
const giac_one = Ref{giac}()
const giac_zero = Ref{giac}()
const giac_pi = Ref{giac}()
const giac_e = Ref{giac}()

function __init__()
    libgiac_c[] = dlopen(joinpath(dirname(@__FILE__), "..", "deps", "lib",
                         string("libgiac_c.", dlext)))
    context_ptr[] = ccall(dlsym(libgiac_c[], "giac_context_ptr"), Ptr{Nothing}, () )
    giac_undef[] = giac("undef") 
    global undef = giac_undef[]
    global infinity = giac("infinity")
    global plus_inf = giac("plus_inf")
    global minus_inf = giac("minus_inf")
    giac_e[] = giac("e")
    giac_pi[] = giac("pi")
    giac_one[] = giac(1)
    giac_zero[] = giac(0)
end


# from giac/dispatch.h:  
@enum( giac_type, 
    _INT_= 0,  # int val
    _DOUBLE_= 1, # double _DOUBLE_val
    _ZINT= 2, # mpz_t * _ZINTptr
    _REAL= 3, # mpf_t * _REALptr
    _CPLX= 4, # gen * _CPLXptr
    _POLY= 5, # polynome * _POLYptr
    _IDNT= 6, # identificateur * _IDNTptr
    _VECT= 7, # vecteur * _VECTptr
    _SYMB= 8, # symbolic * _SYMBptr
    _SPOL1= 9, # sparse_poly1 * _SPOL1ptr
    _FRAC= 10, # fraction * _FRACptr
    _EXT= 11, # gen * _EXTptr
    _STRNG= 12, # string * _STRNGptr
    _FUNC= 13, # unary_fonction_ptr * _FUNCptr
    _ROOT= 14, # real_complex_rootof *_ROOTptr
    _MOD= 15, # gen * _MODptr
    _USER= 16, # giac_user * _USERptr
    _MAP=17, # map<gen.gen> * _MAPptr
    _EQW=18, # eqwdata * _EQWptr
    _GROB=19, # grob * _GROBptr
    _POINTER_=20, # void * _POINTER_val
    _FLOAT_=21 # immediate, _FLOAT_val
)

config_vars = (:Digits, :epsilon)

mutable struct giac_INT_ <: giac
    g::Ptr{Nothing}
end

mutable struct giac_DOUBLE_ <: giac
    g::Ptr{Nothing}
end

mutable struct giac_ZINT <: giac
    g::Ptr{Nothing}
end

mutable struct giac_REAL  <: giac
    g::Ptr{Nothing}
end

mutable struct giac_CPLX <: giac
    g::Ptr{Nothing}
end

mutable struct giac_POLY <: giac
    g::Ptr{Nothing}
end

mutable struct giac_IDNT <: giac
    g::Ptr{Nothing}
end

mutable struct giac_VECT <: giac
    g::Ptr{Nothing}
end

mutable struct giac_SYMB <: giac
    g::Ptr{Nothing}
end

mutable struct giac_SPOL1 <: giac
    g::Ptr{Nothing}
end

mutable struct giac_FRAC <: giac
    g::Ptr{Nothing}
end

mutable struct giac_EXT <: giac
    g::Ptr{Nothing}
end

mutable struct giac_STRNG <: giac
    g::Ptr{Nothing}
end

mutable struct giac_FUNC <: giac
    g::Ptr{Nothing}
end

mutable struct giac_ROOT <: giac
    g::Ptr{Nothing}
end

mutable struct giac_MOD <: giac
    g::Ptr{Nothing}
end

mutable struct giac_USER <: giac
    g::Ptr{Nothing}
end

mutable struct giac_MAP <: giac
    g::Ptr{Nothing}
end

mutable struct giac_EQW <: giac
    g::Ptr{Nothing}
end

mutable struct giac_GROB <: giac
    g::Ptr{Nothing}
end

mutable struct giac_POINTER_ <: giac
    g::Ptr{Nothing}
end

mutable struct giac_FLOAT_ <: giac
    g::Ptr{Nothing}
end

giacReal = Union{giac_INT_,giac_DOUBLE_,giac_ZINT, giac_REAL, giac_FLOAT_}
giacNumber = Union{giac_INT_,giac_DOUBLE_,giac_ZINT, giac_REAL, giac_FLOAT_, giac_CPLX}

function _gen(g::Ptr{Nothing})
   t = unsafe_load(Ptr{UInt8}(g), 1) & 31
   if t==Int( _INT_ )
       gg = giac_INT_(g)
   elseif t==Int( _DOUBLE_ )   
       gg = giac_DOUBLE_(g)
   elseif t==Int( _ZINT )
       gg = giac_ZINT(g)
   elseif t==Int( _REAL )
       gg = giac_REAL(g)
   elseif t==Int( _CPLX )
       gg = giac_CPLX(g)
   elseif t==Int( _POLY )
       gg = giac_POLY(g)
   elseif t==Int( _IDNT )
       gg = giac_IDNT(g)
   elseif t==Int( _VECT )
       gg = giac_VECT(g)
   elseif t==Int( _SYMB )
       gg = giac_SYMB(g)
   elseif t==Int( _SPOL1 )
       gg = giac_SPOL1(g)
   elseif t==Int( _FRAC )
       gg = giac_FRAC(g)
   elseif t==Int( _EXT )
       gg = giac_EXT(g)
   elseif t==Int( _STRNG )
       gg = giac_STRNG(g)
   elseif t==Int( _FUNC )
       gg = giac_FUNC(g)
   elseif t==Int( _ROOT )
       gg = giac_ROOT(g)
   elseif t==Int( _MOD )
       gg = giac_MOD(g)
   elseif t==Int( _USER )
       gg = giac_USER(g)
   elseif t==Int( _MAP )
       gg = giac_MAP(g)
   elseif t==Int( _EQW )
       gg = giac_EQW(g)
   elseif t==Int( _GROB )
       gg = giac_GROB(g)
   elseif t==Int( _POINTER )
       gg = giac_POINTER(g)
   elseif t==Int( _FLOAT_ )
       gg = giac_FLOAT_(g)
   else    
       @assert false "unknown giac_type"
   end
   finalizer(_delete, gg)
   gg
end


subtype(g::giac) = unsafe_load(Ptr{UInt8}(g.g), 2) 

function change_subtype(g::giac, subtype::Integer)
    ccall( dlsym(libgiac_c[], "giac_change_subtype"), Nothing, (Ptr{Nothing},Cint), g.g, subtype)
end


function _delete(g::giac)
    ccall( dlsym(libgiac_c[], "giac_delete"), Nothing, (Ptr{Nothing},), g.g)
end

giac(x) = giac_undef[]

function giac(g::giac)
    _gen(ccall(dlsym(libgiac_c[], "giac_copy_gen"), Ptr{Nothing}, (Ptr{Nothing},), g.g))
end   

function giac(val::Bool)
    g = _gen(ccall(dlsym(libgiac_c[], "giac_new_int"), Ptr{Nothing}, (Cint,), val ? 1 : 0))
    change_subtype(g, 6)
    g
end    

function giac(val::Cint)
    _gen(ccall(dlsym(libgiac_c[], "giac_new_int"), Ptr{Nothing}, (Cint,), val))
end

function giac(val::Int64)
    _gen(ccall(dlsym(libgiac_c[], "giac_new_int64_t"), Ptr{Nothing}, (Int64,), val))
end

function giac(val::BigInt)
    _gen(ccall(dlsym(libgiac_c[], "giac_new_bigint"), Ptr{Nothing}, (Ref{BigInt},), val))
end

function giac(val::Rational)
    _gen(ccall(dlsym(libgiac_c[], "giac_new_rational"), Ptr{Nothing}, (Ptr{Nothing},Ptr{Nothing}), giac(val.num).g, giac(val.den).g))
end

#does not seem to work properly
#function giac(val::Int128)
#    _gen(ccall(dlsym(libgiac_c[], "giac_new_int128_t"), Ptr{Nothing}, (Int128,), val))
#end

function giac(val::Cdouble)
    _gen(ccall(dlsym(libgiac_c[], "giac_new_double"), Ptr{Nothing}, (Cdouble,), val))
end

function giac(val::BigFloat)
    _gen(ccall(dlsym(libgiac_c[], "giac_new_bigfloat"), Ptr{Nothing}, (Ref{BigFloat},), val))
end


function giac(val::Complex)
    _gen(ccall(dlsym(libgiac_c[], "giac_new_complex"), Ptr{Nothing}, (Ptr{Nothing},Ptr{Nothing}), giac(real(val)).g, giac(imag(val)).g))
end


function giac(val::Complex{Cint})  
    _gen(ccall(dlsym(libgiac_c[], "giac_new_complex_int"), Ptr{Nothing}, (Cint, Cint), real(val), real(val)))
end


function giac(val::Complex{Cdouble})  
    _gen(ccall(dlsym(libgiac_c[], "giac_new_complex_double"), Ptr{Nothing}, (Cdouble, Cdouble), real(val), real(val)))
end

function giac_identifier(s::AbstractString)
    _gen(ccall(dlsym(libgiac_c[], "giac_new_ident"), Ptr{Nothing}, (Cstring,), s))
end


function giac(s::AbstractString)
    _gen(ccall(dlsym(libgiac_c[], "giac_new_symbolic"), Ptr{Nothing}, (Cstring,Ptr{Nothing}), s, context_ptr[]))
end

function giac(v::Array{T,1}; subtype::Integer=0) where T
    v1 = Ptr{Nothing}[giac(i).g for i in v]
    _gen(ccall(dlsym(libgiac_c[], "giac_new_vector"), Ptr{Nothing}, 
              (Ptr{Ptr{Nothing}},Cint,Cshort), 
               v1, length(v1), subtype))
end

function giac(A::Array{T,2}) where T
    giac([reshape(A[i,:], size(A,2)) for i in 1:size(A,1)])
end    

giac(x::AbstractRange{T}) where T = giac(collect(x))

giac(::Irrational{:ℯ}) = giac_e[]
giac(::Irrational{:π}) = giac_pi[]

one(::Type{T}) where T<:giac = giac_one[]
one(::giac) = one(giac)
zero(::Type{T}) where T<:giac = giac_zero[]
zero(::giac) = zero(giac)



function string(g::giac)
    cs = ccall(dlsym(libgiac_c[], "giac_to_string"), Ptr{UInt8}, (Ptr{Nothing},Ptr{Nothing}), g.g, context_ptr[]) 
   s = unsafe_string(cs)
   ccall(dlsym(libgiac_c[], "giac_free"), Nothing, (Ptr{Nothing},), cs)
   s
end

function latex(g::giac)
    cs = ccall(dlsym(libgiac_c[], "giac_to_latex"), Ptr{UInt8}, (Ptr{Nothing},Ptr{Nothing}), g.g, context_ptr[]) 
   s = unsafe_string(cs)
   ccall(dlsym(libgiac_c[], "giac_free"), Nothing, (Ptr{Nothing},), cs)
   s
end

prettyprint(ex::giac) = display("text/latex", string("\$\$",latex(ex),"\$\$"))

show(io::IO, g::giac) = print(io, string(g))
#show(io::IO, g::giac) = print(io, "giac(\"", string(g), "\")")


#function writemime(io::IO, mime::MIME"text/latex", ex::giac) 
#    write(io, "\$\$", latex(ex), "\$\$")
#end

   
function +(a::giac, b::giac)
    _gen(ccall(dlsym(libgiac_c[], "giac_plus"), Ptr{Nothing}, (Ptr{Nothing},Ptr{Nothing}), a.g, b.g))
end   
+(a::giac, b::Number) = a+giac(b)
+(a::Number, b::giac) = giac(a)+b
+(a::giac) = a

function -(a::giac, b::giac)
    _gen(ccall(dlsym(libgiac_c[], "giac_minus"), Ptr{Nothing}, (Ptr{Nothing},Ptr{Nothing}), a.g, b.g))
end   
-(a::giac, b::Number) = a-giac(b)
-(a::Number, b::giac) = giac(a)-b

function -(a::giac)
    _gen(ccall(dlsym(libgiac_c[], "giac_uminus"), Ptr{Nothing}, (Ptr{Nothing},), a.g))
end   

function *(a::giac, b::giac)
    _gen(ccall(dlsym(libgiac_c[], "giac_times"), Ptr{Nothing}, (Ptr{Nothing},Ptr{Nothing}), a.g, b.g))
end   
*(a::giac, b::Number) = a*giac(b)
*(a::Number, b::giac) = giac(a)*b

function /(a::giac, b::giac)
    _gen(ccall(dlsym(libgiac_c[], "giac_rdiv"), Ptr{Nothing}, (Ptr{Nothing},Ptr{Nothing}), a.g, b.g))
end   
/(a::giac, b::Number) = a/giac(b)
/(a::Number, b::giac) = giac(a)/b

function ^(a::giac, b::giac)
    _gen(ccall(dlsym(libgiac_c[], "giac_pow"), Ptr{Nothing}, (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing}), a.g, b.g, context_ptr[]))
end   
^(a::giac, b::Integer) = a^giac(b)  # to ovverride ^(::Any, Integer) which is already defined
^(a::giac, b::Number) = a^giac(b)
^(a::Number, b::giac) = giac(a)^b

function ==(a::giac, b::giac)
    ccall(dlsym(libgiac_c[], "giac_equal_bool"), Cint, (Ptr{Nothing},Ptr{Nothing}), a.g, b.g)!=0
end   
==(a::giac, b::Number) = a==giac(b)
==(a::Number, b::giac) = giac(a)==b

function >(a::giacReal, b::giacReal)
    ccall(dlsym(libgiac_c[], "giac_greater_than"), Cint, (Ptr{Nothing},Ptr{Nothing}), a.g, b.g)!=0
end   
<(a::giacReal, b::giacReal) = b>a
<=(a::giacReal, b::giacReal) = !(a>b)
>=(a::giacReal, b::giacReal) = !(b>a)
<=(a::giacReal, b::Real) = a<=giac(b)
<=(a::Real, b::giacReal) = giac(a)<=b
>=(a::giacReal, b::Real) = a>=giac(b)
>=(a::Real, b::giacReal) = giac(a)>=b
>(a::giacReal, b::Real) = a>giac(b)
<(a::Real, b::giacReal) = giac(a)<b
<(a::giacReal, b::Real) = a<giac(b)
>(a::Real, b::giacReal) = giac(a)>b

*(x::giac, a::Array) = (y->x*y).(a)
*(a::Array, x::giac) = (y->y*x).(a)
+(x::giac, a::Array) = (y->x+y).(a)
+(a::Array, x::giac) = (y->y+x).(a)
-(x::giac, a::Array) = (y->x-y).(a)
-(a::Array, x::giac) = (y->y-x).(a)

function size(g::giac)
    ccall(dlsym(libgiac_c[], "giac_size1"), Cint, (Ptr{Nothing},), g.g)
end

length(g::giac) = size(g) # 

function getindex(g::giac_VECT, i)
    _gen(ccall(dlsym(libgiac_c[], "giac_getindex"), Ptr{Nothing}, (Ptr{Nothing},Cint), g.g, i-1))
end

#function call(g::giac, x)
function (g::Giac.giac_FUNC)(x)
    _gen(ccall(dlsym(libgiac_c[], "giac_call"), Ptr{Nothing}, 
    (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing}), g.g, giac(x).g, context_ptr[]))
end

function (g::Giac.giac_IDNT)(x)
    _gen(ccall(dlsym(libgiac_c[], "giac_call"), Ptr{Nothing}, 
    (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing}), g.g, giac(x).g, context_ptr[]))
end

function (g::Giac.giac_SYMB)(x)
    _gen(ccall(dlsym(libgiac_c[], "giac_call"), Ptr{Nothing}, 
    (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing}), g.g, giac(x).g, context_ptr[]))
end

#function call(g::giac, x...)
function (g::Giac.giac_FUNC)(x...)
    _gen(ccall(dlsym(libgiac_c[], "giac_call"), Ptr{Nothing}, 
    (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing}), g.g, giac([x...],subtype=1).g, context_ptr[]))
end

function (g::Giac.giac_IDNT)(x...)
    _gen(ccall(dlsym(libgiac_c[], "giac_call"), Ptr{Nothing}, 
    (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing}), g.g, giac([x...],subtype=1).g, context_ptr[]))
end

function (g::Giac.giac_SYMB)(x...)
    _gen(ccall(dlsym(libgiac_c[], "giac_call"), Ptr{Nothing}, 
    (Ptr{Nothing},Ptr{Nothing},Ptr{Nothing}), g.g, giac([x...],subtype=1).g, context_ptr[]))
end



endof(g::giac_VECT) = size(g)

# unary functions with context_ptr:
for F in (:real, :imag, :conj, :abs,
          :sqrt, :exp, :log, :sin, :cos, :tan,
          :sinh, :cosh, :tanh, :asin, :acos, :atan,
          :asinh, :acosh, :atanh)
   @eval begin
       function ($F)(a::giac)
           _gen(ccall(dlsym(libgiac_c[], $(string("giac_",F))), Ptr{Nothing}, (Ptr{Nothing},Ptr{Nothing}), a.g, context_ptr[]))
       end   
   end
end


function giac(f::Symbol, arg)
    _gen(ccall(dlsym(libgiac_c[], string("giac_", f)), Ptr{Nothing}, (Ptr{Nothing},Ptr{Nothing}), giac(arg).g, context_ptr[]))
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


to_julia(g::giac) = giac(g)

function to_julia(g::giac_INT_)
    z = ccall(dlsym(libgiac_c[], "giac_get_int"), Cint, (Ptr{Nothing},), g.g)
   if subtype(g)==6 #Bool
       return z==1
   else
       return z
   end    
end    

to_julia(g::Union{giac_DOUBLE_, giac_FLOAT_}) = 
ccall(dlsym(libgiac_c[], "giac_get_double"), Cdouble, (Ptr{Nothing},), g.g)

function to_julia(g::giac_ZINT)
    z = BigInt()
    m = ccall(dlsym(libgiac_c[], "giac_get_bigint"), Ptr{BigInt}, (Ptr{Nothing},), g.g)
    ccall((:__gmpz_set,:libgmp), Nothing, (Ref{BigInt}, Ptr{BigInt}), z, m)
    z
end

function to_julia(g::giac_REAL)
    z = BigFloat()
    m = ccall(dlsym(libgiac_c[], "giac_get_bigfloat"), Ptr{BigFloat}, (Ptr{Nothing},), g.g)
    ccall((:mpfr_set, :libmpfr), Int32, (Ref{BigFloat}, Ptr{BigFloat}, Int32),
          z, m, 0)
    z      
end

to_julia(g::giac_CPLX) = complex(to_julia(real(g)), to_julia(imag(g))) 

to_julia(g::giac_FRAC) = to_julia(numerator(g))//to_julia(denominator(g))

to_julia(g::giac_VECT) = [to_julia(g[i]) for i=1:length(g)]


_head(ex::giac) = Symbol(string(head(ex))[2:end-1])

function _args(ex)
    a = args(ex)
    if isa(a, giac_VECT)&&subtype(args(ex))==1
        a1 = to_julia(a)
    else
        a1 = Any[to_julia(a)]
    end
end

_expr(x) = x
_expr(ex::giac_IDNT) = Symbol(string(ex))

function _expr(ex::giac_SYMB)
    h = _head(ex)
    if h==:id
        return _expr(args(ex))
    end    
    if h==:of
        ex = evaluate(ex)
        h = _head(ex)
        @assert h!=:of "could not evaluate user-defined function"
    end
    if h==:ln  #giac/ln corresponds to Julia/log 
        h=:log
    elseif h==:Gamma
        h=:gamma
    elseif h==:Beta
        h=:beta
    elseif h==:BesselJ
        h=:besselj
    elseif h==:BesselY
        h=:bessely
    end
    Expr(:call, h, [_expr(arg) for arg in _args(ex)]...)
end

function to_julia(ex::giac_SYMB, var::giac_IDNT, vars...) 
    if length(vars) == 0
        eval(Expr(:->, _expr(var), _expr(evaluate(ex))))
    else
        eval(Expr(:->, Expr(:tuple, _expr(var), 
            [_expr(v) for v in vars]...), _expr(evaluate(ex))))
    end
end

convert(::Type{giac}, x) = giac(x)
convert(::Type{giac}, x::giac) = x # to avoid some ambiguities
convert(::Type{T}, x::giacNumber) where T<:Number = convert(T,to_julia(x))

function convert(::Type{T}, ex::Union{giac_SYMB,giac_IDNT}) where T<:Number
   ex = evaluate(ex)
   if isa(ex, giacNumber)
      return convert(T,to_julia(ex))
   end   
   ex1 = evaluatef(ex)
   if isa(ex1, giacNumber)
        return convert(T,to_julia(ex1))
   end
   error("Could not convert to number")
end   

function Base.convert(::Type{Function}, f::Union{Giac.giac_SYMB,Giac.giac_IDNT}) 
    a = args(evaluate(f))
    to_julia(to_julia(a[3]),to_julia(a[1])...)
end   

end # module
