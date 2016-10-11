__precompile__()
module Giac

import Base: string, show, write, writemime, expand, factor, collect 
import Base: diff, sum, zeros, length, size, getindex, endof, call
import Base: +, -, (*), /, ^, ==, >, <, >=, <=
import Base: ctranspose, convert
import Base: real, imag, conj, abs, sign
import Base: sqrt, exp, log, sin, cos, tan
import Base: sinh, cosh, tanh, asin, acos, atan, acot, acsc
import Base: asinh, acosh, atanh

export @giac, giac, undef, infinity, giac_identifier
export evaluate, evaluatef, evalf, simplify, to_julia, set, purge, giac_vars
export unapply, simplify, plus_inf, minus_inf, latex, pretty_print, head, args 

export partfrac, subst, left, right, denom, numer
export ⩦, equal
export integrate, limit, series, curl, grad, divergence, hessian
export preval, sum_riemann, taylor
export solve, cSolve, cZeros, fSolve, deSolve, linsolve 
export texpand


function __init__()
    global const libgiac = Libdl.dlopen(joinpath(dirname(@__FILE__), "..", "deps", "lib",
                     string("libgiac.", Libdl.dlext)))
    global const libgiac_c = Libdl.dlopen(joinpath(dirname(@__FILE__), "..", "deps", "lib",
                     string("libgiac_c.", Libdl.dlext)))
    global const context_ptr = ccall(Libdl.dlsym(libgiac_c, "giac_context_ptr"), Ptr{Void}, () )
    global const undef = _gen(ccall(Libdl.dlsym(libgiac_c, "giac_undef"), Ptr{Void}, () ))
    global const infinity = giac("infinity")
    global const plus_inf = giac("plus_inf")
    global const minus_inf = giac("minus_inf")
    global const giac_e = giac("e")
    global const giac_pi = giac("pi")
end


abstract giac


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

type giac_INT_ <: giac
    g::Ptr{Void}
end

type giac_DOUBLE_ <: giac
    g::Ptr{Void}
end

type giac_ZINT <: giac
    g::Ptr{Void}
end

type giac_REAL  <: giac
    g::Ptr{Void}
end

type giac_CPLX <: giac
    g::Ptr{Void}
end

type giac_POLY <: giac
    g::Ptr{Void}
end

type giac_IDNT <: giac
    g::Ptr{Void}
end

type giac_VECT <: giac
    g::Ptr{Void}
end

type giac_SYMB <: giac
    g::Ptr{Void}
end

type giac_SPOL1 <: giac
    g::Ptr{Void}
end

type giac_FRAC <: giac
    g::Ptr{Void}
end

type giac_EXT <: giac
    g::Ptr{Void}
end

type giac_STRNG <: giac
    g::Ptr{Void}
end

type giac_FUNC <: giac
    g::Ptr{Void}
end

type giac_ROOT <: giac
    g::Ptr{Void}
end

type giac_MOD <: giac
    g::Ptr{Void}
end

type giac_USER <: giac
    g::Ptr{Void}
end

type giac_MAP <: giac
    g::Ptr{Void}
end

type giac_EQW <: giac
    g::Ptr{Void}
end

type giac_GROB <: giac
    g::Ptr{Void}
end

type giac_POINTER_ <: giac
    g::Ptr{Void}
end

type giac_FLOAT_ <: giac
    g::Ptr{Void}
end

giacReal = Union{giac_INT_,giac_DOUBLE_,giac_ZINT, giac_REAL, giac_FLOAT_}
giacNumber = Union{giac_INT_,giac_DOUBLE_,giac_ZINT, giac_REAL, giac_FLOAT_, giac_CPLX}

function _gen(g::Ptr{Void})
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
   finalizer(gg, _delete)
   gg
end


subtype(g::giac) = unsafe_load(Ptr{UInt8}(g.g), 2) 

function change_subtype(g::giac, subtype::Integer)
    ccall( Libdl.dlsym(libgiac_c, "giac_change_subtype"), Void, (Ptr{Void},Cint), g.g, subtype)
end


function _delete(g::giac)
    ccall( Libdl.dlsym(libgiac_c, "giac_delete"), Void, (Ptr{Void},), g.g)
end

giac(x) = undef

function giac(g::giac)
   _gen(ccall(Libdl.dlsym(libgiac_c, "giac_copy_gen"), Ptr{Void}, (Ptr{Void},), g.g))
end   

function giac(val::Bool)
    g = _gen(ccall(Libdl.dlsym(libgiac_c, "giac_new_int"), Ptr{Void}, (Cint,), val?1:0))
    change_subtype(g, 6)
    g
end    

function giac(val::Cint)
    _gen(ccall(Libdl.dlsym(libgiac_c, "giac_new_int"), Ptr{Void}, (Cint,), val))
end

function giac(val::Int64)
    _gen(ccall(Libdl.dlsym(libgiac_c, "giac_new_int64_t"), Ptr{Void}, (Int64,), val))
end

function giac(val::BigInt)
    _gen(ccall(Libdl.dlsym(libgiac_c, "giac_new_bigint"), Ptr{Void}, (Ptr{BigInt},), &val))
end

function giac(val::Rational)
    _gen(ccall(Libdl.dlsym(libgiac_c, "giac_new_rational"), Ptr{Void}, (Ptr{Void},Ptr{Void}), giac(val.num).g, giac(val.den).g))
end

#does not seem to work properly
#function giac(val::Int128)
#    _gen(ccall(Libdl.dlsym(libgiac_c, "giac_new_int128_t"), Ptr{Void}, (Int128,), val))
#end

function giac(val::Cdouble)
    _gen(ccall(Libdl.dlsym(libgiac_c, "giac_new_double"), Ptr{Void}, (Cdouble,), val))
end

function giac(val::BigFloat)
    _gen(ccall(Libdl.dlsym(libgiac_c, "giac_new_bigfloat"), Ptr{Void}, (Ptr{BigFloat},), &val))
end


function giac(val::Complex)
    _gen(ccall(Libdl.dlsym(libgiac_c, "giac_new_complex"), Ptr{Void}, (Ptr{Void},Ptr{Void}), giac(real(val)).g, giac(imag(val)).g))
end


function giac(val::Complex{Cint})  
    _gen(ccall(Libdl.dlsym(libgiac_c, "giac_new_complex_int"), Ptr{Void}, (Cint, Cint), real(val), real(val)))
end


function giac(val::Complex{Cdouble})  
    _gen(ccall(Libdl.dlsym(libgiac_c, "giac_new_complex_double"), Ptr{Void}, (Cdouble, Cdouble), real(val), real(val)))
end

function giac_identifier(s::ASCIIString)
    _gen(ccall(Libdl.dlsym(libgiac_c, "giac_new_ident"), Ptr{Void}, (Ptr{UInt8},), s))
end


function giac(s::ASCIIString)
    _gen(ccall(Libdl.dlsym(libgiac_c, "giac_new_symbolic"), Ptr{Void}, (Ptr{UInt8},Ptr{Void}), s, context_ptr))
end

function giac{T}(v::Array{T,1}; subtype::Integer=0)
    v1 = Ptr{Void}[giac(i).g for i in v]
    _gen(ccall(Libdl.dlsym(libgiac_c, "giac_new_vector"), Ptr{Void}, 
              (Ptr{Ptr{Void}},Cint,Cshort), 
               v1, length(v1), subtype))
end

function giac{T}(A::Array{T,2})
    giac([reshape(A[i,:], size(A,2)) for i in 1:size(A,1)])
end    

giac{T}(x::Range{T}) = giac(collect(x))

giac(::Irrational{:e}) = giac_e
giac(::Irrational{:π}) = giac_pi



function string(g::giac)
   cs = ccall(Libdl.dlsym(libgiac_c, "giac_to_string"), Ptr{UInt8}, (Ptr{Void},Ptr{Void}), g.g, context_ptr) 
   s = bytestring(cs)
   ccall((:free, "libc"), Void, (Ptr{Void},), cs)
   s
end

function latex(g::giac)
   cs = ccall(Libdl.dlsym(libgiac_c, "giac_to_latex"), Ptr{UInt8}, (Ptr{Void},Ptr{Void}), g.g, context_ptr) 
   s = bytestring(cs)
   ccall((:free, "libc"), Void, (Ptr{Void},), cs)
   s
end


show(io::IO, g::giac) = print(io, string(g))
#show(io::IO, g::giac) = print(io, "giac(\"", string(g), "\")")

_pretty_print = false

function pretty_print(flag::Bool=true)
    global _pretty_print = flag
end

#function writemime(io::IO, mime::MIME"text/latex", ex::giac) 
#    if _pretty_print
#        write(io, "\$\$", latex(ex), "\$\$")
#    else 
#        print(io, string(ex))        
#    end    
#end

   
function +(a::giac, b::giac)
   _gen(ccall(Libdl.dlsym(libgiac_c, "giac_plus"), Ptr{Void}, (Ptr{Void},Ptr{Void}), a.g, b.g))
end   
+(a::giac, b::Number) = a+giac(b)
+(a::Number, b::giac) = giac(a)+b

function -(a::giac, b::giac)
   _gen(ccall(Libdl.dlsym(libgiac_c, "giac_minus"), Ptr{Void}, (Ptr{Void},Ptr{Void}), a.g, b.g))
end   
-(a::giac, b::Number) = a-giac(b)
-(a::Number, b::giac) = giac(a)-b

function -(a::giac)
   _gen(ccall(Libdl.dlsym(libgiac_c, "giac_uminus"), Ptr{Void}, (Ptr{Void},), a.g))
end   

function *(a::giac, b::giac)
   _gen(ccall(Libdl.dlsym(libgiac_c, "giac_times"), Ptr{Void}, (Ptr{Void},Ptr{Void}), a.g, b.g))
end   
*(a::giac, b::Number) = a*giac(b)
*(a::Number, b::giac) = giac(a)*b

function /(a::giac, b::giac)
   _gen(ccall(Libdl.dlsym(libgiac_c, "giac_rdiv"), Ptr{Void}, (Ptr{Void},Ptr{Void}), a.g, b.g))
end   
/(a::giac, b::Number) = a/giac(b)
/(a::Number, b::giac) = giac(a)/b

function ^(a::giac, b::giac)
   _gen(ccall(Libdl.dlsym(libgiac_c, "giac_pow"), Ptr{Void}, (Ptr{Void},Ptr{Void},Ptr{Void}), a.g, b.g, context_ptr))
end   
^(a::giac, b::Integer) = a^giac(b)  # to ovverride ^(::Any, Integer) which is already defined
^(a::giac, b::Number) = a^giac(b)
^(a::Number, b::giac) = giac(a)^b

function ==(a::giac, b::giac)
   ccall(Libdl.dlsym(libgiac_c, "giac_equal_bool"), Cint, (Ptr{Void},Ptr{Void}), a.g, b.g)!=0
end   
==(a::giac, b::Number) = a==giac(b)
==(a::Number, b::giac) = giac(a)==b

function >(a::giacReal, b::giacReal)
   ccall(Libdl.dlsym(libgiac_c, "giac_greater_than"), Cint, (Ptr{Void},Ptr{Void}), a.g, b.g)!=0
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


function size(g::giac)
   ccall(Libdl.dlsym(libgiac_c, "giac_size1"), Cint, (Ptr{Void},), g.g)
end

length(g::giac) = size(g) # 

function getindex(g::giac_VECT, i)
   _gen(ccall(Libdl.dlsym(libgiac_c, "giac_getindex"), Ptr{Void}, (Ptr{Void},Cint), g.g, i-1))
end

function call(g::giac, x)
   _gen(ccall(Libdl.dlsym(libgiac_c, "giac_call"), Ptr{Void}, 
       (Ptr{Void},Ptr{Void},Ptr{Void}), g.g, giac(x).g, context_ptr))
end

function call(g::giac, x...)
    _gen(ccall(Libdl.dlsym(libgiac_c, "giac_call"), Ptr{Void}, 
        (Ptr{Void},Ptr{Void},Ptr{Void}), g.g, giac([x...],subtype=1).g, context_ptr))
end



endof(g::giac_VECT) = size(g)

# unary functions with context_ptr:
for F in (:real, :imag, :conj, :abs,
          :sqrt, :exp, :log, :sin, :cos, :tan,
          :sinh, :cosh, :tanh, :asin, :acos, :atan,
          :asinh, :acosh, :atanh)
   @eval begin
       function ($F)(a::giac)
          _gen(ccall(Libdl.dlsym(libgiac_c, $(string("giac_",F))), Ptr{Void}, (Ptr{Void},Ptr{Void}), a.g, context_ptr))
       end   
   end
end

function evaluate(a::giac; levels=10)
   _gen(ccall(Libdl.dlsym(libgiac_c, "giac_eval"), Ptr{Void}, (Ptr{Void},Cint,Ptr{Void}), a.g, levels, context_ptr))
end   

evaluate(s::ASCIIString; levels=10) = evaluate(giac(s), levels=levels)

function evaluatef(a::giac; levels=10)
   _gen(ccall(Libdl.dlsym(libgiac_c, "giac_evalf"), Ptr{Void}, (Ptr{Void},Cint,Ptr{Void}), a.g, levels, context_ptr))
end   

#eval = evaluate  # cannot overload Base.eval
evalf = evaluatef

function simplify(a::giac)
   _gen(ccall(Libdl.dlsym(libgiac_c, "giac_simplify"), Ptr{Void}, (Ptr{Void},Ptr{Void}), a.g, context_ptr))
end   

simplify(s::ASCIIString) = simplify(giac(s))

function expand(a::giac)
   _gen(ccall(Libdl.dlsym(libgiac_c, "giac_expand"), Ptr{Void}, (Ptr{Void},Ptr{Void}), a.g, context_ptr))
end   

function factor(a::giac; with_sqrt::Bool=false)
   _gen(ccall(Libdl.dlsym(libgiac_c, "giac_factor1"), Ptr{Void}, (Ptr{Void},Cint, Ptr{Void}), 
              a.g, with_sqrt?1:0, context_ptr))
end   


function giac(f::Symbol, arg)
   _gen(ccall(Libdl.dlsym(libgiac_c, string("giac_", f)), Ptr{Void}, (Ptr{Void},Ptr{Void}), giac(arg).g, context_ptr))
end   

giac(f::Symbol, args...) = giac(f, giac(Any[args...], subtype=1))

include("library.jl")


unapply(ex,var) = giac(:unapply, giac(Any[ex, var], subtype=1))
set(var, val) = giac(:sto, [val, var])
purge(var) = giac(:purge, var)
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
                push!(q.args, Expr(:call, :set, s.args[1], s.args[2]))
            end    
            if length(x)>1
                push!(r.args, s.args[2])
            end    
        elseif (isa(s, Expr)&&s.head==:(=)&&isa(s.args[1], Expr)
              &&s.args[1].head==:(call)&&isa(s.args[1].args[1], Symbol))
            push!(q.args, Expr(:(=), s.args[1].args[1], 
                 Expr(:call, :giac, Expr(:quote, string(s.args[1].args[1])))))
            if length(s.args[1].args)>2
                push!(q.args, Expr(:call, :set, s.args[1].args[1], Expr(:call, :unapply, s.args[2], 
                      Expr(:vect, s.args[1].args[2:end]...))))
            else
                push!(q.args, Expr(:call, :set, s.args[1].args[1], Expr(:call, :unapply, s.args[2], 
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
    eval(Main, q)
end


to_julia(g::giac) = giac(g)

function to_julia(g::giac_INT_)
   z = ccall(Libdl.dlsym(libgiac_c, "giac_get_int"), Cint, (Ptr{Void},), g.g)
   if subtype(g)==6 #Bool
       return z==1
   else
       return z
   end    
end    

to_julia(g::Union{giac_DOUBLE_, giac_FLOAT_}) = 
    ccall(Libdl.dlsym(libgiac_c, "giac_get_double"), Cdouble, (Ptr{Void},), g.g)

function to_julia(g::giac_ZINT)
    z = BigInt()
    m = ccall(Libdl.dlsym(libgiac_c, "giac_get_bigint"), Ptr{BigInt}, (Ptr{Void},), g.g)
    ccall((:__gmpz_set,:libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}), &z, m)
    z
end

function to_julia(g::giac_REAL)
    z = BigFloat()
    m = ccall(Libdl.dlsym(libgiac_c, "giac_get_bigfloat"), Ptr{BigFloat}, (Ptr{Void},), g.g)
    ccall((:mpfr_set, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Int32),
          &z, m, 0)
    z      
end

to_julia(g::giac_CPLX) = complex(to_julia(real(g)), to_julia(imag(g))) 

to_julia(g::giac_FRAC) = to_julia(numer(g))//to_julia(denom(g))

to_julia(g::giac_VECT) = [to_julia(g[i]) for i=1:length(g)]


_head(ex::giac) = symbol(string(head(ex))[2:end-1])

function _args(ex)
    a = args(ex)
    if isa(a, giac_VECT)&&subtype(args(ex))==1
        a1 = to_julia(a)
    else
        a1 = Any[to_julia(a)]
    end
end

_expr(x) = x
_expr(ex::giac_IDNT) = symbol(string(ex))

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
convert{T<:Number}(::Type{T}, x::giacNumber) = convert(T,to_julia(x))

function convert{T<:Number}(::Type{T}, ex::Union{giac_SYMB,giac_IDNT}) 
   ex = evaluate(ex)
   if isa(ex, giacNumber)
      return convert(T,to_julia(ex))
   else   
      return convert(T,to_julia(evaluatef(ex)))
   end
end   

function Base.convert(::Type{Function}, f::Union{Giac.giac_SYMB,Giac.giac_IDNT}) 
    a = args(evaluate(f))
    to_julia(to_julia(a[3]),to_julia(a[1])...)
end   

end # module
