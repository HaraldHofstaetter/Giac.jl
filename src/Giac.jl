__precompile__()
module Giac

import Base: string, show, write, writemime, expand, factor, collect 
import Base: diff, sum, zeros
import Base: +, -, (*), /, ^, ==, >, <, >=, <=
import Base: real, imag, conj, abs, sign
import Base: sqrt, exp, log, sin, cos, tan
import Base: sinh, cosh, tanh, asin, acos, atan, acot, acsc
import Base: asinh, acosh, atanh

export @giac, giac, Gen, undef, infinity, giac_identifier
export evaluate, evaluatef, value, evalf, simplify
export simplify, plus_inf, minus_inf, latex, pretty_print

export partfrac, subst, left, right, denom, numer
export ⩦, equal
export integrate, limit, series, curl, grad, divergence, hessian
export preval, sum_riemann, taylor
export solve, cSolve, cZeros 
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
end


abstract Gen


# from giac/dispatch.h:  
@enum( Gen_type, 
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

type Gen_INT_ <: Gen
    g::Ptr{Void}
end

type Gen_DOUBLE_ <: Gen
    g::Ptr{Void}
end

type Gen_ZINT <: Gen
    g::Ptr{Void}
end

type Gen_REAL  <: Gen
    g::Ptr{Void}
end

type Gen_CPLX <: Gen
    g::Ptr{Void}
end

type Gen_POLY <: Gen
    g::Ptr{Void}
end

type Gen_IDNT <: Gen
    g::Ptr{Void}
end

type Gen_VECT <: Gen
    g::Ptr{Void}
end

type Gen_SYMB <: Gen
    g::Ptr{Void}
end

type Gen_SPOL1 <: Gen
    g::Ptr{Void}
end

type Gen_FRAC <: Gen
    g::Ptr{Void}
end

type Gen_EXT <: Gen
    g::Ptr{Void}
end

type Gen_STRNG <: Gen
    g::Ptr{Void}
end

type Gen_FUNC <: Gen
    g::Ptr{Void}
end

type Gen_ROOT <: Gen
    g::Ptr{Void}
end

type Gen_MOD <: Gen
    g::Ptr{Void}
end

type Gen_USER <: Gen
    g::Ptr{Void}
end

type Gen_MAP <: Gen
    g::Ptr{Void}
end

type Gen_EQW <: Gen
    g::Ptr{Void}
end

type Gen_GROB <: Gen
    g::Ptr{Void}
end

type Gen_POINTER_ <: Gen
    g::Ptr{Void}
end

type Gen_FLOAT_ <: Gen
    g::Ptr{Void}
end

GenReal = Union{Gen_INT_,Gen_DOUBLE_,Gen_ZINT, Gen_REAL, Gen_FLOAT_}

function _gen(g::Ptr{Void})
   t = unsafe_load(Ptr{UInt8}(g), 1) & 31
   if t==Int( _INT_ )
       gg = Gen_INT_(g)
   elseif t==Int( _DOUBLE_ )   
       gg = Gen_DOUBLE_(g)
   elseif t==Int( _ZINT )
       gg = Gen_ZINT(g)
   elseif t==Int( _REAL )
       gg = Gen_REAL(g)
   elseif t==Int( _CPLX )
       gg = Gen_CPLX(g)
   elseif t==Int( _POLY )
       gg = Gen_POLY(g)
   elseif t==Int( _IDNT )
       gg = Gen_IDNT(g)
   elseif t==Int( _VECT )
       gg = Gen_VECT(g)
   elseif t==Int( _SYMB )
       gg = Gen_SYMB(g)
   elseif t==Int( _SPOL1 )
       gg = Gen_SPOL1(g)
   elseif t==Int( _FRAC )
       gg = Gen_FRAC(g)
   elseif t==Int( _EXT )
       gg = Gen_EXT(g)
   elseif t==Int( _STRNG )
       gg = Gen_STRNG(g)
   elseif t==Int( _FUNC )
       gg = Gen_FUNC(g)
   elseif t==Int( _ROOT )
       gg = Gen_ROOT(g)
   elseif t==Int( _MOD )
       gg = Gen_MOD(g)
   elseif t==Int( _USER )
       gg = Gen_USER(g)
   elseif t==Int( _MAP )
       gg = Gen_MAP(g)
   elseif t==Int( _EQW )
       gg = Gen_EQW(g)
   elseif t==Int( _GROB )
       gg = Gen_GROB(g)
   elseif t==Int( _POINTER )
       gg = Gen_POINTER(g)
   elseif t==Int( _FLOAT_ )
       gg = Gen_FLOAT_(g)
   else    
       @assert false "unknown giac_type"
   end
   finalizer(gg, _delete)
   gg
end


subtype(g::Gen) = unsafe_load(Ptr{UInt8}(g.g), 2) 

function change_subtype(g::Gen, subtype::Integer)
    ccall( Libdl.dlsym(libgiac_c, "giac_change_subtype"), Void, (Ptr{Void},Cint), g.g, subtype)
end


function _delete(g::Gen)
    ccall( Libdl.dlsym(libgiac_c, "giac_delete"), Void, (Ptr{Void},), g.g)
end

Gen(x) = undef
Gen(x::Gen) = x

function Gen(val::Cint)
    _gen(ccall(Libdl.dlsym(libgiac_c, "giac_new_int"), Ptr{Void}, (Cint,), val))
end

function Gen(val::Int64)
    _gen(ccall(Libdl.dlsym(libgiac_c, "giac_new_int64_t"), Ptr{Void}, (Int64,), val))
end

function Gen(val::BigInt)
    _gen(ccall(Libdl.dlsym(libgiac_c, "giac_new_bigint"), Ptr{Void}, (Ptr{BigInt},), &val))
end

function Gen(val::Rational)
    _gen(ccall(Libdl.dlsym(libgiac_c, "giac_new_rational"), Ptr{Void}, (Ptr{Void},Ptr{Void}), Gen(val.num).g, Gen(val.den).g))
end

#does not seem to work properly
#function Gen(val::Int128)
#    _gen(ccall(Libdl.dlsym(libgiac_c, "giac_new_int128_t"), Ptr{Void}, (Int128,), val))
#end

function Gen(val::Cdouble)
    _gen(ccall(Libdl.dlsym(libgiac_c, "giac_new_double"), Ptr{Void}, (Cdouble,), val))
end

function Gen(val::BigFloat)
    _gen(ccall(Libdl.dlsym(libgiac_c, "giac_new_bigfloat"), Ptr{Void}, (Ptr{BigFloat},), &val))
end


function Gen(val::Complex)
    _gen(ccall(Libdl.dlsym(libgiac_c, "giac_new_complex"), Ptr{Void}, (Ptr{Void},Ptr{Void}), Gen(real(val)).g, Gen(imag(val)).g))
end


function Gen(val::Complex{Cint})  
    _gen(ccall(Libdl.dlsym(libgiac_c, "giac_new_complex_int"), Ptr{Void}, (Cint, Cint), real(val), real(val)))
end


function Gen(val::Complex{Cdouble})  
    _gen(ccall(Libdl.dlsym(libgiac_c, "giac_new_complex_double"), Ptr{Void}, (Cdouble, Cdouble), real(val), real(val)))
end

function giac_identifier(s::ASCIIString)
    _gen(ccall(Libdl.dlsym(libgiac_c, "giac_new_ident"), Ptr{Void}, (Ptr{UInt8},), s))
end


#This magic code stolen from SymPy, cf.
#https://github.com/jverzani/SymPy.jl/blob/master/src/utils.jl
macro giac(x...) 
    q=Expr(:block)
    if length(x) == 1 && isa(x[1],Expr)
        @assert x[1].head === :tuple "@giac expected a list of symbols"
        x = x[1].args
    end
    for s in x
        @assert isa(s,Symbol) "@giac expected a list of symbols"
        push!(q.args, Expr(:(=), s, Expr(:call, :giac, Expr(:quote, string(s)))))
        # Use here :giac (:giac_identifier should also work, but this leads to 
        # strange behavior...
    end
    push!(q.args, Expr(:tuple, x...))
    eval(Main, q)
end


function Gen(s::ASCIIString)
    _gen(ccall(Libdl.dlsym(libgiac_c, "giac_new_symbolic"), Ptr{Void}, (Ptr{UInt8},Ptr{Void}), s, context_ptr))
end

function Gen{T}(v::Array{T,1}; subtype::Integer=0)
    v1 = Ptr{Void}[Gen(i).g for i in v]
    _gen(ccall(Libdl.dlsym(libgiac_c, "giac_new_vector"), Ptr{Void}, 
              (Ptr{Ptr{Void}},Cint,Cshort), 
               v1, length(v1), subtype))
end

function Gen{T}(A::Array{T,2})
    Gen([reshape(A[i,:], size(A,2)) for i in 1:size(A,1)])
end    


giac = Gen


function string(g::Gen)
   cs = ccall(Libdl.dlsym(libgiac_c, "giac_to_string"), Ptr{UInt8}, (Ptr{Void},Ptr{Void}), g.g, context_ptr) 
   s = bytestring(cs)
   ccall((:free, "libc"), Void, (Ptr{Void},), cs)
   s
end

function latex(g::Gen)
   cs = ccall(Libdl.dlsym(libgiac_c, "giac_to_latex"), Ptr{UInt8}, (Ptr{Void},Ptr{Void}), g.g, context_ptr) 
   s = bytestring(cs)
   ccall((:free, "libc"), Void, (Ptr{Void},), cs)
   s
end


show(io::IO, g::Gen) = print(io, string(g))

_pretty_print = false

function pretty_print(flag::Bool=true)
    global _pretty_print = flag
end

#writemime(io::IO, ::MIME"text/latex", ex::Gen) =  
#    _pretty_print?write(io, "\$", latex(ex), "\$"):print(io, string(ex))

   
function +(a::Gen, b::Gen)
   _gen(ccall(Libdl.dlsym(libgiac_c, "giac_plus"), Ptr{Void}, (Ptr{Void},Ptr{Void}), a.g, b.g))
end   
+(a::Gen, b::Number) = a+Gen(b)
+(a::Number, b::Gen) = Gen(a)+b

function -(a::Gen, b::Gen)
   _gen(ccall(Libdl.dlsym(libgiac_c, "giac_minus"), Ptr{Void}, (Ptr{Void},Ptr{Void}), a.g, b.g))
end   
-(a::Gen, b::Number) = a-Gen(b)
-(a::Number, b::Gen) = Gen(a)-b

function -(a::Gen)
   _gen(ccall(Libdl.dlsym(libgiac_c, "giac_uminus"), Ptr{Void}, (Ptr{Void},), a.g))
end   

function *(a::Gen, b::Gen)
   _gen(ccall(Libdl.dlsym(libgiac_c, "giac_times"), Ptr{Void}, (Ptr{Void},Ptr{Void}), a.g, b.g))
end   
*(a::Gen, b::Number) = a*Gen(b)
*(a::Number, b::Gen) = Gen(a)*b

function /(a::Gen, b::Gen)
   _gen(ccall(Libdl.dlsym(libgiac_c, "giac_rdiv"), Ptr{Void}, (Ptr{Void},Ptr{Void}), a.g, b.g))
end   
/(a::Gen, b::Number) = a/Gen(b)
/(a::Number, b::Gen) = Gen(a)/b

function ^(a::Gen, b::Gen)
   _gen(ccall(Libdl.dlsym(libgiac_c, "giac_pow"), Ptr{Void}, (Ptr{Void},Ptr{Void},Ptr{Void}), a.g, b.g, context_ptr))
end   
^(a::Gen, b::Integer) = a^Gen(b)  # to ovverride ^(::Any, Integer) which is already defined
^(a::Gen, b::Number) = a^Gen(b)
^(a::Number, b::Gen) = Gen(a)^b

function ==(a::Gen, b::Gen)
   ccall(Libdl.dlsym(libgiac_c, "giac_equal_bool"), Cint, (Ptr{Void},Ptr{Void}), a.g, b.g)!=0
end   
==(a::Gen, b::Number) = a==Gen(b)
==(a::Number, b::Gen) = Gen(a)==b

function >(a::GenReal, b::GenReal)
   ccall(Libdl.dlsym(libgiac_c, "giac_greater_than"), Cint, (Ptr{Void},Ptr{Void}), a.g, b.g)!=0
end   
<(a::GenReal, b::GenReal) = b>a
<=(a::GenReal, b::GenReal) = !(a>b)
>=(a::GenReal, b::GenReal) = !(b>a)
<=(a::GenReal, b::Real) = a<=Gen(b)
<=(a::Real, b::GenReal) = Gen(a)<=b
>=(a::GenReal, b::Real) = a>=Gen(b)
>=(a::Real, b::GenReal) = Gen(a)>=b
>(a::GenReal, b::Real) = a>Gen(b)
<(a::Real, b::GenReal) = Gen(a)<b
<(a::GenReal, b::Real) = a<Gen(b)
>(a::Real, b::GenReal) = Gen(a)>b



# unary functions with context_ptr:
for F in (:real, :imag, :conj, :abs,
          :sqrt, :exp, :log, :sin, :cos, :tan,
          :sinh, :cosh, :tanh, :asin, :acos, :atan,
          :asinh, :acosh, :atanh)
   @eval begin
       function ($F)(a::Gen)
          _gen(ccall(Libdl.dlsym(libgiac_c, $(string("giac_",F))), Ptr{Void}, (Ptr{Void},Ptr{Void}), a.g, context_ptr))
       end   
   end
end







function evaluate(a::Gen; levels=10)
   _gen(ccall(Libdl.dlsym(libgiac_c, "giac_eval"), Ptr{Void}, (Ptr{Void},Cint,Ptr{Void}), a.g, levels, context_ptr))
end   

evaluate(s::ASCIIString; levels=10) = evaluate(Gen(s), levels=levels)

function evaluatef(a::Gen; levels=10)
   _gen(ccall(Libdl.dlsym(libgiac_c, "giac_evalf"), Ptr{Void}, (Ptr{Void},Cint,Ptr{Void}), a.g, levels, context_ptr))
end   

#eval = evaluate  # cannot overload Base.eval
evalf = evaluatef

function simplify(a::Gen)
   _gen(ccall(Libdl.dlsym(libgiac_c, "giac_simplify"), Ptr{Void}, (Ptr{Void},Ptr{Void}), a.g, context_ptr))
end   

simplify(s::ASCIIString) = simplify(Gen(s))

function expand(a::Gen)
   _gen(ccall(Libdl.dlsym(libgiac_c, "giac_expand"), Ptr{Void}, (Ptr{Void},Ptr{Void}), a.g, context_ptr))
end   

function factor(a::Gen; with_sqrt::Bool=false)
   _gen(ccall(Libdl.dlsym(libgiac_c, "giac_factor1"), Ptr{Void}, (Ptr{Void},Cint, Ptr{Void}), 
              a.g, with_sqrt?1:0, context_ptr))
end   


function Gen(f::Symbol, arg)
   _gen(ccall(Libdl.dlsym(libgiac_c, string("giac_", f)), Ptr{Void}, (Ptr{Void},Ptr{Void}), Gen(arg).g, context_ptr))
end   

include("library.jl")


value(g::Gen) = g

end # module
