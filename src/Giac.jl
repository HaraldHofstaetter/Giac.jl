__precompile__()
module Giac

import Base: string, show, write, writemime, expand, factor, collect 
import Base: diff, sum, zeros, length, size, getindex, endof, call
import Base: +, -, (*), /, ^, ==, >, <, >=, <=
import Base: ctranspose
import Base: real, imag, conj, abs, sign
import Base: sqrt, exp, log, sin, cos, tan
import Base: sinh, cosh, tanh, asin, acos, atan, acot, acsc
import Base: asinh, acosh, atanh

export @giac, giac, Gen, undef, infinity, giac_identifier
export evaluate, evaluatef, evalf, simplify, to_julia, store, purge, giac_vars
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

config_vars = (:Digits, :epsilon)

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
GenNumber = Union{Gen_INT_,Gen_DOUBLE_,Gen_ZINT, Gen_REAL, Gen_FLOAT_, Gen_CPLX}

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

function Gen(g::Gen)
   _gen(ccall(Libdl.dlsym(libgiac_c, "giac_copy_gen"), Ptr{Void}, (Ptr{Void},), g.g))
end   

function Gen(val::Bool)
    g = _gen(ccall(Libdl.dlsym(libgiac_c, "giac_new_int"), Ptr{Void}, (Cint,), val?1:0))
    change_subtype(g, 6)
    g
end    

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

Gen{T}(x::Range{T}) = Gen(collect(x))

Gen(::Irrational{:e}) = giac_e
Gen(::Irrational{:π}) = giac_pi


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
#show(io::IO, g::Gen) = print(io, "giac(\"", string(g), "\")")

_pretty_print = false

function pretty_print(flag::Bool=true)
    global _pretty_print = flag
end

#function writemime(io::IO, mime::MIME"text/latex", ex::Gen) 
#    if _pretty_print
#        write(io, "\$\$", latex(ex), "\$\$")
#    else 
#        print(io, string(ex))        
#    end    
#end

   
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


function size(g::Gen)
   ccall(Libdl.dlsym(libgiac_c, "giac_size1"), Cint, (Ptr{Void},), g.g)
end

length(g::Gen) = size(g) # 

function getindex(g::Gen_VECT, i)
   _gen(ccall(Libdl.dlsym(libgiac_c, "giac_getindex"), Ptr{Void}, (Ptr{Void},Cint), g.g, i-1))
end

function call(g::Gen, x)
   _gen(ccall(Libdl.dlsym(libgiac_c, "giac_call"), Ptr{Void}, 
       (Ptr{Void},Ptr{Void},Ptr{Void}), g.g, Gen(x).g, context_ptr))
end

function call(g::Gen, x...)
    _gen(ccall(Libdl.dlsym(Giac.libgiac_c, "giac_call"), Ptr{Void}, 
        (Ptr{Void},Ptr{Void},Ptr{Void}), g.g, Gen([x...],subtype=1).g, context_ptr))
end



endof(g::Gen_VECT) = size(g)

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
#include("plotting.jl")


unapply(ex,var) = giac(:unapply, giac(Any[ex, var], subtype=1))
store(val, var) = giac(:sto, [val, var])
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
                push!(q.args, Expr(:call, :store, s.args[2], s.args[1]))
            end    
            if length(x)>1
                push!(r.args, s.args[2])
            end    
        elseif (isa(s, Expr)&&s.head==:(=)&&isa(s.args[1], Expr)
              &&s.args[1].head==:(call)&&isa(s.args[1].args[1], Symbol))
            push!(q.args, Expr(:(=), s.args[1].args[1], 
                 Expr(:call, :giac, Expr(:quote, string(s.args[1].args[1])))))
            if length(s.args[1].args)>2
                push!(q.args, Expr(:call, :store, Expr(:call, :unapply, s.args[2], 
                      Expr(:vect, s.args[1].args[2:end]...)), s.args[1].args[1]))
            else
                push!(q.args, Expr(:call, :store, Expr(:call, :unapply, s.args[2], 
                      s.args[1].args[2]), s.args[1].args[1]))
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


to_julia(g::Gen) = Gen(g)

function to_julia(g::Gen_INT_)
   z = ccall(Libdl.dlsym(libgiac_c, "giac_get_int"), Cint, (Ptr{Void},), g.g)
   if subtype(g)==6 #Bool
       return z==1
   else
       return z
   end    
end    

to_julia(g::Union{Gen_DOUBLE_, Gen_FLOAT_}) = 
    ccall(Libdl.dlsym(libgiac_c, "giac_get_double"), Cdouble, (Ptr{Void},), g.g)

function to_julia(g::Gen_ZINT)
    z = BigInt()
    m = ccall(Libdl.dlsym(libgiac_c, "giac_get_bigint"), Ptr{BigInt}, (Ptr{Void},), g.g)
    ccall((:__gmpz_set,:libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}), &z, m)
    z
end

function to_julia(g::Gen_REAL)
    z = BigFloat()
    m = ccall(Libdl.dlsym(libgiac_c, "giac_get_bigfloat"), Ptr{BigFloat}, (Ptr{Void},), g.g)
    ccall((:mpfr_set, :libmpfr), Int32, (Ptr{BigFloat}, Ptr{BigFloat}, Int32),
          &z, m, 0)
    z      
end

to_julia(g::Gen_CPLX) = complex(to_julia(real(g)), to_julia(imag(g))) 

to_julia(g::Gen_FRAC) = to_julia(numer(g))//to_julia(denom(g))

to_julia(g::Gen_VECT) = [to_julia(g[i]) for i=1:length(g)]

end # module
