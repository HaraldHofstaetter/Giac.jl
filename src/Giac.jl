__precompile__()
module Giac

import Base: string, show, write, writemime, expand, factor
import Base: +, -, (*), /, ^, ==, >, <, >=, <=
import Base: real, imag, conj, abs
import Base: sqrt, exp, log, sin, cos, tan
import Base: sinh, cosh, tanh, asin, acos, atan
import Base: asinh, acosh, atanh

export @giac, giac, Gen, undef, giac_identifier
export evaluate, evaluatef, value, evalf, simplify
export simplify, latex, pretty_print
export integrate



function __init__()
    global libgiac
    global libgiac_c
    global context_ptr
    global undef
    libgiac = Libdl.dlopen(joinpath(dirname(@__FILE__), "..", "deps", "lib",
                     string("libgiac.", Libdl.dlext)))
    libgiac_c = Libdl.dlopen(joinpath(dirname(@__FILE__), "..", "deps", "lib",
                     string("libgiac_c.", Libdl.dlext)))
    context_ptr = ccall(Libdl.dlsym(libgiac_c, "giac_context_ptr"), Ptr{Void}, () )
    undef = _gen(ccall(Libdl.dlsym(libgiac_c, "giac_undef"), Ptr{Void}, () ))
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
       return Gen_INT_(g)
   elseif t==Int( _DOUBLE_ )   
       return Gen_DOUBLE_(g)
   elseif t==Int( _ZINT )
       return Gen_ZINT(g)
   elseif t==Int( _REAL )
       return Gen_REAL(g)
   elseif t==Int( _CPLX )
       return Gen_CPLX(g)
   elseif t==Int( _POLY )
       return Gen_POLY(g)
   elseif t==Int( _IDNT )
       return Gen_IDNT(g)
   elseif t==Int( _VECT )
       return Gen_VECT(g)
   elseif t==Int( _SYMB )
       return Gen_SYMB(g)
   elseif t==Int( _SPOL1 )
       return Gen_SPOL1(g)
   elseif t==Int( _FRAC )
       return Gen_FRAC(g)
   elseif t==Int( _EXT )
       return Gen_EXT(g)
   elseif t==Int( _STRNG )
       return Gen_STRNG(g)
   elseif t==Int( _FUNC )
       return Gen_FUNC(g)
   elseif t==Int( _ROOT )
       return Gen_ROOT(g)
   elseif t==Int( _MOD )
       return Gen_MOD(g)
   elseif t==Int( _USER )
       return Gen_USER(g)
   elseif t==Int( _MAP )
       return Gen_MAP(g)
   elseif t==Int( _EQW )
       return Gen_EQW(g)
   elseif t==Int( _GROB )
       return Gen_GROB(g)
   elseif t==Int( _POINTER )
       return Gen_POINTER(g)
   elseif t==Int( _FLOAT_ )
       return Gen_FLOAT_(g)
   end
   @assert false "unknown giac_type"
end



function _delete(g::Gen)
    ccall( Libdl.dlsym(libgiac_c, "giac_delete"), Void, (Ptr{Void},), g.g)
end

Gen(x) = undef
Gen(x::Gen) = x

function Gen(val::Cint)
    c = ccall(Libdl.dlsym(libgiac_c, "giac_new_int"), Ptr{Void}, (Cint,), val)
    g = _gen(c)
    finalizer(g, _delete)
    g
end

function Gen(val::Int64)
    c = ccall(Libdl.dlsym(libgiac_c, "giac_new_int64_t"), Ptr{Void}, (Int64,), val)
    g = _gen(c)
    finalizer(g, _delete)
    g
end

function Gen(val::BigInt)
    c = ccall(Libdl.dlsym(libgiac_c, "giac_new_bigint"), Ptr{Void}, (Ptr{BigInt},), &val)
    g = _gen(c)
    finalizer(g, _delete)
    g
end

function Gen(val::Rational)
    c = ccall(Libdl.dlsym(libgiac_c, "giac_new_rational"), Ptr{Void}, (Ptr{Void},Ptr{Void}), Gen(val.num).g, Gen(val.den).g)
    g = _gen(c)
    finalizer(g, _delete)
    g
end

#does not seem to work properly
#function Gen(val::Int128)
#    c = ccall(Libdl.dlsym(libgiac_c, "giac_new_int128_t"), Ptr{Void}, (Int128,), val)
#    g = _gen(c)
#    finalizer(g, _delete)
#    g
#end

function Gen(val::Cdouble)
    c = ccall(Libdl.dlsym(libgiac_c, "giac_new_double"), Ptr{Void}, (Cdouble,), val)
    g = _gen(c)
    finalizer(g, _delete)
    g
end

function Gen(val::BigFloat)
    c = ccall(Libdl.dlsym(libgiac_c, "giac_new_bigfloat"), Ptr{Void}, (Ptr{BigFloat},), &val)
    g = _gen(c)
    finalizer(g, _delete)
    g
end


function Gen(val::Complex)
    c = ccall(Libdl.dlsym(libgiac_c, "giac_new_complex"), Ptr{Void}, (Ptr{Void},Ptr{Void}), Gen(real(val)).g, Gen(imag(val)).g)
    g = _gen(c)
    finalizer(g, _delete)
    g
end


function Gen(val::Complex{Cint})  
    c = ccall(Libdl.dlsym(libgiac_c, "giac_new_complex_int"), Ptr{Void}, (Cint, Cint), real(val), real(val))
    g = _gen(c)
    finalizer(g, _delete)
    g
end


function Gen(val::Complex{Cdouble})  
    c = ccall(Libdl.dlsym(libgiac_c, "giac_new_complex_double"), Ptr{Void}, (Cdouble, Cdouble), real(val), real(val))
    g = _gen(c)
    finalizer(g, _delete)
    g
end

function giac_identifier(s::ASCIIString)
    c = ccall(Libdl.dlsym(libgiac_c, "giac_new_ident"), Ptr{Void}, (Ptr{UInt8},), s)
    g = _gen(c)
    finalizer(g, _delete)
    g
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
    c = ccall(Libdl.dlsym(libgiac_c, "giac_new_symbolic"), Ptr{Void}, (Ptr{UInt8},Ptr{Void}), s, context_ptr)
    g = _gen(c)
    finalizer(g, _delete)
    g
end

function Gen{T}(v::Array{T,1})
    v1 = Ptr{Void}[Gen(i).g for i in v]
    c = ccall(Libdl.dlsym(libgiac_c, "giac_new_vector"), Ptr{Void}, 
              (Ptr{Ptr{Void}},Cint,Cshort), 
               v1, length(v1), 0)
    g = _gen(c)
    finalizer(g, _delete)
    g
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
   ccall(Libdl.dlsym(libgiac_c, "giac_equal"), Cint, (Ptr{Void},Ptr{Void}), a.g, b.g)!=0
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
   _gen(ccall(Libdl.dlsym(libgiac_c, "giac_factor"), Ptr{Void}, (Ptr{Void},Cint, Ptr{Void}), 
              a.g, with_sqrt?1:0, context_ptr))
end   

function integrate(ex::Gen, var::Gen)
   _gen(ccall(Libdl.dlsym(libgiac_c, "giac_integrate"), Ptr{Void}, (Ptr{Void},Ptr{Void}), 
              Gen([ex,var]).g, context_ptr))
end   

function integrate(ex::Gen, var::Gen, a::Union{Gen,Number}, b::Union{Gen,Number})
   _gen(ccall(Libdl.dlsym(libgiac_c, "giac_integrate"), Ptr{Void}, (Ptr{Void},Ptr{Void}), 
              Gen([ex,var,a,b]).g, context_ptr))
end   


value(g::Gen) = g

end # module
