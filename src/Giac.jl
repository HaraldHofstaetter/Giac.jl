module Giac

import Base: string, show, +, -, (*), /

export Gen

function __init__()
    global libgiac_c
    libgiac_c = Libdl.dlopen(joinpath(dirname(@__FILE__), "..", "deps", "lib",
                     string("libgiac_c.", Libdl.dlext)))
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
    _USER= 16, # gen_user * _USERptr
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
   @assert false "unknown gen_type"
end



function _delete(g::Gen)
    ccall( Libdl.dlsym(libgiac_c, "gen_delete"), Void, (Ptr{Void},), g.g)
end

function Gen(val::Cint)
    c = ccall(Libdl.dlsym(libgiac_c, "gen_new_int"), Ptr{Void}, (Cint,), val)
    g = _gen(c)
    finalizer(g, _delete)
    g
end

function Gen(val::Int64)
    c = ccall(Libdl.dlsym(libgiac_c, "gen_new_int64_t"), Ptr{Void}, (Int64,), val)
    g = _gen(c)
    finalizer(g, _delete)
    g
end

function Gen(val::Rational{Int64})
    num = ccall(Libdl.dlsym(libgiac_c, "gen_new_int64_t"), Ptr{Void}, (Int64,), val.num)
    den = ccall(Libdl.dlsym(libgiac_c, "gen_new_int64_t"), Ptr{Void}, (Int64,), val.den)
    c = ccall(Libdl.dlsym(libgiac_c, "gen_new_fraction"), Ptr{Void}, (Ptr{Void},Ptr{Void}), num, den)
    g = _gen(c)
    finalizer(g, _delete)
    g
end

#does not seem to work properly
#function Gen(val::Int128)
#    c = ccall(Libdl.dlsym(libgiac_c, "gen_new_int128_t"), Ptr{Void}, (Int128,), val)
#    g = _gen(c)
#    finalizer(g, _delete)
#    g
#end

function Gen(val::Cdouble)
    c = ccall(Libdl.dlsym(libgiac_c, "gen_new_double"), Ptr{Void}, (Cdouble,), val)
    g = _gen(c)
    finalizer(g, _delete)
    g
end

function Gen(a::Cint, b::Cint)
    c = ccall(Libdl.dlsym(libgiac_c, "gen_new_int_int"), Ptr{Void}, (Cint, Cint), a, b)
    g = _gen(c)
    finalizer(g, _delete)
    g
end

Gen(val::Complex{Cint}) = Gen(real(val), imag(val))

function Gen(a::Cdouble, b::Cdouble)
    c = ccall(Libdl.dlsym(libgiac_c, "gen_new_double_double"), Ptr{Void}, (Cdouble, Cdouble), a, b)
    g = _gen(c)
    finalizer(g, _delete)
    g
end

Gen(val::Complex{Cdouble}) = Gen(real(val), imag(val))



function string(g::Gen)
   cs = ccall(Libdl.dlsym(libgiac_c, "gen_to_c_string"), Ptr{UInt8}, (Ptr{Void},), g.g) 
   s = bytestring(cs)
   ccall((:free, "libc"), Void, (Ptr{Void},), cs)
   s
end

show(io::IO, g::Gen) = print(io, string(g))



   
function +(a::Gen, b::Gen)
   _gen(ccall(Libdl.dlsym(libgiac_c, "gen_op_plus"), Ptr{Void}, (Ptr{Void},Ptr{Void}), a.g, b.g))
end   

function -(a::Gen, b::Gen)
   _gen(ccall(Libdl.dlsym(libgiac_c, "gen_op_minus"), Ptr{Void}, (Ptr{Void},Ptr{Void}), a.g, b.g))
end   

function -(a::Gen)
   _gen(ccall(Libdl.dlsym(libgiac_c, "gen_op_uminus"), Ptr{Void}, (Ptr{Void},), a.g))
end   

function *(a::Gen, b::Gen)
   _gen(ccall(Libdl.dlsym(libgiac_c, "gen_op_times"), Ptr{Void}, (Ptr{Void},Ptr{Void}), a.g, b.g))
end   

function /(a::Gen, b::Gen)
   _gen(ccall(Libdl.dlsym(libgiac_c, "gen_op_rdiv"), Ptr{Void}, (Ptr{Void},Ptr{Void}), a.g, b.g))
end   

end # module
