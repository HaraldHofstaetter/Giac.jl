using Giac
using Base.Test

#For elementary functions we check, whether the Giac/Function of a 
#numerical value computes (approximately) the same result as the
#corresponding Julia function

for (ex, res) in [
    (sqrt(giac(0.9)), sqrt(0.9)),
    (exp(giac(0.9)), exp(0.9)),
    (log(giac(0.9)), log(0.9)),
    (log(3,giac(0.9)), log(3,0.9)),
   #(log10(giac(0.9)), log10(0.9)), # log10 not available in Giac
    (sin(giac(0.9)), sin(0.9)),
    (cos(giac(0.9)), cos(0.9)),
    (tan(giac(0.9)), tan(0.9)),
    (cot(giac(0.9)), cot(0.9)),
    (sec(giac(0.9)), sec(0.9)),
    (csc(giac(0.9)), csc(0.9)),
    (sinh(giac(0.9)), sinh(0.9)),
    (cosh(giac(0.9)), cosh(0.9)),
    (tanh(giac(0.9)), tanh(0.9)),
    (asin(giac(0.9)), asin(0.9)),
    (acos(giac(0.9)), acos(0.9)),
    (atan(giac(0.9)), atan(0.9)),
    (acot(giac(0.9)), acot(0.9)),
    (asec(giac(1.9)), asec(1.9)), # |arg|>=1
    (acsc(giac(1.9)), acsc(1.9)), # |arg|>=1
    (asinh(giac(0.9)), asinh(0.9)),
    (acosh(giac(1.9)), acosh(1.9)), #arg>=1
    (atanh(giac(0.9)), atanh(0.9)),

    (erf(giac(0.9)), erf(0.9)),
    (erfc(giac(0.9)), erfc(0.9)),
    (gamma(giac(0.9)), gamma(0.9)),
    (beta(giac(0.9),giac(1.9)), beta(0.9,1.9)),
    (zeta(giac(0.9)), zeta(0.9)),
    #(airyai(giac(0.9)), airyai(0.9)), # Giac/Airy_Ai does not work as expected
    #(airybi(giac(0.9)), airybi(0.9)), # Giac/Airy_Bi does not work as expected
    (besselj(2,giac(0.9)), besselj(2,0.9)),
    (bessely(2,giac(0.9)), bessely(2,0.9)),
]
    @test convert(Float64,ex) ≈ res
end    

@test convert(Float64, sign(giac(-12.3))) == -1.0
@test convert(Float64, sign(giac(0.0))) == 0.0 
@test convert(Float64, sign(giac(1.3))) == 1.0
@test convert(Float64, round(giac(-1.7))) == -2.0
@test convert(Float64, round(giac(1.4999))) == 1.0
@test convert(Float64, ceil(giac(1.1))) == 2.0
@test convert(Float64, ceil(giac(-1.1))) == -1.0
@test convert(Float64, floor(giac(1.1))) == 1.0
@test convert(Float64, floor(giac(-1.1))) == -2.0
@test convert(Float64, trunc(giac(1.1))) == 1.0
@test convert(Float64, trunc(giac(-1.1))) == -1.0



#Note: We don't check whether the underlying Giac C++ library computes
#correct results. We only test if the Julia functions in Giac.jl/src/library.jl
#are correctly implemented. We assume that this is the case, if the Julia
#function computes the same (or at least an equivalent) result as the
#corresponding Giac function called via the Giac parser.

@giac x y z n

ex1 = simplify(4*atan(giac(1//5))-atan(giac(1//239)))
ex2 = giac("simplify(4*atan(1/5)-atan(1/239))")
@test evaluate(ex1) == evaluate(ex2)

ex1 = collect(x+2*x+1-4)
ex2 = giac("collect(x+2*x+1-4)")
@test evaluate(ex1) == evaluate(ex2)

ex1 = expand((x+y)*(z+1))
ex2 = giac("expand((x+y)*(z+1))")
@test evaluate(ex1) == evaluate(ex2)

ex1 = factor(x^4-4)
ex2 = giac("factor(x^4-4)")
@test evaluate(ex1) == evaluate(ex2)

ex1 = partfrac((x^2-2*x+3)/(x^2-3*x+2))
ex2 = giac("partfrac((x^2-2*x+3)/(x^2-3*x+2))")
@test evaluate(ex1) == evaluate(ex2)

ex1 = numer((x^3-1)/(x^2-1))
ex2 = giac("numer((x^3-1)/(x^2-1))")
@test evaluate(ex1) == evaluate(ex2)

ex1 = denom((x^3-1)/(x^2-1))
ex2 = giac("denom((x^3-1)/(x^2-1))")
@test evaluate(ex1) == evaluate(ex2)

ex1 = left(x^2-1 ⩦ 2*x+3)
ex2 = giac("left(x^2-1 = 2*x+3)")
@test evaluate(ex1) == evaluate(ex2)

ex1 = right(x^2-1 ⩦ 2*x+3)
ex2 = giac("right(x^2-1 = 2*x+3)")
@test evaluate(ex1) == evaluate(ex2)

ex1 = diff(x^3-x,x)
ex2 = giac("diff(x^3-x,x)")
@test evaluate(ex1) == evaluate(ex2)

ex1 = diff(exp(x*y),x,x,x,y,y)
ex2 = giac("diff(exp(x*y),x,x,x,y,y)")
@test evaluate(ex1) == evaluate(ex2)

ex1 = integrate(1/(4+z^2),z)
ex2 = giac("integrate(1/(4+z^2),z)")
@test evaluate(ex1) == evaluate(ex2)

ex1 = integrate(1/(1-x^4),x,2,3)
ex2 = giac("integrate(1/(1-x^4),x,2,3)")
@test evaluate(ex1) == evaluate(ex2)

ex1 = limit(sin(x)/(x^2-3*x),x=>0)
ex2 = giac("limit(sin(x)/(x^2-3*x),x=0)")
@test evaluate(ex1) == evaluate(ex2)

ex1 = limit((2*x-1)/exp(1/(x-1)),x=>plus_inf)
ex2 = giac("limit((2*x-1)/exp(1/(x-1)),x=+infinity)")
@test evaluate(ex1) == evaluate(ex2)

ex1 = limit(sign(x),x=>0,1)
ex2 = giac("limit(sign(x),x=0,1)")
@test evaluate(ex1) == evaluate(ex2)

ex1 = series((x^4+x+2)/(x^2+1),x=>0, 5)
ex2 = giac("series((x^4+x+2)/(x^2+1),x=0, 5)")
@test evaluate(ex1) == evaluate(ex2)

ex1 = series(1/(1+x+y),[x,y],[0,0],5)
ex2 = giac("series(1/(1+x+y),[x,y],[0,0],5)")
@test evaluate(ex1) == evaluate(ex2)

ex1 = sum(1/n^2,n,1,17)
ex2 = giac("sum(1/n^2,n,1,17)")
@test evaluate(ex1) == evaluate(ex2)

ex1 = sum(1/n^2,n,1,17,2)
ex2 = giac("sum(1/n^2,n,1,17,2)")
@test evaluate(ex1) == evaluate(ex2)

ex1 = sum(1/n^2,n,1,infinity)
ex2 = giac("sum(1/n^2,n,1,infinity)")
@test evaluate(ex1) == evaluate(ex2)

ex1 = sum(cos(n*x),n)
ex2 = giac("sum(cos(n*x),n)")
@test evaluate(ex1) == evaluate(ex2)

ex1 = curl([2*x*y,x*z,y*z],[x,y,z])
ex2 = giac("curl([2*x*y,x*z,y*z],[x,y,z])")
@test evaluate(ex1) == evaluate(ex2)

ex1 = divergence([x^2+y,x+z+y,z^3+x^2],[x,y,z])
ex2 = giac("divergence([x^2+y,x+z+y,z^3+x^2],[x,y,z])")
@test evaluate(ex1) == evaluate(ex2)

ex1 = grad(2*x^2*y-x*z^3,[x,y,z])
ex2 = giac("grad(2*x^2*y-x*z^3,[x,y,z])")
@test evaluate(ex1) == evaluate(ex2)

ex1 = hessian(2*x^2*y-x*z,[x,y,z])
ex2 = giac("hessian(2*x^2*y-x*z,[x,y,z])")
@test evaluate(ex1) == evaluate(ex2)

ex1 = taylor(sin(x)/x,x)
ex2 = giac("taylor(sin(x)/x,x)")
@test evaluate(ex1) == evaluate(ex2)

ex1 = taylor(sin(x)/x,x=>0, 7)
ex2 = giac("taylor(sin(x)/x,x=0, 7)")
@test evaluate(ex1) == evaluate(ex2)

ex1 = solve(x^2-3⩦1)
ex2 = giac("solve(x^2-3=1)")
@test evaluate(ex1) == evaluate(ex2)

ex1 = solve([y-z⩦0,z-x⩦0,x-y⩦0,x-1+y+z⩦0],[x,y,z])
ex2 = giac("solve([y-z=0,z-x=0,x-y=0,x-1+y+z=0],[x,y,z])")
@test evaluate(ex1) == evaluate(ex2)

ex1 = zeros(x^2-4,x)
ex2 = giac("zeros(x^2-4,x)")
@test evaluate(ex1) == evaluate(ex2)

ex1 = zeros([x^2-1,x^2-y^2],[x,y])
ex2 = giac("zeros([x^2-1,x^2-y^2],[x,y])")
@test evaluate(ex1) == evaluate(ex2)

ex1 = cSolve(x^4-1⩦0, x)
ex2 = giac("cSolve(x^4-1=0, x)")
@test evaluate(ex1) == evaluate(ex2)

ex1 = cSolve([x^4-y^4,x+y⩦0,x^2⩦2*x],[x,y])
ex2 = giac("cSolve([x^4-y^4,x+y=0,x^2=2*x],[x,y])")
@test evaluate(ex1) == evaluate(ex2)

ex1 = cZeros(x^4-1, x)
ex2 = giac("cZeros(x^4-1, x)")
@test evaluate(ex1) == evaluate(ex2)

ex1 = cZeros([x^2-1,x^2-y^2],[x,y])
ex2 = giac("cZeros([x^2-1,x^2-y^2],[x,y])")
@test evaluate(ex1) == evaluate(ex2)

ex1 = fSolve(cos(x)⩦x,x=>0)
ex2 = giac("fsolve(cos(x)=x,x=0)")
#Note: renamed fsolve->fSolve, because that is the name the HP Prime uses
@test evaluate(ex1) == evaluate(ex2)

ex1 = deSolve(diff(y(x),x,x)+y(x)⩦0, y)
ex2 = giac("deSolve(diff(y(x),x,x)+y(x)=0, y)")
@test evaluate(ex1) == evaluate(ex2)

ex1 = deSolve(y''(x)+y(x)⩦0, y)
ex2 = giac("deSolve(y''+y=0, y)")
#Note: standalone y,y',y'' in this context not yet defined
@test evaluate(ex1) == evaluate(ex2)

ex1 = linsolve([x+y+z⩦1,x-y⩦2,2*x-z⩦3],[x,y,z])
ex2 = giac("linsolve([x+y+z=1,x-y=2,2*x-z=3],[x,y,z])")
@test evaluate(ex1) == evaluate(ex2)

ex1 = logcollect(log(x)+2*log(y))
ex2 = giac("lncollect(log(x)+2*log(y))")
#Note: renamed lncollect->logcollect because Giac/ln is Julia/log
@test evaluate(ex1) == evaluate(ex2)

ex1 = powexpand(2^(x+y))
ex2 = giac("powexpand(2^(x+y))")
@test evaluate(ex1) == evaluate(ex2)

ex1 = texpand(sin(2*x)+exp(x+y))
ex2 = giac("texpand(sin(2*x)+exp(x+y))")
@test evaluate(ex1) == evaluate(ex2)

ex1 = exp2pow(exp(3*log(x)))
ex2 = giac("exp2pow(exp(3*log(x)))")
@test evaluate(ex1) == evaluate(ex2)

ex1 = pow2exp(x^y)
ex2 = giac("pow2exp(x^y)")
@test evaluate(ex1) == evaluate(ex2)

ex1 = exp2trig(exp(im*x))
ex2 = giac("exp2trig(exp(i*x))")
#Note: Julia/im corresponds to Giac/i
@test evaluate(ex1) == evaluate(ex2)

ex1 = expexpand(exp(3*x+2*y))
ex2 = giac("expexpand(exp(3*x+2*y))")
@test evaluate(ex1) == evaluate(ex2)

ex1 = asin2acos(acos(x)+asin(x))
ex2 = giac("asin2acos(acos(x)+asin(x))")
@test evaluate(ex1) == evaluate(ex2)

ex1 = asin2atan(2*asin(x))
ex2 = giac("asin2atan(2*asin(x))")
@test evaluate(ex1) == evaluate(ex2)

ex1 = sin2costan(sin(x))
ex2 = giac("sin2costan(sin(x))")
@test evaluate(ex1) == evaluate(ex2)

ex1 = acos2asin(acos(x)+asin(x))
ex2 = giac("acos2asin(acos(x)+asin(x))")
@test evaluate(ex1) == evaluate(ex2)

ex1 = acos2atan(2*acos(x))
ex2 = giac("acos2atan(2*acos(x))")
@test evaluate(ex1) == evaluate(ex2)

ex1 = cos2sintan(cos(x))
ex2 = giac("cos2sintan(cos(x))")
@test evaluate(ex1) == evaluate(ex2)

ex1 = atan2asin(atan(2*x))
ex2 = giac("atan2asin(atan(2*x))")
@test evaluate(ex1) == evaluate(ex2)

ex1 = atan2acos(atan(2*x))
ex2 = giac("atan2acos(atan(2*x))")
@test evaluate(ex1) == evaluate(ex2)

ex1 = tan2sincos(tan(x))
ex2 = giac("tan2sincos(tan(x))")
@test evaluate(ex1) == evaluate(ex2)

ex1 = halftan(sin(x))
ex2 = giac("halftan(sin(x))")
@test evaluate(ex1) == evaluate(ex2)

ex1 = trigsin(cos(x)^4+sin(x)^2)
ex2 = giac("trigsin(cos(x)^4+sin(x)^2)")
@test evaluate(ex1) == evaluate(ex2)

ex1 = trigcos(cos(x)^4+sin(x)^2)
ex2 = giac("trigcos(cos(x)^4+sin(x)^2)")
@test evaluate(ex1) == evaluate(ex2)

ex1 = trigtan(cos(x)^4+sin(x)^2)
ex2 = giac("trigtan(cos(x)^4+sin(x)^2)")
@test evaluate(ex1) == evaluate(ex2)

ex1 = atrig2log(atan(x))
ex2 = giac("atrig2ln(atan(x))")
#Note: renamed atrig2ln->atrig2log because Giac/ln is Julia/log
@test evaluate(ex1) == evaluate(ex2)

ex1 = tlin(sin(x)^3)
ex2 = giac("tlin(sin(x)^3)")
@test evaluate(ex1) == evaluate(ex2)

ex1 = tcollect(sin(x)+cos(x))
ex2 = giac("tcollect(sin(x)+cos(x))")
@test evaluate(ex1) == evaluate(ex2)

ex1 = trigexpand(sin(3*x))
ex2 = giac("trigexpand(sin(3*x))")
@test evaluate(ex1) == evaluate(ex2)

ex1 = trig2exp(sin(x))
ex2 = giac("trig2exp(sin(x))")
@test evaluate(ex1) == evaluate(ex2)

# to be continued ...
