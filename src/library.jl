a2q(A,X) = giac(:a2q,A,X)
abcuv(a,b,c) = giac(:abcuv,a,b,c)
abcuv(a,b,c,var) = giac(:abcuv,a,b,c,var)
adjoint_matrix(A) = giac(:adjoint_matrix, A)

cot(x::giac) = giac(:cot, x)
sec(x::giac) = giac(:sec, x)
csc(x::giac) = giac(:csc, x)
#sech(x::giac) = giac(:sech, x) # not available in Giac
#csch(x::giac) = giac(:csch, x) # not available in Giac
asec(x::giac) = giac(:asec, x)
acsc(x::giac) = giac(:acsc, x)
acot(x::giac) = giac(:acot, x)

sign(x::giac) = giac(:sign,x)
log(::Irrational{:e}, x::Giac.giac) = log(x)
log(base, x::giac)  = giac(:logb, x, base)
log10(x::giac) = log(10, x)
round(x::giac) = giac(:round, x)
ceil(x::giac) = giac(:ceil, x)
floor(x::giac) = giac(:floor, x)
trunc(x::giac) = giac(:trunc, x)
factorial(x::giac) = giac(:factorial, x)
binomial(x::giac, y::giac) = giac(:binomial, x, y)
binomial(x::giac, y) = giac(:binomial, x, y)
binomial(x, y::giac) = giac(:binomial, x, y)
expm1(x::giac) = giac(:EXPM1, x)
log1p(x::giac) = giac(:LNP1, x)
Ei(x::giac) = giac(:Ei, x)
Si(x::giac) = giac(:Si, x)
Ci(x::giac) = giac(:Ci, x)

erf(x::giac) = giac(:erf, x)
erfc(x::giac) = giac(:erfc, x)
gamma(x::giac) = giac(:Gamma, x) #  Giac/Gamma -> Julia/gamma
beta(x::giac, y::giac) = giac(:Beta, x, y) # Giac/Beta -> Julia/beta
beta(x::giac, y) = giac(:Beta, x, y) # Giac/Beta -> Julia/beta
beta(x, y::giac) = giac(:Beta, x, y) # Giac/Beta -> Julia/beta
zeta(x::giac) = giac(:Zeta, x)
airyai(x::giac) = giac(:Airy_Ai, x) # Giac/Airy_Ai does not work as expected
airybi(x::giac) = giac(:Airy_Bi, x)  # Giac/Airy_Bi does not work as expected
besselj(nu,x::giac) = giac(:BesselJ, nu, x) # Giac/BesselJ -> Julia/besselj
bessely(nu,x::giac) = giac(:BesselY, nu, x) # Giac/BesselY -> Julia/bessely




evaluate(x) = x 
evaluatef(x) = x 
simplify(x) = x 

evaluate(ex::giac) = giac(:eval, ex)
evaluatef(ex::giac) = giac(:evalf, ex)
evalf = evaluatef
simplify(ex::giac) = giac(:simplify, ex)
factor(ex::giac) = giac(:factor, ex)
expand(ex::giac) = giac(:expand, ex)
collect(ex::giac) = giac(:collect, ex)
collect(exs::Array{giac_SYMB}) = giac(:collect, giac(exs))
factor(ex::giac, sr)= giac(:factor, ex, sr)
diff(ex::giac, vars...) = giac(:derive, ex, vars...)
diff(ex::giac, vars::Array) = giac(:derive, ex, vars)
partfrac(ex) = giac(:partfrac, ex)
partfrac(ex, var) = giac(:partfrac, ex, var)
equal(left, right) = giac(:equal, left, right)
⩦(a,b) = equal(a, b)
right(eq) = giac(:right, eq)
left(eq) = giac(:left, eq)
denominator(ex::giac) = giac(:denom, ex) #Giac/denom -> Julia/denominator
numerator(ex::giac) = giac(:numer, ex) #Giac/numer -> Julia/numerator

subst(ex, var, val) = giac(:subst, ex, var, val)
subst(ex, sp::Pair...) = subst(ex, [v for (v,w) in sp], [w for (v,w) in sp])

limit(ex, var, val) = giac(:limit, ex, var, val)
limit(ex, var_val::Pair) = giac(:limit, ex, var_val[1], var_val[2])
limit(ex, var, val, dir) = giac(:limit, ex, var, val, dir)
limit(ex, var_val::Pair, dir) = giac(:limit, ex, var_val[1], var_val[2], dir)
series(ex, var, val) = giac(:series, ex, var, val)
series(ex, var, val, order) = giac(:series, ex, var, val, order)
series(ex, var_val::Pair) = giac(:series, ex, var_val[1], var_val[2])
series(ex, var_val::Pair, order) = giac(:series, ex, var_val[1], var_val[2], order)
series(ex, var, val, order, dir) = giac(:series, ex, var, val, order, dir)
series(ex, var_val::Pair, order, dir) = giac(:series, ex, var_val[1], var_val[2], order, dir)
sum(ex::giac) = giac(:sum, ex)
sum(ex::giac, x::giac) = giac(:sum, ex, x)
sum(ex::giac, x::giac, a::Union{giac,Number}, b::Union{giac,Number}) = giac(:sum, ex, x, a, b)
sum(ex::giac, x::giac, a::Union{giac,Number}, b::Union{giac,Number}, step::Union{giac,Number}) = 
    giac(:sum, ex, x, a, b, step)
curl(exs, vars) = giac(:curl, exs, vars)
divergence(exs, vars) = giac(:divergence, exs, vars)
grad(ex, vars) = giac(:grad, ex, vars)
hessian(ex, vars) = giac(:hessian, ex, vars)
solve(eqs, var) = giac(:solve, eqs, var)
preval(ex, a, b) = giac(:preval, ex, a, b)
preval(ex, a, b, var) = giac(:preval, ex, a, b, var)
sum_riemann(expr, vars) = giac(:sum_riemann, expr, vars)
taylor(ex, var) = giac(:taylor, ex, var)
taylor(ex, var, order) = giac(:taylor, ex, var, order)
taylor(ex, var_val::Pair) = giac(:taylor, ex, var_val[1], var_val[2])
taylor(ex, var_val::Pair, order) = giac(:taylor, ex, var_val[1], order, var_val[2])
divpc(p1, p2, order) = giac(:divpc, p1, p2, order)
solve(eq) = giac(:solve, eq)
cSolve(eq) = giac(:cSolve, eq)
cSolve(eqs, var) = giac(:cSolve, eqs, var)
zeros(ex::giac) = giac(:zeros, ex)
zeros(exs, var) = giac(:zeros, exs, var)
cZeros(eq::giac) = giac(:cZeros, eq)
cZeros(exs, var) = giac(:cZeros, exs, var)
fSolve(eqs, vars, guess) = giac(:fsolve, eqs, vars, guess)
fSolve(eqs, var_guess::Pair) = giac(:fsolve, eqs, var_guess[1], var_guess[2])
fSolve(eqs, vars) = giac(:fsolve, eqs, vars)
fSolve(eqs) = giac(:fsolve, eqs)
deSolve(eq, var) = giac(:desolve, eq, var)
deSolve(eq, timevar, var) = giac(:desolve, eq, timevar, var)
linsolve(eqs, vars) = giac(:linsolve, eqs, vars)

logcollect(ex) = giac(:lncollect, ex)
powexpand(ex) = giac(:powexpand, ex)
texpand(ex) = giac(:texpand, ex)
exp2pow(ex) = giac(:exp2pow, ex)
pow2exp(ex) = giac(:pow2exp, ex)
exp2trig(ex) = giac(:exp2trig, ex)
expexpand(ex) = giac(:expexpand, ex)
asin2acos(ex) = giac(:asin2acos, ex)
asin2atan(ex) = giac(:asin2atan, ex)
sin2costan(ex) = giac(:sin2costan, ex)
acos2asin(ex) = giac(:acos2asin, ex)
acos2atan(ex) = giac(:acos2atan, ex)
cos2sintan(ex) = giac(:cos2sintan, ex)
atan2asin(ex) = giac(:atan2asin, ex)
atan2acos(ex) = giac(:atan2acos, ex)
tan2sincos(ex) = giac(:tan2sincos, ex)
halftan(ex) = giac(:halftan, ex)
trigsin(ex) = giac(:trigsin, ex)
trigcos(ex) = giac(:trigcos, ex)
trigtan(ex) = giac(:trigtan, ex)
atrig2log(ex) = giac(:atrig2ln, ex)
tlin(ex) = giac(:tlin, ex)
tcollect(ex) = giac(:tcollect, ex)
trigexpand(ex) = giac(:trigexpand, ex)
trig2exp(ex) = giac(:trig2exp, ex)

gbasis(polys, vars) = giac(:gbasis, polys, vars)
greduce(poly, basis, vars) = giac(:greduce, poly, basis, vars)


integrate(ex::giac) = giac(:integrate, ex)
integrate(ex::giac, var::giac) = giac(:integrate, ex, var)
integrate(ex::giac, var::giac, a::Union{giac,Number}, b::Union{giac,Number}) =
    giac(:integrate, ex, var, a, b)
    
head(ex::giac) = giac(:sommet, ex)
args(ex::giac) = giac(:feuille, ex)

# to be continued ...
