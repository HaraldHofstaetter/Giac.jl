import PyPlot.plot


function plot(ex::Gen, var, a, b)
    xx= giac(:plot, giac(Any[ex, var, evalf(giac(a)), evalf(giac(b))], subtype=1))
    xy=to_julia(args(args(xx)[1])[2])
    plot(real(xy), imag(xy))
end

