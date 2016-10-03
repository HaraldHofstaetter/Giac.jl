cd(dirname(@__FILE__))

if (!ispath("lib"))
    run(`mkdir lib`)
end


cd(joinpath(dirname(@__FILE__), "src"))

run(`make`)
run(`mv libgiac_c.$(Libdl.dlext) ../lib`
