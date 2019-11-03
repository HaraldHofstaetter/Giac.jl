using Libdl

cd(dirname(@__FILE__))

if (!ispath("lib"))
  run(`mkdir lib`)
end

cd("src")
run(`make`)
run(`mv libgiac_c.$(dlext) ../lib`)

