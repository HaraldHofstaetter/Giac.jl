cd(dirname(@__FILE__))

if (!ispath("lib"))
    run(`mkdir lib`)
end

if (!ispath("giac-1.2.2"))
   download("https://www-fourier.ujf-grenoble.fr/~parisse/giac/giac-1.2.2.tar.gz", "./giac-1.2.2.tar.gz")
   run(`tar xzvf giac-1.2.2.tar.gz`)
   cd(joinpath(dirname(@__FILE__), "giac-1.2.2"))
   prefix = dirname(@__FILE__)
   run(`./configure --prefix=$prefix --enable-debug --disable-gui --disable-static`)
   run(`make`)
   run(`make install`)
end   

cd(joinpath(dirname(@__FILE__), "src"))
run(`make`)
run(`mv libgiac_c.$(Libdl.dlext) ../lib`)
