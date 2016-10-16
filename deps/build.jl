DOWNLOAD_BINARIES = searchindex(readall(`uname -a`), "juliabox")>0

cd(dirname(@__FILE__))

if (!ispath("lib"))
    run(`mkdir lib`)
end

if DOWNLOAD_BINARIES

   cd(joinpath(dirname(@__FILE__), "lib"))
   download("https://github.com/HaraldHofstaetter/Giac.jl/releases/download/0.1/libgiac.tgz", 
            "./libgiac.tgz")
   run(`tar xzvf libgiac.tgz`)
   run(`rm libgiac.tgz`)

else # build from sources

if (!ispath("giac-1.2.2"))
   download("https://www-fourier.ujf-grenoble.fr/~parisse/giac/giac-1.2.2.tar.gz", "./giac-1.2.2.tar.gz")
   run(`tar xzvf giac-1.2.2.tar.gz`)
   cd(joinpath(dirname(@__FILE__), "giac-1.2.2"))
   prefix = dirname(@__FILE__)
   withenv("CXX"=>"g++-4.9", "CFLAGS"=>"-O2", "CXXFLAGS"=>"-O2") do # otherwise -g included which needs too much diskspace
      run(`./configure --prefix=$prefix --disable-debug  --disable-gui --disable-static --disable-nls --disable-ao`)
   end   
   cd(joinpath(dirname(@__FILE__), "giac-1.2.2/src"))
   run(`make`)
   run(`make install`)
end   

cd(joinpath(dirname(@__FILE__), "src"))
run(`make`)
run(`mv libgiac_c.$(Libdl.dlext) ../lib`)

end
