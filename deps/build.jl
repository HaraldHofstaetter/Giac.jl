DOWNLOAD_BINARIES = false # searchindex(readall(`uname -a`), "juliabox")>0

cd(dirname(@__FILE__))

if DOWNLOAD_BINARIES
   if (!ispath("lib"))
      run(`mkdir lib`)
   end

   cd(joinpath(dirname(@__FILE__), "lib"))
   download("https://github.com/HaraldHofstaetter/Giac.jl/releases/download/0.1/libgiac.tgz", 
            "./libgiac.tgz")
   run(`tar xzvf libgiac.tgz`)
   run(`rm libgiac.tgz`)

else # build from sources

giac_version = "giac-1.2.2"

if !ispath(giac_version)
   download("https://www-fourier.ujf-grenoble.fr/~parisse/giac/$(giac_version).tar.gz", "./$(giac_version).tar.gz")
   run(`tar xzvf $(giac_version).tar.gz`)
   cd(joinpath(dirname(@__FILE__), giac_version))
   prefix = dirname(@__FILE__)
   withenv("CXX"=>"g++-5", "CFLAGS"=>"-O2", "CXXFLAGS"=>"-O2") do # otherwise -g included which needs too much diskspace
      run(`./configure --prefix=$prefix --disable-debug  --disable-gui --disable-static --disable-nls --disable-ao`)
   end   
   cd("src")
   run(`make`)
   run(`make install`)
end   

cd(dirname(@__FILE__))
cp("$(giac_version)/config.h", "include/giac/config.h", remove_destination=true)
cd("src")
run(`make`)
run(`mv libgiac_c.$(Libdl.dlext) ../lib`)

end
