# Giac

Julia interface to the [Giac computer algebra system](http://www-fourier.ujf-grenoble.fr/~parisse/giac.html).

##Installation
```julia
Pkg.clone("https://github.com/HaraldHofstaetter/Giac.jl")
Pkg.build("Giac")
```
In the general case (i.e., not inside a JuliaBox) the sources of the Giac C++ library are downloaded from  http://www-fourier.ujf-grenoble.fr/~parisse/giac/, from which then the library is built. 
In a [JuliaBox](https://juliabox.com/), however, precompiled binaries of the library are downloaded from http://www.harald-hofstaetter.at/Giac/. (Due to the limited available disk space it is not possible to build these libraries from the sources inside a JuliaBox.)
##Examples
To get easy access to the examples, copy them into the home directory:
```julia
cp(joinpath(homedir(), ".julia/v0.4/Giac/examples/"), joinpath(homedir(), "Giac_examples"), remove_destination=true)
```
Then 'Giac_examples' will be listed in the JuliaBox home screen. The examples contain among others
+ [Giac_basics.ipynb](https://github.com/HaraldHofstaetter/Giac.jl/blob/master/examples/Giac_basics.ipynb)
+ [Giac_examples.ipynb](https://github.com/HaraldHofstaetter/Giac.jl/blob/master/examples/Giac_examples.ipynb)
