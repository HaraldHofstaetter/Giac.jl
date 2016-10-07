# Giac

Julia interface to the [Giac computer algebra system](http://www-fourier.ujf-grenoble.fr/~parisse/giac.html).

##Installation
```julia
Pkg.clone("https://github.com/HaraldHofstaetter/Giac.jl")
Pkg.build("Giac")
```
##Examples
To get easy access to the examples, copy them into the home directory:
```julia
cp(joinpath(homedir(), ".julia/v0.4/Giac/examples/"), joinpath(homedir(), "Giac_examples"), remove_destination=true)
```
Then 'Giac_examples' will be listed in the JuliaBox home screen. The examples contain among others
+ [Giac.ipynb](https://github.com/HaraldHofstaetter/Giac.jl/blob/master/examples/Giac.ipynb)
