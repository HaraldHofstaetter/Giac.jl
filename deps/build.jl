if !Sys.iswindows()
    try # check if giac installed
        kill(run(pipeline(`giac`, stdin=devnull, stdout=devnull, stderr=stderr), wait=false))
    catch # build
        using Libdl

        cd(dirname(@__FILE__))

        if (!ispath("lib"))
          run(`mkdir lib`)
        end

        cd("src")
        run(`make`)
        run(`mv libgiac_c.$(dlext) ../lib`)
    end
end
