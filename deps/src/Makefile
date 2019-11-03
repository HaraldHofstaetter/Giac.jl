all: shared_lib

CFLAGS =  -O2  -DHAVE_LIBMPFR  -D_GLIBCXX_USE_CXX11_ABI=0
#Note: for the -D_GLIBCXX_USE_CXX11_ABI=0 option see
#https://stackoverflow.com/questions/33394934/converting-std-cxx11string-to-stdstring
CPP = g++ $(CFLAGS)
INCLUDE = /usr/include
LIBS = /usr/lib

OBJS = giac_c.o giac_funs.o

shared_lib: libgiac_c.so


giac_c.o: giac_c.C
	$(CPP) -I$(INCLUDE) -I. -fpic  -c $<

giac_funs.C: gen_giac_funs.jl giac_funs.txt
	julia  gen_giac_funs.jl > giac_funs.C

giac_funs.o: giac_funs.C
	$(CPP) -I$(INCLUDE) -I. -fpic  -c $<


libgiac_c.so: $(OBJS)
	$(CPP) $(OBJS) -shared -o $@ -L$(LIBS) -lgiac


clean:
	rm -f *.o *.so
