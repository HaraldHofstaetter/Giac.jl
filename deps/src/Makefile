all: shared_lib

CFLAGS =  -O2  -DHAVE_LIBMPFR  
#Note: linking errors with newer versions of g++ ...
CPP = g++-4.8 $(CFLAGS)

# Figure out OS and architecture
OS = $(shell uname)
ifeq ($(OS), MINGW32_NT-6.1)
  OS=WINNT
endif

# file extensions
ifeq ($(OS), WINNT)
  SHLIB_EXT = dll 
else ifeq ($(OS), Darwin)
  SHLIB_EXT = dylib
else
  SHLIB_EXT = so
endif


OBJS = giac_c.o giac_funs.o

shared_lib: libgiac_c.$(SHLIB_EXT)


giac_c.o: giac_c.C
	$(CPP) -fpic  -c $<

giac_funs.C: gen_giac_funs.jl giac_funs.txt
	julia  gen_giac_funs.jl > giac_funs.C

giac_funs.o: giac_funs.C
	$(CPP) -fpic  -c $<

libgiac_c.$(SHLIB_EXT): $(OBJS)
	$(CPP) $(OBJS) -shared -o $@ -lgiac


clean:
	rm -f *.o *.$(SHLIB_EXT)        
