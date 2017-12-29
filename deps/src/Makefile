all: shared_lib

CFLAGS =  -O2 -DHAVE_CONFIG_H 
CC = gcc-5 $(CFLAGS)
CPP = g++-5 $(CFLAGS)

LIBDIR = -L../lib
INCDIR = -I../include/giac
LIBS = ../lib/libgiac.so


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
	$(CPP) -fpic $(INCDIR)  -c $<

giac_funs.C: gen_giac_funs.jl giac_funs.txt
	julia  gen_giac_funs.jl > giac_funs.C

giac_funs.o: giac_funs.C
	$(CPP) -fpic $(INCDIR)  -c $<


libgiac_c.$(SHLIB_EXT): $(OBJS)
	$(CPP) $(OBJS) -rdynamic -shared -o $@ $(LIBDIR) $(LIBS)


clean:
	rm -f *.o *.$(SHLIB_EXT)        
