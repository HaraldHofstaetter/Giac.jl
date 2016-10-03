all: shared_lib

CFLAGS = #-g 
CC = gcc-4.9 $(CFLAGS)
CPP = g++-4.9 $(CFLAGS)

LIBDIR =
INCDIR = -I /usr/include/giac
LIBS = -lgiac


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


OBJS = giac_c.o

shared_lib: libgiac_c.$(SHLIB_EXT)


giac_c.o: giac_c.C
	$(CPP) -fpic $(INCDIR)  -c $<

libgiac_c.$(SHLIB_EXT): $(OBJS)
	$(CPP) $(OBJS) -rdynamic -shared -o $@ $(LIBDIR) $(LIBS)

clean:
	rm -f *.o *.$(SHLIB_EXT)        
