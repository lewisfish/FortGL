ifeq ($(FCOMP),gfortran)
FCFLAGS = -O2 -cpp -freal-4-real-8 -march=native -ftree-vectorize -flto -mavx -ffast-math -funroll-loops
FCDEBUG = -g -fbacktrace -fcheck=all -fbounds-check -ffpe-trap=invalid,overflow,underflow,denormal
FCBUILD = -Wall -Wextra -pedantic
else ifeq ($(FCOMP),ifort)
FCFLAGS = -O2 -r8 -xHOST -ipo
FCDEBUG = -g -debug -traceback -check all -CB -CU -CA -fpe0 -debug
FCBUILD = -warn all -stand f08
endif

PROGRAM =  fortgl

SRCS =		utils.f90 \
			Image_mod.f90 \
			types.f90 \
			shapeclass.f90 \
			triangle.f90 \
			plyread.f90 \
			objread.f90 \
			camera.f90 \
			shader.f90 \
			render.f90 \
			main.f90 
                    
OBJECTS = $(SRCS:.f90=.o)

all:	$(PROGRAM)
debug:	FCFLAGS += $(FCDEBUG)
debug:	$(PROGRAM)
build:	FCFLAGS += $(FCBUILD)
build:	$(PROGRAM)

$(PROGRAM):	$(OBJECTS)
	$(FCOMP) $(FCFLAGS) -o $@ $^ 

%.o:  %.f90
	$(FCOMP)  $(FCFLAGS) -c $<

.PHONY:	clean

clean:
	rm -f *.o *.mod *.MOD fortgl
