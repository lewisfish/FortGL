FCOMP = gfortran
FCFLAGS = -O2 -freal-4-real-8
FCDEBUG = -g -fbacktrace -fcheck=all -fbounds-check -ffpe-trap=invalid,overflow,underflow,denormal

PROGRAM =  fortgl

SRCS =		Image_mod.f90 \
			types.f90 \
			shapeclass.f90 \
			triangle.f90 \
			objread.f90 \
			Draw_mod.f90 \
			main.f90 
                    
OBJECTS = $(SRCS:.f90=.o)

all:	$(PROGRAM)
debug:	FCFLAGS += $(FCDEBUG)
debug:	$(PROGRAM)

$(PROGRAM):	$(OBJECTS)
	$(FCOMP) $(FCFLAGS) -o $@ $^ 

%.o:  %.f90
	$(FCOMP)  $(FCFLAGS) -c $<

.PHONY:	clean

clean:
	rm -f *.o *.mod *.MOD fortgl
