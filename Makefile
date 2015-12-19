FC = gfortran
FFLAGS = -O2
LDFLAGS = 
EXEC = scanner

OBJECTS = $(patsubst %.f90, %.o, $(wildcard *.f90))

all : $(EXEC)

$(EXEC) : $(OBJECTS)
	$(FC) $(LDFLAGS) -o $@ $^

%.o : %.f90
	$(FC) $(FFLAGS) -o $@ -c $<

main.o : lexer.o

clean :
	@rm -fv *.o *.mod $(EXEC)

