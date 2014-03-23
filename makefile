
COMPILADOR = gfortran
FLAGS = -o

all: memory

# ************ Compilación de programas ************
memory : memory.f90
	$(COMPILADOR) $(FLAGS) memory memory.f90

# ************ Limpieza ************
clean :
	-rm ./*~
	-rm memory
