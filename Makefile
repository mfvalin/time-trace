#
# (tested on Ubuntu 14.04 and Ubuntu 18.04)
#
CCOMP = gcc

FCOMP = gfortran

MPIFCOMP = mpif90

FFLAGS = 

CFLAGS =

OBJECTS =  time_trace_c.o time_trace_f.o

LIB = libtimetrace.a

all:	lib tests

tests: trace_test_f.Abs trace_test_c.Abs

time_trace_c.o: time_trace_c.c time_trace.h
	$(CCOMP) $(CFLAGS) -I. -c time_trace_c.c

time_trace_f.o: time_trace_f.F90 time_trace.h time_trace.hf
	$(FCOMP) $(CFLAGS) -I. -c time_trace_f.F90

lib:	$(OBJECTS)
	ar rcv $(LIB) $(OBJECTS)

trace_test_f_mpi:
	$(MPIFCOMP) $(FFLAGS) -DSELF_TEST time_trace.F90 -o $@

trace_test_f.Abs: time_trace_c.c time_trace_f.F90 time_trace.h time_trace.hf
	$(CCOMP) $(CFLAGS) -I. -c -DSELF_TEST time_trace_c.c
	$(FCOMP) $(FFLAGS) -I. -DSELF_TEST -DNO_MPI time_trace_f.F90 time_trace_c.o -o $@

trace_test_c.Abs: time_trace_c.c time_trace.h
	$(CCOMP) $(CFLAGS) -I. -DSELF_TEST -DC_SELF_TEST time_trace_c.c -o $@

clean:	
	rm -f *.o *.mod time_list_*.txt time_list_*.dat *.Abs libtimetrace.a
