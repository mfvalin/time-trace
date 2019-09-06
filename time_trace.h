#if 0
//  Hopefully useful software for FORTRAN and C
//  Copyright (C) 2019  Division de Recherche en Prevision Numerique
//                      Environnement Canada
// 
//  This is free software; you can redistribute it and/or
//  modify it under the terms of the GNU Lesser General Public
//  License as published by the Free Software Foundation,
//  version 2.1 of the License.
// 
//  This software is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//  Lesser General Public License for more details.
#endif

#if defined SELF_TEST
#define MAX_TIMES  7
#else
#define MAX_TIMES 1024
#endif

#if defined(FORTRAN_SOURCE)
type, BIND(C) :: time_context  ! wrappper to give a type to a C pointer
  type(C_PTR) :: t
end type

type, bind(C) :: timeval
  integer(C_LONG_LONG) :: sec
  integer(C_LONG_LONG) :: usec
end type

#else
typedef struct{
  void *t;
}time_context;
#endif

#if ! defined(FORTRAN_SOURCE)
#if defined(NEED_PRIVATE)
typedef struct{
  void *next;
  int nbent;
  int mxent;
  unsigned int t[MAX_TIMES];
}bead;

typedef struct{
  void *first;
  void *last;
  unsigned long long offset;
  int initialized;
  int step;
  int nbeads;
  int nwords;
  int major;
  int minor;
}trace_table;
#endif
#endif

#if defined(FORTRAN_SOURCE)
interface
#if ! defined(NEED_PRIVATE)
  subroutine time_trace_dump_text(t, filename, ordinal)   ! dump timings int file filename_nnnnnn.txt (nnnnnn from ordinal)
    import :: time_context
    implicit none
    type(time_context), intent(IN) :: t              ! opaque time context pointer (from time_trace_init)
    character(len=*), intent(IN) :: filename         ! file name prefix (will be trimmed to remove trailing blanks if any)
    integer, intent(IN) :: ordinal                   ! numbered extension to file name (nnnnnn) (normally MPI rank)
  end subroutine time_trace_dump_text

  subroutine time_trace_dump_binary(t, filename, ordinal)   ! dump timings int file filename_nnnnnn.dat (nnnnnn from ordinal)
    import :: time_context
    implicit none
    type(time_context), intent(IN) :: t              ! opaque time context pointer
    character(len=*), intent(IN) :: filename         ! file name prefix (will be trimmed to remove trailing blanks if any)
    integer, intent(IN) :: ordinal                   ! numbered extension to file name (nnnnnn) (normally MPI rank)
  end subroutine time_trace_dump_binary

  subroutine time_trace_barr(t, tag, barrier, comm, barrier_code)  ! insert a new time trace entry (2 entries if barrier is true)
    import :: time_context
    type(time_context), intent(IN) :: t              ! opaque time context pointer (from time_trace_init)
    integer, intent(IN) :: tag                       ! tag number for this timing point (MUST be >0 and <99999999)
    logical, intent(IN) :: barrier                   ! if true, call MPI_barrier with timing points before and after
    integer, intent(IN) :: comm                      ! MPI communicator (only used if barrier flag is true)
    external :: barrier_code
  end subroutine time_trace_barr
#endif

#if defined(NEED_PRIVATE)
  subroutine new_time_tag(t, tag, t1, t2) bind(C,name='TimeTraceNewTag') ! insert tag and 1 or 2 time deltas
    import :: time_context, C_INT, C_LONG_LONG
    type(time_context), intent(IN), value :: t              ! opaque time context pointer
    integer(C_INT), intent(IN),value :: tag
    integer(C_LONG_LONG), intent(IN), value :: t1, t2
  end subroutine new_time_tag

  function what_time_is_it() result (t) bind(C,name='WallClockTime')
    import :: C_LONG_LONG
    INTEGER(C_LONG_LONG) t
  end function what_time_is_it
#endif

  function get_time_of_day(tv,tz) result(time) BIND(C,name='gettimeofday') ! interface to C library function gettimeofday
    import :: timeval, C_PTR, C_INT
    implicit none
    type(timeval), intent(OUT)     :: tv
    type(C_PTR), value, intent(IN) :: tz
    integer(C_INT) :: time
  end function get_time_of_day

  subroutine time_trace_init(t) bind(C,name='TimeTraceInit')! create and initialize a new time trace context
    import :: time_context
    implicit none
    type(time_context), intent(OUT) :: t                    ! opaque time context pointer (passed to other routines)
  end subroutine time_trace_init

  subroutine time_trace_step(t, step) bind(C,name='TimeTraceStep')  ! set step value for subsequent calls to time_trace
    import :: time_context, C_INT
    implicit none
    type(time_context), intent(IN), value :: t              ! opaque time context pointer (from time_trace_init)
    integer(C_INT), intent(IN), value :: step               ! time step number (MUST be >0 and <99999999)
  end subroutine time_trace_step

  subroutine time_trace(t, tag)  bind(C,name='TimeTrace') ! insert a new time trace entry (no barrier)
    import :: time_context, C_INT
    implicit none
    type(time_context), intent(IN), value :: t              ! opaque time context pointer (from time_trace_init)
    integer(C_INT), intent(IN), value :: tag                ! tag number for this timing point (MUST be >0 and <99999999)
  end subroutine time_trace

  subroutine time_trace_get_buffers(t, array, larray, n) bind(C,name='TimeTraceGetBuffers')
    import :: C_PTR , C_INT, time_context
    implicit none
    type(time_context), intent(IN), value :: t              ! opaque time context pointer (passed to other routines)
    type(C_PTR), dimension(n), intent(OUT) :: array         ! to receive pointers to buffers
    integer(C_INT), dimension(n), intent(OUT) :: larray     ! to receive lengths of buffers
    integer(C_INT), intent(IN), value :: n                  ! size of array and larray 
  end subroutine time_trace_get_buffers

end interface
#else
#if defined(NEED_PRIVATE)
  void *TimeTraceCreate();
  void TimeTraceCreateNewBead(time_context t);
  void TimeTraceInsert(time_context t, int val);
  void TimeTraceNewTag(time_context t, int tag, long long t1, long long t2);
  void TimeTraceNewStep(time_context t, int step);
#endif
  void TimeTraceGetBuffers(time_context t, void **array, int *larray, int n);
  void TimeTraceInit(time_context *t);
  void TimeTraceStep(time_context t, int step);
  void TimeTrace(time_context t, int tag);
  void TimeTraceBarr(time_context t, int tag, int barrier, int fcomm, void (*barrier_code)());
  void TimeTraceDump(time_context t, char *filename, int ordinal);
  void TimeTraceDumpBinary(time_context t, char *filename, int ordinal);
#endif

#if defined(FORTRAN_SOURCE)
#else
#endif

