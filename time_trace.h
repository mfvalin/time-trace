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
//
//****P* libtimetrace/set of timing routines for Fortran and C  
*/
// DESCRIPTION
//  This package contains functions to get the wall clock time, the time of the day, the number of machine cpu_clock_cycles
//  from an arbitrary time point in the past (some functions may only work on amd64 architecture CPUs).
//
//  This package also contains a set of functions to perform a "time trace" of an application.
//
//  Post-processing tools will eventually be created to help interpreting these time traces.
//
//  In the case of a MPI program, there would normally be one time trace per process, unless the application only calls
//  the time trace functions on a reduced set of processes.
//
// EXAMPLES
int main(){        // non MPI C example of use
  time_context t;
  int i, tag;
  void *array[100];
  int larray[100];

  printf("C Test of time trace \n");
  TimeTraceInit(&t);
  printf("-------------------------\n");
  TimeTrace(t, 0);
  TimeTraceStep(t, 0);
  printf("=========================\n");
  TimeTraceBarr(t, 1, 1, 0, NULL);
  for(i=1 ; i<4 ; i++){
    TimeTraceStep(t, i);
    printf("++++ start step %d ++++\n",i);
    tag = 10*i+1;
    TimeTraceBarr(t, tag, 1, 0, NULL);
    printf("++++ end step %d ++++\n",i);
    tag = 10*i+2;
    TimeTrace(t, tag);
  }
  TimeTraceStep(t, 4);
  printf("=========================\n");
   TimeTrace(t, 77777777);
   TimeTraceDumpText(t, "time_list", 0);
   TimeTraceDumpBinary(t, "time_list", 0);
   TimeTraceGetBuffers(t, array, larray, 10);
   for(i=0 ; i<10 ; i++) printf("%d ",larray[i]);
   printf("\n");
  return 0;
}
program test_trace      ! MPI example of use in Fortran
  use ISO_C_BINDING
  implicit none
#include <time_trace.hf>
  include 'mpif.h'
  integer :: ierr
  integer :: i, tag, rank
  type(time_context) :: t
  type(C_PTR), dimension(10) :: array
  integer(C_INT), dimension(10) :: larray
  external :: MPI_barrier

  rank = 0
  call MPI_init(ierr)
  call MPI_comm_rank(MPI_COMM_WORLD, rank, ierr)
  call time_trace_init(t)
  print *,'-----------------------------'
  call time_trace(t, 0)
  call time_trace_step(t, 0)
  print *,'============================='
  call time_trace_barr(t, 1, .true., MPI_COMM_WORLD, MPI_barrier)
  do i = 1, 3
    call time_trace_step(t, i)
    print *,'+++++++ start step =',i
    tag = 10*i+1
    call time_trace_barr(t, tag, .true., MPI_COMM_WORLD, MPI_barrier)
    print *,'+++++++ end   step =',i
    tag = 10*i+2
    call time_trace(t, tag)
  enddo
  call time_trace_step(t, 4)
  print *,'============================='
  call time_trace(t, 999999)
  call time_trace_dump_text(t, 'time_list', rank)
  call time_trace_dump_binary(t, 'time_list', rank)
  call time_trace_get_buffers(t, array, larray, 10)
  write(6,'(10I6)')larray
  call MPI_finalize(ierr)

  stop
end program
//**** 
*/

// NEED_PRIVATE should never be defined in user code, it is needed for the library code
// Fortran programs should use #include <time_trace.hf> that will include  time_trace.h in an appropriate manner
// C programs should use #include <time_trace.hf>
#endif

#if defined(NEED_PRIVATE)
#define MAJORV 1
#define MINORV 0
#define HEADER 4
#endif

#if defined SELF_TEST
#define MAX_TIMES  7
#else
#define MAX_TIMES 4096
#endif

#if defined(FORTRAN_SOURCE)
!****T* libtimetrace/user defined types  (Fortran version)
! SYNOPSIS
!  use #include <time_trace.hf>

type, BIND(C) :: time_context   !  wrappper to give a type to a C pointer
  type(C_PTR) :: t              ! actual pointer to internal trace table
end type
!****
#if defined(NEED_PRIVATE)

!type, bind(C) :: timeval        !  interface to structure returned by get_time_of_day
!  integer(C_LONG_LONG) :: sec   !  seconds for an arbitrary time in the past
!  integer(C_LONG_LONG) :: usec  !  microseconds (added to sec)
!end type
#endif

#else
//****T* libtimetrace/user defined types  (C version) 
/**/
// SYNOPSIS
// use #include <time_trace.h>

typedef struct{      //  struct wrapper to give a type to a C pointer to a trace table
  void *t;           // actual pointer to trace table
}time_context;
//****   */
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
!****f* libtimetrace/time_trace_dump_text  (Fortran version)
! Synopsis
!  Dump the accumulated time trace into a text file, using a free format
!  the file name will be filename_nnnnnn.txt (nnnnnn from ordinal)
!  filename amy be a relative path (some_name) or an absolute path (/some/path/to/some/name)
!
!  t        : opaque time context object (from time_trace_init)
!  filename : file name prefix (will be trimmed to remove trailing blanks if any)
!  ordinal  : numbered extension to file name (nnnnnn) (normally the MPI rank)
!
! ARGUMENTS
  subroutine time_trace_dump_text(t, filename, ordinal)   ! dump timings int file filename_nnnnnn.txt (nnnnnn from ordinal)
    import :: time_context
    implicit none
    type(time_context), intent(IN) :: t              ! opaque time context object (from time_trace_init)
    character(len=*), intent(IN) :: filename         ! file name prefix (will be trimmed to remove trailing blanks if any)
    integer, intent(IN) :: ordinal                   ! numbered extension to file name (nnnnnn) (normally MPI rank)
!****
  end subroutine time_trace_dump_text

!****f* libtimetrace/time_trace_dump_binary  (Fortran version)
! Synopsis
!  Dump the accumulated time trace into a C binary file (NATIVE ENDIANNESS)
!  the file name will be filename_nnnnnn.dat (nnnnnn from ordinal)
!  filename amy be a relative path (some_name) or an absolute path (/some/path/to/some/name)
!
!  t        : opaque time context object (from time_trace_init)
!  filename : file name prefix (will be trimmed to remove trailing blanks if any)
!  ordinal  : numbered extension to file name (nnnnnn) (normally the MPI rank)
!
! ARGUMENTS
  subroutine time_trace_dump_binary(t, filename, ordinal)   ! dump timings int file filename_nnnnnn.dat (nnnnnn from ordinal)
    import :: time_context
    implicit none
    type(time_context), intent(IN) :: t              ! opaque time context object
    character(len=*), intent(IN) :: filename         ! file name prefix (will be trimmed to remove trailing blanks if any)
    integer, intent(IN) :: ordinal                   ! numbered extension to file name (nnnnnn) (normally MPI rank)
!****
  end subroutine time_trace_dump_binary

!****f* libtimetrace/time_trace_barr  (Fortran version)
! Synopsis
!  Add a "barriered" pair of time traces. The first time will be taken before the "barrier" call, the second
!  time will be taken after the "barrier". This allows to measure the "load imbalance" across the barrier.
!  This function is normally intended to be used by an MPI application.
!  ( "barriered" version of function time_trace)
!
!  t        : opaque time context object (from time_trace_init)
!  tag      : tag number associated with this timing point (MUST be >0 and <99999999)
!  barrier  : if .true., the function issues a barrier call and get two times
!             if .false. the function behaves like function time_trace
!  comm     : integer value (normally a MPI communicator) passed to barrier_code
!  barrier_code : "barrier" function to be called between the two times
!             call barrier_code(comm, ierr)  (ierr is an integer variable)
!             MPI_barrier would be the usual value passed to time_trace_barr
!
! ARGUMENTS
  subroutine time_trace_barr(t, tag, barrier, comm, barrier_code)  ! insert a new time trace entry (2 entries if barrier is true)
    import :: time_context
    type(time_context), intent(IN) :: t              ! opaque time context object (from time_trace_init)
    integer, intent(IN) :: tag                       ! tag number for this timing point (MUST be >0 and <99999999)
    logical, intent(IN) :: barrier                   ! if true, call MPI_barrier with timing points before and after
    integer, intent(IN) :: comm                      ! MPI communicator (only used if barrier flag is true)
    external :: barrier_code
!****
  end subroutine time_trace_barr
#endif

#if defined(NEED_PRIVATE)
  subroutine new_time_tag(t, tag, t1, t2) bind(C,name='TimeTraceNewTag') ! insert tag and 1 or 2 time deltas
    import :: time_context, C_INT, C_LONG_LONG
    type(time_context), intent(IN), value :: t              ! opaque time context object
    integer(C_INT), intent(IN),value :: tag                 ! 32 bit integer
    integer(C_LONG_LONG), intent(IN), value :: t1, t2       ! 64 bit integers
  end subroutine new_time_tag

!  function gettime(tv,tz) result(time) BIND(C,name='gettimeofday')
!    import :: timeval, C_PTR, C_INT
!    implicit none
!    type(timeval), intent(OUT)     :: tv
!    type(C_PTR), value, intent(IN) :: tz
!    integer(C_INT) :: time
!  end function gettime

  function what_time_is_it() result (t) bind(C,name='WhatTimeIsIt')
    import :: C_LONG_LONG
    INTEGER(C_LONG_LONG) t
  end function what_time_is_it
#endif

!****f* libtimetrace/wall_clock_time  (Fortran version)
! Synopsis
!  Returns elapsed wall clock time since an arbitrary time in the past (time is in microseconds)
! ARGUMENTS
  function wall_clock_time() result (t) bind(C,name='WallClockTime')
    import :: C_LONG_LONG
    INTEGER(C_LONG_LONG) t     ! 64 bit integer
!****
  end function wall_clock_time

!****f* libtimetrace/cpu_clock_cycles  (Fortran version)
! Synopsis
!  Returns the number of CPU clock cycles since an arbitrary time in the past (usually last boot time)
!  The CPU is assumed to be running at its "nominal frequency" (insensitive to the actual clock speed that may vary)
! ARGUMENTS
  function cpu_clock_cycles() result (t) bind(C,name='CpuClockCycles')
    import :: C_LONG_LONG
    INTEGER(C_LONG_LONG) t     ! 64 bit integer
!****
  end function cpu_clock_cycles

!****f* libtimetrace/get_time_of_day  (Fortran version)
! Synopsis
!  Interface to the C library function gettimeofday (see user defined type timeval above)
!  Returns the elapsed time from an arbitrary time in the past
!
!  output argument tv is type(timeval) (see above)
!  argument tz should be C_NULL_PTR as no Fortran user defined type is available for the Time Zone
! ARGUMENTS
  function get_time_of_day(tv,tz) result(time) BIND(C,name='gettimeofday') ! interface to C library function gettimeofday
    import :: timeval, C_PTR, C_INT
    implicit none
    type(timeval), intent(OUT)     :: tv
    type(C_PTR), value, intent(IN) :: tz    ! for the time being this should be C_NULL_PTR
    integer(C_INT) :: time
!****
  end function get_time_of_day

!****f* libtimetrace/time_trace_init  (Fortran version)
! Synopsis
!  Create a "time context", used to accumulate a time trace
!  (multiple threads could have independent time traces if necessary)
!
!  t        : opaque time context object (to be used in subsequent calls to the package functions)
! ARGUMENTS
  subroutine time_trace_init(t) bind(C,name='TimeTraceInit')! create and initialize a new time trace context
    import :: time_context
    implicit none
    type(time_context), intent(OUT) :: t                    ! opaque time context object (passed to other routines)
!****
  end subroutine time_trace_init

!****f* libtimetrace/time_trace_step  (Fortran version)
! Synopsis
!  Start of a new "time step" (or timing epoch). Until the next "time step" times will be stored
!  relative to the start of "time step" ( up o ~ 2000 seconds before overflow)
!
!  t        : opaque time context object (from time_trace_init)
!  step     : time step number (arbitrary, user defined, MUST be >0 and <99999999)
!
! ARGUMENTS
  subroutine time_trace_step(t, step) bind(C,name='TimeTraceStep')  ! set step value for subsequent calls to time_trace
    import :: time_context, C_INT
    implicit none
    type(time_context), intent(IN), value :: t              ! opaque time context object (from time_trace_init)
    integer(C_INT), intent(IN), value :: step               ! time step number (MUST be >0 and <99999999)
!****
  end subroutine time_trace_step

  function time_trace_since_step(t) result(tm) bind(C,name='TimeTraceSinceStep')
    import :: time_context, C_LONG_LONG
    implicit none
    type(time_context), intent(IN), value :: t              ! opaque time context pointer (from time_trace_init)
    integer(C_LONG_LONG) :: tm                              ! time since last time_trace_step call (microseconds)
  end function time_trace_since_step

!****f* libtimetrace/time_trace  (Fortran version)
! Synopsis
!  Record a timing point with an associated tag
!
!  t        : opaque time context object (from time_trace_init)
!  tag      : arbitrary number identifying this timing point (MUST be >0 and <99999999)
! ARGUMENTS
  subroutine time_trace(t, tag)  bind(C,name='TimeTrace') ! insert a new time trace entry (no barrier)
    import :: time_context, C_INT
    implicit none
    type(time_context), intent(IN), value :: t              ! opaque time context object (from time_trace_init)
    integer(C_INT), intent(IN), value :: tag                ! tag number for this timing point (MUST be >0 and <99999999)
!****
  end subroutine time_trace

!****f* libtimetrace/time_trace_get_buffers  (Fortran version)
! Synopsis
!  Get timing buffers and their associated lengths. If there are less than n buffers available, the
!  unused entries in array will be filled with C_NULL_PTR and the unused entries in larray will be filled with 0
!
!  t        : opaque time context object (from time_trace_init)
!  array    : C pointer array that will receive the C addresses of the buffers
!  larray   : integer array that 
!  n        : dimension of array and larray
! ARGUMENTS
  subroutine time_trace_get_buffers(t, array, larray, n) bind(C,name='TimeTraceGetBuffers')
    import :: C_PTR , C_INT, time_context
    implicit none
    type(time_context), intent(IN), value :: t              ! opaque time context object (from time_trace_init)
    integer(C_INT), intent(IN), value :: n                  ! size of array and larray 
    type(C_PTR), dimension(n), intent(OUT) :: array         ! to receive pointers to buffers
    integer(C_INT), dimension(n), intent(OUT) :: larray     ! to receive lengths of buffers (32 bit integers)
!****
  end subroutine time_trace_get_buffers

  ! this function is normally used with consolidate non zero
  ! it will allocate the data buffer, return a C pointer to said buffer, nbuf and nent are set
  ! if consolidate is zero, a NULL pointer is returned, nbuf and nent are set
  function time_trace_get_buffer_data(t, nbuf, nent, consolidate) result(p) bind(C,name='TimeTraceGetBufferData')
    import :: C_PTR , C_INT, time_context
    implicit none
    type(time_context), intent(IN), value :: t              ! opaque time context pointer (from time_trace_init)
    integer(C_INT), intent(OUT) :: nbuf                     ! number of time trace buffers in this context
    integer(C_INT), intent(OUT) :: nent                     ! total number of entries (one entry == one 32 bit integer)
    integer(C_INT), intent(IN), value :: consolidate        ! if non zero, consolidate all buffers into one
    type(C_PTR) :: p
  end function time_trace_get_buffer_data

  ! print data buffer from time_trace_get_buffer_data to a file
  subroutine time_trace_single_text(data, nbent, filename, ordinal) bind(C,name='TimeTraceSingleText')
    import :: C_PTR , C_INT
    implicit none
    type(C_PTR), intent(IN), value :: data                  ! data buffer pointer from time_trace_get_buffer_data
    integer(C_INT), intent(IN), value :: nbent              ! value obtained from time_trace_get_buffer_data
    character(len=1), dimension(*), intent(IN) :: filename  ! file name will be filename_nnnnnn.txt (nnnnnn from ordinal)
    integer(C_INT), intent(IN), value :: ordinal
  end subroutine time_trace_single_text

  ! expand data buffer from time_trace_get_buffer_data into an integer array
  function time_trace_expand(data, nbent, ref, out, linesize, lines, ver) result(n) bind(C,name='TimeTraceExpand')
    import :: C_PTR , C_INT
    implicit none
    type(C_PTR), intent(IN), value :: data                  ! data buffer pointer from time_trace_get_buffer_data
    integer(C_INT), intent(IN), value :: nbent
    integer(C_INT), intent(IN), value :: linesize
    integer(C_INT), intent(IN), value :: lines
    integer(C_INT), dimension(linesize,lines), intent(INOUT) :: ref
    integer(C_INT), dimension(linesize,lines), intent(OUT) :: out
    integer(C_INT), intent(IN), value :: ver
    integer(C_INT) :: n
  end function time_trace_expand

  function i8_from_2_i4(a,b) result(i8) bind(C, name='I8From2I4')  ! pack 2 32 bit integers into a 64 bit integer
    import :: C_LONG_LONG , C_INT                                  ! useful because Fortran has no notion of UNSIGNED integers
    implicit none
    integer(C_INT), intent(IN), value :: a, b
    integer(C_LONG_LONG) :: i8
  end function i8_from_2_i4

end interface
#else
#if defined(NEED_PRIVATE)
  void *TimeTraceCreate();
  void TimeTraceCreateNewBead(time_context t);
  void TimeTraceInsert(time_context t, int val);
  void TimeTraceNewTag(time_context t, int tag, long long t1, long long t2);
  void TimeTraceNewStep(time_context t, int step);
#endif
//****f* libtimetrace/WallClockTime (C version)
// */
// Synopsis
//  Returns elapsed wall clock time since an arbitrary time in the past (time is in microseconds)
// ARGUMENTS
  uint64_t WallClockTime();
//**** */
//****f* libtimetrace/CpuClockCycles (C version)
//*/
// Synopsis
//  Returns the number of CPU clock cycles since an arbitrary time in the past (usually last boot time)
//  The CPU is assumed to be running at its "nominal frequency" (insensitive to the actual clock speed that may vary)
// ARGUMENTS
  uint64_t CpuClockCycles(void);
//**** */
//****f* libtimetrace/TimeTraceGetBuffers (C version)
// */
// Synopsis
//  See time_trace_get_buffers
// ARGUMENTS
  void TimeTraceGetBuffers(time_context t, void **array, int *larray, int n);
//**** */
//****f* libtimetrace/TimeTraceInit (C version)
// */
// Synopsis
//  See time_trace_init
// ARGUMENTS
  void TimeTraceInit(time_context *t);
//**** */
//****f* libtimetrace/TimeTraceStep (C version)
// */
// Synopsis
//  See time_trace_step
// ARGUMENTS
  void TimeTraceStep(time_context t, int step);
//**** */
  long long  TimeTraceSinceStep(time_context t);
//****f* libtimetrace/TimeTrace (C version)
// */
// Synopsis
//  See time_trace
// ARGUMENTS
  void TimeTrace(time_context t, int tag);
//**** */
//****f* libtimetrace/TimeTraceBarr (C version)
// */
// Synopsis
//   Add a "barriered" pair of time traces. The first time will be taken before the "barrier" call, the second
//   time will be taken after the "barrier". This allows to measure the "load imbalance" across the barrier.
//   This function is normally intended to be used by an MPI application.
//   ( "barriered" version of function TimeTrace)
//
//   DEFERRED FULL IMPLEMENTATION (as of now, barrier_code and comm are IGNORED)
//   two timing entries are created, to remain consistent with the Fortran version
// 
//   t        : opaque time context object (from time_trace_init)
//   tag      : tag number associated with this timing point (MUST be >0 and <99999999)
//   barrier  : if non zero, the function issues a barrier call and get two times
//              if equal to zero. the function behaves like function TimeTrace
//   comm     : integer value (normally a Fortran MPI communicator) passed to barrier_code
//   barrier_code : "barrier" function to be called between the two times
//              ierr = barrier_code(comm)  (ierr is an integer variable)
//              MPI_barrier would be the usual value passed to time_trace_barr
// ARGUMENTS
  void TimeTraceBarr(time_context t, int tag, int barrier, int fcomm, void (*barrier_code)());
//**** */
  void TimeTraceDump(time_context t, char *filename, int ordinal);
  void  *TimeTraceGetBufferData(time_context t, int *nbuf, int *nent, int consolidate);
//****f* libtimetrace/TimeTraceDumpText (C version)
// */
// Synopsis
//  See time_trace_dump_text
// ARGUMENTS
  void TimeTraceDumpText(time_context t, char *filename, int ordinal);
//**** */
//****f* libtimetrace/TimeTraceDumpBinary (C version)
// */
// Synopsis
//  See time_trace_dump_binary
// ARGUMENTS
  void TimeTraceDumpBinary(time_context t, char *filename, int ordinal);
//**** */
  void TimeTraceSingleText(unsigned int *data, int nbent, char *filename, int ordinal);
  int TimeTraceExpand(unsigned int *data, int nbent, int *ref, int *out, int linesize, int lines, int ver);
  unsigned long long I8From2I4(unsigned int a, unsigned int b);
#endif

#if defined(FORTRAN_SOURCE)
#else
#endif

