      subroutine rdkeyc(name,dfault,maxval,carray,actval,status)
 
*++
*     RDKEYC - Read Key Character
*
*     This routine is used to read the values of program parameters
*     as character strings.
*
*     If the input argument, DFAULT, is set to TRUE then the array,
*     CARRAY, is assumed to contain default values to be communicated
*     to the user; i.e. it serves as an input and output argument.
*
*
*     CALL RDKEYC(NAME,DFAULT,MAXVAL,CARRAY,ACTVAL,STATUS)
*
*     Input arguments:
*     ---------------
*     NAME:    CHARACTER expression:    Parameter name
*     DFAULT:  LOGICAL expression:      If TRUE, CARRAY is assumed to
*                                       hold default values.
*     MAXVAL:  INTEGER expression:      Size of CARRAY.
*
*     Output arguments:
*     ----------------
*     CARRAY:  CHARACTER array:         Array to hold values (This is
*                                       also an input argument if DFAULT
*                                       is set to .TRUE.)
*     ACTVAL:  INTEGER variable:        Actual number of values found
*     STATUS:  INTEGER variable:        Status return value.
*
*
*     D.PEARCE  23/JUN/80  VERSION #2
*--
*
      implicit      integer(a-z)
 
*
      integer       maxval,actval,status
      character*(*) name,carray(*)
      logical       dfault
 
*
      include 'interim(errpar)'
 
*
*
*     .....Search for entry in PCT
      call stl_findpe(name,entry)
 
*
*     .....if entry does not exist then create one
 
      if (entry.eq.0) call stl_crepe(name,entry)
 
*
*     .....Get Program Parameter Information from environment
      call stl_getppi(entry,dfault,maxval,carray,actval,status)
 
*
*     .....check error status
 
      if (status.ne.err_normal) call stlerr('RDKEYx',status)
 
*
*
      return
 
      end
 
 
 
