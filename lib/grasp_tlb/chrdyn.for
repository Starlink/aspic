C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R CHRDYN *
C      *            *
C      **************
C
C
C   PURPOSE
C     This is used after GETDYN (qv) when dynamically allocating
C     storage for characters.
C     The GETDYN space should be 2 FMT_SL larger than required
C
C   USE
C
C      Allocate the space as bigger than wanted
C
C          N = NWANT + 8
C          CALL GETDYN('TEST',FMT_SB,N,IP,IERR)
C          CALL CHRDYN(%VAL(IP))
C
C      IP can now be used as %VAL(IP) in a s/r call
C
C
C   ARGUMENTS
C   IN/OUT
C      DESCR   Integer*3      The pointer which is changed to an address
C
C   USES
C    %LOC   VAX Fortran
C
C
C   A.J.PENNY                   RGO                    83-2-16
C
C -----------------------------------------------------------------
C
C
C
      SUBROUTINE CHRDYN(DESCR)
C
C
C
      INTEGER DESCR(3)
C
C
C
      DESCR(1) = 1
      DESCR(2) = %LOC(DESCR)+8
C
C
C
      END






