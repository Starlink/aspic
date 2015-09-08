      SUBROUTINE IZOOM(IX,IY,IFX,IFY)
      INTEGER IX,IY,IFX,IFY
 
 
 
      CALL ARGS_FLUSH(4)
      CALL ARGS_PUT1('C000'X+IX)
      CALL ARGS_PUT1('A000'X+IY)
      CALL ARGS_PUT1('5001'X)
      CALL ARGS_PUT1(256*(IFY-1)+IFX-1)
      CALL SRSEND
 
      END
 
 
 
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R STORE  *
C      *            *
C      **************
C
C
C   PURPOSE
C     Copy input file over and new psoitions, ignoring removed points
C
C   ARGUMENTS
C  IN
C    AIN    Real(NX,NY)     input file
C    NX     Int             No of items in an IN record
C    NY     Int             No of records in In
C    ANEW   Real(3,NY)      New positions and remove flags
C  OUT
C    OUT    Real(NX,KLEN)   Output list
C
C   STARLINK PARAMETERS
C     None
C
C
C   CALLS
C     None
C
C   USES
C
C
C   A.J.PENNY                   RGO                    83-6-21
C
C -----------------------------------------------------------------
C
C
C
