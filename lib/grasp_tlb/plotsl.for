C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R PLOTSL *
C      *            *
C      **************
C
C      Purpose
C       This s/r plots out as a projected view of the data in a 2-d array
C       as though the data values represented the height data of a solid
C       body. The user is asked for the Min and Max values to plot.
C       It uses the SIMPLEPLOT routines.
C
C   INPUT/OUTPUT
C     IN
C       DATA   Real (2)
C          The data to be plotted
C       LX,LY  Integer
C          The dimensions of DATA
C       SIZE   Real (2)
C          The physical size of the plotted area
C    OUT
C       IERR   Integer
C          Error flag (=0 for success)
C
C    STARLINK PARAMETERS
C       DEVLIM    The Min and Max values to plot
C
C
C   CALLS
C
C      Simpleplot
C        LIM3D,SOLID
C      Starlink
C        CNPAR,RDKEYR,WRUSER
C
C
C        A J Penny                 RGO                   82-11-15
C --------------------------------------------------------------
C
C
C
      SUBROUTINE PLOTSL(DATA,LX,LY,SIZE,IERR)
C
C
C
      REAL DATA(LX,LY),SIZE(2),DEVLIM(2)
      CHARACTER*72 TEXT
C
C
C
      IERR = 0
C
C
      AMAX = DATA(1,1)
      AMIN = AMAX
      DO K = 1,LY
         DO J = 1,LX
            IF (DATA(J,K).LT.AMIN) AMIN = DATA(J,K)
            IF (DATA(J,K).GT.AMAX) AMAX = DATA(J,K)
         ENDDO
      ENDDO
      WRITE(TEXT,900)AMIN,AMAX
  900 FORMAT (' ','MIN VALUE = ',F12.6,'  MAX VALUE = ',F12.6)
      CALL WRUSER(TEXT,ISTAT)
      CALL WRUSER('INPUT MIN AND MAX VALUES TO PLOT',ISTAT)
      DEVLIM(1) = AMIN
      DEVLIM(2) = AMAX
      CALL RDKEYR('DEVLIM',.TRUE.,2,DEVLIM,NVAL,ISTAT)
      BOT = DEVLIM(1)
      TOP = DEVLIM(2)
      CALL LIM3D(BOT,TOP)
      ASIZE = 0.9*SIZE(1)
C
C
C
      CALL SOLID(DATA,LX,LY,ASIZE,15.0)
C
C
C
      CALL CNPAR('DEVLIM',ISTAT)
C
C
C
      END



