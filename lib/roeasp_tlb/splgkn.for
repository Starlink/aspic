      SUBROUTINE SPLGKN(NLEVS,LEVELS,KNOTS,NKNOTS)                            
C+
C   SPLGKN
C
C     generates knots for SPLPAR in STEPWEDGE
C
C     given:
C       NLEVS  -  I*4       - number of calibration levels
C	LEVELS -  R*4 array - observed values and original intensities
C
C     calculates:
C       KNOTS    R*4 array - knots (exterior knots undefined)
C       NKNOTS   I*4       - number of knots (includes exterior)
C
C   J.A.Cooke/UOE/27.8.1981
C   D. Tudhope/ROE/Nov 1982
C-
 
      INTEGER NLEVS
      REAL LEVELS(NLEVS,2)
 
      REAL*8 KNOTS(20)
      INTEGER NKNOTS
 
      IF (NLEVS.EQ.4) THEN
         NKNOTS=8
 
      ELSE
         NKNOTS=9
         KNOTS(5)=(LEVELS(1,1)+LEVELS(NLEVS,1))/2
 
      ENDIF
 
      END
C
C
C*********************************************************************
