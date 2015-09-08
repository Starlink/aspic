C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R DEVOPA *
C      *            *
C      **************
C
C      Purpose
C        This s/r opens devices for writing graphs using the
C        SIMPLEPLOT package.
C        It takes the Device number and graph size as inputs.
C        The permitted devices are:-
C        NONE,ARGS,TEKTRONIX,GOC,VERSATEC,CALCOMP or CC81. (this last
C        is a small flatbed Calcomp.)
C        These are coded 1,2,3,4,5,6,7,8.
C
C
C   ARGUMENTS
C        IN
C         IDEV  Integer
C              The number assigned to the device for Simpleplot.
C         SIZE  Real (2)
C               The user chosen physical size for use.
C
C
C   CALLS
C        Simpleplot
C           JBDEV
C        Starlink
C           DO_DCL
C
C
C      A J Penny                 RGO                   82-11-15
C ----------------------------------------------------------
C
C
C
      SUBROUTINE DEVOPA(IDEV,SIZE)
C
C
C
      REAL SIZE(2)
      CHARACTER TEXT*72,DEVICE*4
C
C  Get device
C
      IF (IDEV.EQ.2) THEN
         DEVICE = 'ARGS'
      ENDIF
      IF (IDEV.EQ.3) DEVICE = 'TEK '
      IF (IDEV.EQ.4) THEN
         DEVICE = 'GOC '
      ENDIF
      IF (IDEV.EQ.5) THEN
         DEVICE = 'VERS'
      ENDIF
      IF (IDEV.EQ.6) THEN
         DEVICE = 'CALC'
      ENDIF
      IF (IDEV.EQ.8) THEN
         DEVICE = 'CC81'
         CALL DO_DCL('ASSIGN FOR000 CAL81',ISTAT)
      ENDIF
C
C  Open device
C
      IF (IDEV.NE.1) THEN
         CALL JBDEV(DEVICE)
      ENDIF
C
C
C
      END



