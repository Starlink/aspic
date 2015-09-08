CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R DEVOPN *
C      *            *
C      **************
C
C      Purpose
C        This s/r opens devices for writing graphs using the
C        SIMPLEPLOT package.
C        The user is asked for the Device name which must be
C        NONE,ARGS,TEKTRONIX,GOC,VERSATEC,CALCOMP or CC81. (this last
C        is a small flatbed Calcomp.)
C        The user is also asked for the physical size of the area on the
C        device to use.
C
C
C   INPUT/OUTPUT
C        IN
C         None
C
C        OUT
C         IDEV  Integer
C              The number assigned to the device for Simpleplot.
C         SIZE  Real (2)
C               The user chosen physical size for use.
C
C   STARLINK PARAMETERS
C        DEVICE  The device wanted
C        DEVSIZE The physical size of the area to be used
C
C   CALLS
C        Edrs
C           GETCMD
C        Simpleplot
C           JBDEV,PAGE
C        Starlink
C           CNPAR,DO_DCL,RDKEYR
C
C
C      A J Penny                 RGO                   82-11-15
C  mod D J King                  RGO                   85-09-12
C ----------------------------------------------------------
C
C
C
      SUBROUTINE DEVOPN(IDEV,SIZE)
C
C
C
      REAL SIZE(2)
      CHARACTER TEXT*72,DEVICE*4
C
C  Get device
C
      IDEV = 2
      CALL GETCMD('DEVICE','NONE,ARGS,TEKTRONIX,GOC,VERSATEC,CALCOMP,
     +            Z,CC81.',1,IDEV,TEXT,LDEV,ISTAT)
C
      SIZE(1) = 10.0
      SIZE(2) = 10.0
      IF (IDEV.EQ.2) THEN
         DEVICE = '   1'
         SIZE(1) = 22.0
         SIZE(2) = 22.0
      ENDIF
      IF (IDEV.EQ.3) DEVICE = '   2'
      IF (IDEV.EQ.4) THEN
         DEVICE = '   3'
         SIZE(1) = 20.0
         SIZE(2) = 20.0
      ENDIF
      IF (IDEV.EQ.5) THEN
         DEVICE = '   5'
         SIZE(1) = 20.0
         SIZE(2) = 20.0
      ENDIF
      IF (IDEV.EQ.6) THEN
         DEVICE = '  10'
      ENDIF
      IF (IDEV.EQ.8) THEN
         DEVICE = '   9'
         CALL DO_DCL('ASSIGN FOR000 CAL81',ISTAT)
      ENDIF
C
C  Open device
C
      IF (IDEV.NE.1) THEN
         CALL JBDEV(DEVICE)
         CALL RDKEYR('DEVSIZE',.TRUE.,2,SIZE,NVAL,ISTAT)
         CALL CNPAR('DEVSIZE',ISTAT)
      ENDIF
      CALL CNPAR('DEVICE',ISTAT)
C
C
C
      END



