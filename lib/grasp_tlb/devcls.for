C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R DEVCLS *
C      *            *
C      **************
C
C
C      PURPOSE
C        This s/r closes a device which has been used by SIMPLOT routines
C        to plot graphs. It is the companion to s/r DEVOPN (qv).
C
C
C   INPUT/OUTPUT
C    IN
C       IDEV   Integer
C             The number of the device to close
C    OUT
C       None
C
C   CALLS
C     Simpleplot
C        END PLT
C     Starlink
C        DO_DCL
C
C
C      A J Penny                  RGO                    82-11-15
C --------------------------------------------------------------
C
C
C
      SUBROUTINE DEVCLS(IDEV)
C
C
C
      IF (IDEV.NE.1) THEN
         CALL END PLT
         IF (IDEV.EQ.5) THEN
            CALL DO_DCL('$PRINT/DELETE/QUEUE=GKS_RASTER/PARAMETER=VERSATEC
     +                   FOR000.DAT',ISTAT)
            IF (MOD(ISTAT,2).EQ.0) CALL GETMSG(ISTAT)
         ENDIF
         IF (IDEV.EQ.6) THEN
            CALL DO_DCL('@LSTARDISK:[STARLOCAL.UTILITY.CALCOMP]CALPLOT
     +                   FOR000',ISTAT)
            IF (MOD(ISTAT,2).EQ.0) CALL GETMSG(ISTAT)
         ENDIF
      ENDIF
C
C
C
      END


