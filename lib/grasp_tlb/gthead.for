C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R GTHEAD *
C      *            *
C      **************
C
C
C
C   PURPOSE
C     This s/r extracts a descriptorfrom a file. It
C     assumes the file is an XYlist and finds how many parameters
C     it has and then reads the decsriptor 'HEAD00n' , where 'n'
C     is the number of the wanted Header.
C     The descriptor can be up to 20 characters long.
C     If the descriptor is missing it is set to ' ' and an error flagged.
C
C   ARGUMENTS
C   IN
C      FILE     Character*    The file to read from
C      NUM      Integer       The number of the descriptor to be read
C   OUT
C      TEXT     Character*20  The descriptor
C      IERR     Integer       Error flag. Success = 0
C
C   CALLS
C     Edrs
C       GTDSCR
C
C
C   A.J.PENNY                   RGO                    84-2-6  15:08
C
C -----------------------------------------------------------------
C
C
C
      SUBROUTINE GTHEAD(FILE,NUM,TEXT,IERR)
C
C
C
      CHARACTER*20 TEXT,TVAL
      CHARACTER FILE*(*)
      CHARACTER TEXTH*7,CVAL
C
C  Set error flag
C
      IERR = 0
C
C  Get header
C
      WRITE(TEXTH,900)NUM
  900 FORMAT('HEAD',I3.3)
      CALL GTDSCR(FILE,TEXTH,'CHARACTER',IVAL,RVAL,TVAL,ISTAT)
      IF (ISTAT.NE.0) THEN
         IERR = 1
         TVAL = ' '
      ENDIF
      TEXT = TVAL
C
C
C
      END



