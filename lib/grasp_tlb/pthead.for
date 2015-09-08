C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R PTHEAD *
C      *            *
C      **************
C
C
C
C   PURPOSE
C     This s/r puts a descriptor onto a file. It is put as the
C     contents of 'HEADn' where 'n' is the desired number 001 or 002
C     thru 099 to 999.
C     They should be 20 characters long and must be 72 or less.
C
C   ARGUMENTS
C   IN
C      FILE       Character*(*) The file to write to
C      NUM        Integer       The number of descriptor to write
C      TEXT       Character*(*) The descriptor contents
C   OUT
C      IERR       Integer       Error flag. Success = 0
C
C   CALLS
C     Edrs
C       PTDSCR
C
C   A.J.PENNY                   RGO                    84-2-6 15:15
C
C -----------------------------------------------------------------
C
C
      SUBROUTINE PTHEAD(FILE,NUM,TEXT,IERR)
C
C
C
      CHARACTER FILE*(*)
      CHARACTER*(*) TEXT
      CHARACTER*72 TEXTA
      CHARACTER TEXTH*7
C
C  Set error flag
C
      IERR = 0
C
C  Put header
C
      WRITE(TEXTH,900)NUM
  900 FORMAT('HEAD',I3.3)
      TEXTA = TEXT
      CALL PTDSCR(FILE,TEXTH,'CHARACTER',IVAL,RVAL,TEXTA,ISTAT)
      IF (ISTAT.NE.0) IERR = 1
C
C
C
      END



