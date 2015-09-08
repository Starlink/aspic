C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R PUTIMG *
C      *            *
C      **************
C
C
C   PURPOSE
C      To open an OUTPUT image for GRASP programs
C
C   ARGUMENTS
C  IN
C     IMAGE    Char*(*)       Starlink paramter name of image
C     NSIZE    Int(2)         Size of image
C     BS       Real           BSCALE factor
C     BZ       Real           BZERO factor  (Actual=BS*value + BZ)
C     INVAL    Int            Pixel value flagging an invalid pixel
C     TITLE    Char*(*)       Starlink parameter name of image title
C     COPY     Char*(*)       Starlink parameter name of image supplying
C                             extra descriptors to write to image (optional)
C     KCOPY    Int            Flag to use COPY (=1 yes,=0 no)
C  OUT
C     IPOUT    Int            VAX/VMS %val pointer to image
C     ISTAT    Int            Error flag (=0 for ok)
C
C   CALLS
C     Starlink
C         GT2DIW,RDKEYC,CYDSCR,PTDSCR
C
C   USES
C     I*2 arrays
C     %VAL facility
C     Starlink ERRPAR,FMTPAR
C
C
C   A.J.PENNY                   RGO                    84-2-6 12:41
C
C -----------------------------------------------------------------
C
C
      SUBROUTINE PUTIMG(IMAGE,NSIZE,BS,BZ,INVAL,TITLE,COPY,KCOPY,
     +                  IPOUT,ISTAT)
C
C
C
      INCLUDE 'STARDIR:ERRPAR.FOR/NOLIST'
      INCLUDE 'STARDIR:FMTPAR.FOR/NOLIST'
C
C
C
      CHARACTER*(*) IMAGE,COPY,TITLE
      CHARACTER CVAL*1,CTITLE*72
      INTEGER NSIZE(2)
C
C
C
      CALL GT2DIW(IMAGE,FMT_SW,.FALSE.,NSIZE(1),NSIZE(2),IPOUT,ISTAT)
      IF (ISTAT.EQ.0) THEN
C
C
         IF (KCOPY.NE.0) CALL CYDSCR(COPY,IMAGE,IERR)
         CALL RDKEYC(TITLE,.FALSE.,1,CTITLE,NVAL,IERR)
         CALL PTDSCR(IMAGE,TITLE,'CHARACTER',IVAL,RVAL,CTITLE,IERR)
         CALL PTDSCR(IMAGE,'BSCALE','REAL',IVAL,BS,CVAL,IERR)
         CALL PTDSCR(IMAGE,'BZERO','REAL',IVAL,BZ,CVAL,IERR)
         CALL PTDSCR(IMAGE,'INVALID','REAL',INVAL,RV,CVAL,IERR)
      ENDIF
C
C
C
      END



