C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R GTIMAG *
C      *            *
C      **************
C
C
C   PURPOSE
C    This opens a .BDF image of type Integer*2 with BSCALE,BZERO,INVAL
C    descriptors and returns with a pointer to the image for use in
C    %VAL calls and with the values of the descriptors.
C    If the descriptors are not there it puts them at 1.0,0.0,-32767.
C    A pixel true value is then = BSCALE*VALUE + BZERO, with a pixel
C    of content INVAL is flagged as invalid.
C
C    The file is opened in response to a Starlink 'IMAGE' parameter
C    and this parameter is then cancelled.
C
C
C
C   ARGUMENTS
C  OUT
C     IPIN      Integer      Pointer to image
C     NSIZE(2)  Integer      X and Y size of image
C     BS        Real         BSCALE descriptor
C     BZ        Real         BZERO descriptor
C     INVAL     Integer      Invalid pixel flag
C     IERR      Integer      Success flag (OK=0;no image=1; zero response = 4)
C
C   STARLINK PARAMETERS
C
C     IMAGE
C
C
C
C
C   CALLS
C     Aspic
C        CNPAR,CTOI,CTOR,RDDSCR,RDIMAG,WRUSER
C
C   USES
C     Starlink ERRPAR,FMTPAR
C
C
C   A.J.PENNY                   RGO                    83-2-28
C
C -----------------------------------------------------------------
C
C
C
      SUBROUTINE GTIMAG(IPIN,NSIZE,BS,BZ,INVAL,IERR)
C
C
C
      INCLUDE 'STARDIR:ERRPAR.FOR/NOLIST'
      INCLUDE 'STARDIR:FMTPAR.FOR/NOLIST'
C
C
C
      INTEGER NSIZE(2)
      CHARACTER*72 TXT
      CHARACTER TEXT*72
C
C   Note the use of Integer*2 for compatibility with EDRS
C
      CALL RDIMAG('IMAGE',FMT_SW,2,NSIZE,NDIM,IPIN,ISTATI)
      IF (ISTATI.EQ.ERR_NORMAL.AND.NDIM.EQ.2) THEN
         IERR = 0
C
C   Get the relevant image descriptor items
C
         CALL RDDSCR('IMAGE','INVAL',1,TXT,I,ISTAT)
         IF (ISTAT.EQ.ERR_NORMAL) THEN
            TEXT='INVALID PIXEL FLAG IS '//TXT
            CALL WRUSER(TEXT,ISTAT)
            CALL CTOI(TXT,INVAL,ISTAT)
         ELSE
            INVAL=-32767
            CALL WRUSER('NO VALID VALUE FOR INVAL, SET TO -32767',ISTAT)
         END IF
         CALL RDDSCR('IMAGE','BZERO',1,TXT,I,ISTAT)
         IF (ISTAT.EQ.ERR_NORMAL) THEN
            CALL CTOR(TXT,BZ,ISTAT)
         ELSE
            BZ = 0.0
         END IF
         CALL RDDSCR('IMAGE','BSCALE',1,TXT,I,ISTAT)
         IF (ISTAT.EQ.ERR_NORMAL) THEN
            CALL CTOR(TXT,BS,ISTAT)
         ELSE
            BS = 1.0
         END IF
      ELSE
C
C  If cant pick up frame, set error flag
C
         IF (ISTATI.EQ.ERR_PARNUL) THEN
            IERR = 4
         ELSE
            IERR = 1
            CALL WRUSER('NO VALID IMAGE',ISTAT)
         ENDIF
      ENDIF
C
C  Free IMAGE connection
C
      CALL CNPAR('IMAGE',ISTAT)
C
C
C
      END



