C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R GTIMG  *
C      *            *
C      **************
C
C      This makes an I*2 image available, together with its
C      descriptors NSIZE,BSCALE,BZERO,INVAL and TITLE.
C
C      INPUT
C        None
C      OUTPUT
C        IPIN     The VAX %val pointer to the image
C        NSIZE(2) The X,Y size of the image
C        BS       The BSCALE of the values
C        BZ       The BZERO of the values
C        INVAL    The pixel value taken as a flag for an invalid pixel
C        TITLE    The Title of the image
C        IERR     =0 for success
C
C       The real value of a pixel = BSCALE*VALUE + BZERO
C       If the image does not have these descriptors, they are set
C       to BS=1 BZ=0 INVAL=-32767
C
C       STARLINK parameter
C         IMAGE
C
C
C      AJPENNY           RGO                     17-DEC-82
C --------------------------------------------------------------
C
C
C
      SUBROUTINE GTIMG(IPIN,NSIZE,BS,BZ,INVAL,TITLE,IERR)
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
      CHARACTER TEXT*72,TITLE*72
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
            CALL CTOI(TXT,INVAL,ISTAT)
         ELSE
            INVAL=-32767
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
         CALL RDDSCR('IMAGE','TITLE',1,TITLE,I,ISTAT)
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



