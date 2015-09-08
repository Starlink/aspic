C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R GTIMGF *
C      *            *
C      **************
C
C      This makes an I*2 image available, together with its
C      descriptors NSIZE,BSCALE,BZERO,INVAL and TITLE.
C      The parameter name given to the image is left uncanceled.
C
C      INPUT
C        NAME   Character*(*)  The Starlink parameter name of the image
C        IVFLA  INTEGER        Flag for typing comment if no inval (0=no)
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
C       to BS=1 BZ=0 INVAL=-32767 and a message may be typed out
C
C       STARLINK parameter
C         'NAME'
C
C
C      AJPENNY           RGO                     83-10-13
C --------------------------------------------------------------
C
C
C
      SUBROUTINE GTIMGF(NAME,IVFLA,IPIN,NSIZE,BS,BZ,INVAL,TITLE,IERR)
C
C
C
      INCLUDE 'STARDIR:ERRPAR.FOR/NOLIST'
      INCLUDE 'STARDIR:FMTPAR.FOR/NOLIST'
C
C
C
      CHARACTER*(*) NAME
      INTEGER NSIZE(2)
      CHARACTER*72 TXT
      CHARACTER TEXT*72,TITLE*72
C
C   Note the use of Integer*2 for compatibility with EDRS
C
      CALL RDIMAG(NAME,FMT_SW,2,NSIZE,NDIM,IPIN,ISTATI)
      IF (ISTATI.EQ.ERR_NORMAL.AND.NDIM.EQ.2) THEN
         IERR = 0
C
C   Get the relevant image descriptor items
C
         CALL RDDSCR(NAME,'INVAL',1,TXT,I,ISTAT)
         IF (ISTAT.EQ.ERR_NORMAL) THEN
            CALL CTOI(TXT,INVAL,ISTAT)
         ELSE
            INVAL=-32767
            IF (IVFLA.NE.0) THEN
               WRITE(TEXT,900)
  900          FORMAT(' NO INVALID PIXEL WARNING LEVEL, ASSUMED=-32767')
               CALL WRUSER(TEXT,ISTAT)
            ENDIF
         ENDIF
         CALL RDDSCR(NAME,'BZERO',1,TXT,I,ISTAT)
         IF (ISTAT.EQ.ERR_NORMAL) THEN
            CALL CTOR(TXT,BZ,ISTAT)
         ELSE
            BZ = 0.0
         ENDIF
         CALL RDDSCR(NAME,'BSCALE',1,TXT,I,ISTAT)
         IF (ISTAT.EQ.ERR_NORMAL) THEN
            CALL CTOR(TXT,BS,ISTAT)
         ELSE
            BS = 1.0
         ENDIF
         CALL RDDSCR(NAME,'TITLE',1,TITLE,I,ISTAT)
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
C
C
      END



