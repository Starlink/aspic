C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     ********************
C                     *                  *
C                     * Program   CCDCON *
C                     *                  *
C                     ********************
C
C
C
C          CALLING SEQUENCE:-
C               CCDCON
C
C
C          FUNCTION:-
C               It changes the dimensions of an image by resetting the
C               values of the NAXIS1,NAXIS2  descriptors.
C
C
C          USE:-
C               It is used to convert Chile CCD data tape files which
C               have been read by PDSIN
C
C
C
C         USER PARAMETERS:-
C
C         FRAME                               The name  of  the  (existing)
C                                             frame     to     which    the
C                                             descriptors are to be added.
C
C
C         CDP                      RGO                          18-MAR-83
C
C
C--------------------------------------------------------------------------



      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
      INTEGER AX(2)
      CHARACTER DESNAM*20,VAL*72
C
C    First get the frame
C
      CALL RDIMAG('FRAME',FMT_SW,2,AX,I,IP,ISTAT)
      IF (ISTAT.NE.ERR_NORMAL) THEN
         CALL WRERR('IMERR',ISTAT)
         GO TO 100
      END IF
*
*  check dimensions
*
      IF(AX(1).NE.2560.OR.AX(2).NE.64)  THEN
         CALL WRUSER(' Dimensions of input image not as expected',IST)
         CALL WRUSER(' must be 2560 x 64 ',IST)
         GO TO 100
      ENDIF

C
C     CHANGE DESCRIPTORS
C
      DESNAM = 'NAXIS1'
      VAL = '320'
      CALL WRDSCR('FRAME',DESNAM,VAL,1,ISTAT)
      DESNAM = 'NAXIS2'
      VAL = '512'
      CALL WRDSCR('FRAME',DESNAM,VAL,1,ISTAT)

  100 CONTINUE
      CALL FRDATA(' ',ISTAT)
      CALL EXIT
      END
