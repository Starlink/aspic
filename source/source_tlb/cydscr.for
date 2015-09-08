      PROGRAM CALL_CYDSCR
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     ********************
C                     *                  *
C                     * Program   CYDSCR *
C                     *                  *
C                     ********************
C
C
C
C          CALLING SEQUENCE:-
C               CYDSCR
C
C
C          FUNCTION:-
C               It copies the whole of the descriptor  part  of  one  frame
C               into another frame.
C
C
C          USE:-
C               It may be used to  transfer  specialized  information  from
C               frame  to frame without changing the data values. Note that
C               it is only permitted if the total number of pixels  in  the
C               two images are the same.
C
C
C
C         USER PARAMETERS:-
C
C         FROM                                The  name  of  the   Starlink
C                                             frame  whose descriptor is to
C                                             be copied.
C
C         TO                                  The  name  of  the   Starlink
C                                             frame  which  is  to have its
C                                             descriptor   overwritten   by
C                                             that from "FROM".
C
C
C         K F Hartley              RGO                            13-JUL-82
C
C
C--------------------------------------------------------------------------



      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
      INTEGER IN(3),OUT(3)
      INTEGER INPIX,OUTPIX
C
C   Gain access to the two frames.
C   (Note - READ access to data gives WRITE
C   access to the descriptor.
C
      CALL RDIMAG('FROM',104,3,IN,I,IFR,ISTAT)
      IF (ISTAT.NE.ERR_NORMAL) GO TO 100
      CALL RDIMAG('TO',104,3,OUT,I,ITO,ISTAT)
      IF (ISTAT.NE.ERR_NORMAL) GO TO 100
C
C   Now check forsame dimensions
C
      INPIX=1
      OUTPIX=1
      DO I=1,3
         INPIX=INPIX*IN(I)
         OUTPIX=OUTPIX*OUT(I)
      END DO
      IF(INPIX.NE.OUTPIX) THEN
         CALL WRERR('DIMERR',ISTAT)
         GO TO 200
      END IF
C
C   Now do the work
C
      CALL CYDSCR('FROM','TO',ISTAT)
      GO TO 200
  100 CONTINUE
      CALL WRERR('IMERR',ISTAT)
  200 CONTINUE
      CALL FRDATA(' ',ISTAT)
      CALL EXIT
      END
