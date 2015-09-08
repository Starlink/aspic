      PROGRAM CALL_WRDSCR
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     ********************
C                     *                  *
C                     * Program   WRDSCR *
C                     *                  *
C                     ********************
C
C
C
C          CALLING SEQUENCE:-
C               WRDSCR    [LOOP=false]
C
C
C          FUNCTION:-
C               It allows  descriptor  items  (specified  by  NAME)  to  be
C               overwritten  with  new  information  (VALUE) or inserted if
C               they were not present.
C
C
C          USE:-
C               It may used to store useful information with image data.
C
C
C
C         USER PARAMETERS:-
C
C         FRAME                               The name  of  the  (existing)
C                                             frame     to     which    the
C                                             descriptors are to be added.
C
C         NAME                                The name of  the  descriptor.
C                                             (Up  to  20  characters, null
C                                             response to exit.)
C
C         VALUE                               A character string containing
C                                             the     "value"     of    the
C                                             descriptor. May be up  to  72
C                                             characters.
C
C         NORMALLY DEFAULTED PARAMETERS
C
C            LOOP      TRUE                   If TRUE it allows many items
C                                             to be inserted; if FALSE then
C                                             only one is allowed.
C
C
C         K F Hartley              RGO                            13-JUL-82
C
C
C--------------------------------------------------------------------------



      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
      INTEGER AX(3)
      CHARACTER DESNAM*20,VAL*72
      LOGICAL LOOP
C
C    First get the frame
C
      CALL RDIMAG('FRAME',FMT_R,3,AX,I,IP,ISTAT)
      IF (ISTAT.NE.ERR_NORMAL) THEN
         CALL WRERR('IMERR',ISTAT)
         GO TO 100
      END IF
C
C   then decide if looping is required
C
      LOOP=.TRUE.
      CALL RDKEYL('LOOP',.TRUE.,1,LOOP,I,ISTAT)
C
C   Then loop reading descriptors.
C
   10 CONTINUE
         CALL RDKEYC('NAME',.FALSE.,1,DESNAM,I,ISTAT)
         IF (ISTAT.EQ.ERR_NORMAL) THEN
            CALL RDKEYC('VALUE',.FALSE.,1,VAL,I,ISTAT)
            CALL WRDSCR('FRAME',DESNAM,VAL,1,ISTAT)
            CALL CNPAR('VALUE',ISTAT)
            CALL CNPAR('NAME',ISTAT)
            IF (LOOP) GO TO 10
         END IF
  100 CONTINUE
      CALL FRDATA(' ',ISTAT)
      CALL EXIT
      END
