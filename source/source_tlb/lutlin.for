      PROGRAM LUTLIN
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     ********************
C                     *                  *
C                     * Program   LUTLIN *
C                     *                  *
C                     ********************
C
C
C
C          CALLING SEQUENCE:-
C               LUTLIN [LUT=name] [QUIET=false]
C
C
C          FUNCTION:-
C               It uses the ARGS cursor/trackerball  to  allow  interactive
C               modification  of  the look-up-table in the ARGS. Normally a
C               greyscale LUT is used, bur any other may be read in. Values
C               below  a  lower limit are set to the limit, and those above
C               an upper limit are set to that limit. The cursor is used to
C               move  these limits, on the assumption that the scale from 0
C               to 255 is spread out across the full width of the displayed
C               image.  An  option  is  available  to replace the (default)
C               linear LUT with a logarithmic one, between the same limits.
C               Note  that  only  the x position of the cursor is used, but
C               the cursor must be within the image currently displayed.
C
C
C          USE:-
C               It is designed to  allow  the  user  to  create  the  ideal
C               display   of   an   image,   and  to  store  the  resulting
C               look-up-table for later use.
C
C
C
C         USER PARAMETERS:-
C
C         PVLO            0                   This   is   only   used    if
C                                             QUIET=FALSE,  and is the data
C                                             value  corresponding  to  the
C                                             ARGS value of 0, as used when
C                                             the image was displayed.
C
C         PVHI            255                 This   is   only   used    if
C                                             QUIET=FALSE,  and is the data
C                                             value  corresponding  to  the
C                                             ARGS value  of  255,  as used
C                                             when the image was displayed.
C
C         OUTPUT                              If present it is the name  of
C                                             the  .BDF  file used to store
C                                             the  LUT   which   has   been
C                                             produced.
C
C
C
C
C
C         NORMALLY DEFAULTED PARAMETERS:-
C
C         LUT             ASPDIR:GLUT         This is the LUT which is used
C                                             -  the  default is the normal
C                                             greyscale one
C
C         QUIET           TRUE                If FALSE it permits output to
C                                             the   VDU   of   the  current
C                                             position  of  the  lower  and
C                                             upper  cut-off points both in
C                                             terms of LUT  entries  (0  to
C                                             255) and data values (PVLO to
C                                             PVHI).
C
C
C
C         USE OF TRACKER-BALL BUTTONS:-
C
C         GREEN 1     Changes the  mode  of  the  display  from  linear  to
C                     logarithmic, and back again
C
C         WHITE 2     Reads the current cursor (x) position  as  the  LOWER
C                     cutoff (ie black, normally).
C
C         WHITE 3     Reads the current cursor (x) position  as  the  UPPER
C                     cutoff (ie white, normally).
C
C         RED   4     Exits from the program.,
C
C
C
C
C
C
C         K F Hartley et al        RGO                             5-AUG-82
C
C
C--------------------------------------------------------------------------



C
C      VERSION #6
C
C      WRITTEN BY K F HARTLEY AT RGO ON 17/12/80
C MODIFIED 2/2/81 W F LUPTON (GREYLUT->LUTLIN VERSION #4)
C MODIFIED 20/2/81 W F LUPTON (SAVE OUTPUT LUT OPTION)
C       MODIFIED 18/9/81 K F HARTLEY (USE TRACKERBALL)
C      Modified by K F Hartley (5/8/82) to output to VDU
C
C   N.B. THAT FOR THE BUTTONS TO BE EFFECTIVE THE CURSOR MUST
C   LIE WITHIN THE IMAGE EVEN THOUGH THE Y-POSITION IS IRRELEVANT.
C
      INTEGER*4 AX(2),STATUS,PIN,POUT,HIST(0:255)
      INTEGER*4 LOGS(0:255),TAB(3,0:255)
      CHARACTER*72 TEXT
C
C      FIRST INITIALIZE THE ARGS , AND STOP ON ERROR
C
      LOGICAL FALS,LEND,QUIET
      FALS=.FALSE.
      LEND=.FALSE.
      CALL SRINIT(0,FALS,IFL)
      IF (IFL.NE.0) CALL EXIT
C
C GET A LOOKUP TABLE
C
   15 CALL RDIMAG('LUT',104,2,AX,I,LUTPTR,STATUS)
      IF (STATUS.NE.0.OR.I.NE.2.OR.AX(1).NE.3.OR.AX(2).NE.256) THEN
         CALL WRERR('HELL')
         CALL CNPAR('LUT',STATUS)
         IF (STATUS.NE.0) THEN
            CALL WRERR('TOOMUC')
            CALL EXIT
         ELSE
            GO TO 15
         END IF
      END IF
C
C   LOAD THE INPUT LUT INTO THE ARGS
C
      CALL SRCOLS(0,256,%VAL(LUTPTR))
C
C      Now see if output to VDU is needed
C
      QUIET=.TRUE.
      CALL RDKEYL('QUIET',.FALSE.,1,QUIET,I,ISTAT)
      IF (.NOT.QUIET) THEN
         PVLO=0.0
         PVHI=255.0
         CALL WRUSER('Enter limits used for display',ISTAT)
         CALL RDKEYR('PVLO',.FALSE.,1,PVLO,I,ISTAT)
         CALL RDKEYR('PVHI',.FALSE.,1,PVHI,I,ISTAT)
         IBX=0
         ITX=255
         WRITE (TEXT,900) IBX,ITX,PVLO,PVHI
         CALL WRUSER(TEXT,ISTAT)
         FAC=(PVHI-PVLO)/255.0
      END IF
C
C   SET-UP INITIAL VALUES
C   BX IS THE LOWER CUTOFF AND TX IS THE UPPER CUTOFF
C   TX IS THE LOWER CUTOFF VALUE (ALWAYS 0) AND TY THE UPPER
C   CUTOFF VALUE (ALWAYS 255) , IN VERSION #6 OF THE PROGRAM.
C
      BX=0.0
      BY=0.0
      TX=255.0
      TY=255.0
C
C      MODE=0 MEANS LINEAR , =1 LOGARITHMIC
C
      MODE=0
C
C      LOGS IS USED TO STORE LOGBASE10 OF 0 TO 255
C
      F=255.0/ALOG10(256.0)
      DO 20 I=0,255
         LOGS(I)=IFIX(ALOG10(FLOAT(I+1))*F)
   20 CONTINUE
C
C   READ THE ARGS DATABASE TO OBTAIN THE SIZE OF THE DISPLAYED
C   IMAGE (ASSUMING IT WAS DISPLAYED "PROPERLY")
C
      CALL ARGS_RDIM(ICX,ICY,ISX,ISY,I,I,ISTAT)
      IF (ISTAT.EQ.0) THEN
         XL=ISX
      ELSE
C
C   IF NO DATABASE ENTRY , ASSUME IT IS FULL SCREEN
C
         XL=512
      END IF
C
C   INITIALIZE THE ARGS CURSOR AND LOOP ON THE BUTTONS
C
      CALL ARGS_CUROP ('1234','G')
  100 CONTINUE
      CALL ARGS_RDCUR(' ',ID,IB,UX,UY)
C
C   CHANGE MODE
C
      IF (IB.EQ.1) THEN
         CALL WRUSER('CHANGING MODE',ISTAT)
         MODE=1-MODE
         CALL OUT (BX,BY,TX,TY,MODE,%VAL(LUTPTR),LOGS,TAB)
         GO TO 100
      END IF
C
C   EXIT FROM LOOP
C
      IF (IB.EQ.4) GO TO 200
C
C   UPDATE LOWER CUTOFF POSITION
C
      IF (IB.EQ.2) BX=UX*255.0/XL
C
C   UPDATE UPPER CUTOFF VALUE
C
      IF (IB.EQ.3) TX=UX*255.0/XL
C
C   NOW GO AND FILL THE LUT AND WRITE IT TO THE ARGS
C
      CALL OUT (BX,BY,TX,TY,MODE,%VAL(LUTPTR),LOGS,TAB)
C
C      Now generate VDU output if needed
C
      IF (.NOT.QUIET) THEN
         BD=PVLO+BX*FAC
         TD=PVLO+TX*FAC
         IBX=BX
         ITX=TX
         WRITE (TEXT,900) IBX,ITX,BD,TD
         CALL WRUSER(TEXT,ISTAT)
  900    FORMAT (' LUT :-',2I5,10X,'DATA :-',2F10.1)
      END IF
C
C   AND RETURN FOR MORE
C
      GO TO 100
C
C "4" WAS HIT - OPTION OF SAVING CURRENT LUT
C
  200 CONTINUE
      CALL ARGS_CURCL
      CALL WRIMAG('OUTPUT',104,AX,2,POUT,ISTAT)
      IF (ISTAT.GT.1) THEN
         CALL WRERR('HELL')
         CALL CNPAR('OUTPUT',STATUS)
         CALL WRERR('TOOMUC')
         CALL EXIT
      ELSE IF (ISTAT.EQ.0) THEN
        CALL ASP_COPCON(104,104,TAB,%VAL(POUT),3*256,ISTAT)
      ENDIF
      CALL FRDATA(' ',ISTAT)
      END
      SUBROUTINE OUT(BX,BY,TX,TY,MODE,COLTAB,LOGS,TAB)
C
C      THIS ROUTINE : FILLS A SET OF LUTS WITH VALUES BETWEEN 0 AND 255
C      AND OUTPUTS IT TO THE ARGS
C
C      THE TABLE (TAB) IS 3 BY 256 (3 COLOURS AND 256 ENTRIES)
C
      INTEGER TAB(3,0:255),COLTAB(3,0:255),LOGS(0:255)
      IX=BX
      IY=BY
      JX=TX
      JY=TY
C
C      SPECIAL CARE IS NEEDED IF THE LINE IS VERTICAL (IX=JX)
C
      IF (IX.NE.JX) THEN
         DO 100 I=0,255
          IF(IX.LT.JX) THEN
            X=BY+FLOAT(I-IX)*(TY-BY)/FLOAT(JX-IX)
          ELSE
            X=TY-FLOAT(I-JX)*(TY-BY)/FLOAT(IX-JX)
          END IF
            IF (X.LT.0.0) X=0.0
            IF (X.GT.255.0) X=255.0
            IVAL=IFIX(X)
            IF (MODE.EQ.1) IVAL=LOGS(IVAL)
            TAB(1,I)=COLTAB(1,IVAL)
            TAB(2,I)=COLTAB(2,IVAL)
            TAB(3,I)=COLTAB(3,IVAL)
  100    CONTINUE
      ELSE
      DO 200 I=0,IX
         TAB(1,I)=COLTAB(1,0)
         TAB(2,I)=COLTAB(2,0)
         TAB(3,I)=COLTAB(3,0)
  200 CONTINUE
      DO 300 I=IX+1,255
         TAB(3,I)=COLTAB(3,255)
         TAB(2,I)=COLTAB(2,255)
         TAB(1,I)=COLTAB(1,255)
  300 CONTINUE
      END IF
      TAB(1,0)=0
      TAB(2,0)=0
      TAB(3,0)=0
      CALL SRCOLS(0,256,TAB)
      IF (IFL.NE.0) THEN
         CALL WRUSER('ERROR IN SRCOLS',ISTAT)
      END IF
      END
