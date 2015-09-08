C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     ********************
C                     *                  *
C                     * Program   XYTORD *
C                     *                  *
C                     ********************
C
C
C
C          CALLING SEQUENCE:-
C               XYTORD
C
C
C          FUNCTION:-
C		It converts X Y positions in an array to
C               to a list of RA's and DEC's  by  refining  a
C               cursor selected position in an image displayed on the ARGS.
C		The results are stored in the file XYTORD.LIS
C
C
C          USE:-
C               It may be used to list a set  of  positions,intensities
C               and  sizes  of  star  images. The positions are stored
C		the file XYTORD.LIS to allow the user to obtain a
C		hard copy of the output from the printer.
C
C
C		The parameter TRCOEFFS is used to define the
C		transformation from X, Y to RA, DEC.
C		Normally the coefficients created by XYFIT
C		will be needed - this is set up using the
C		command procedure CFIXYRD. Otherwise the
C		coefficients may be defined on the command
C		line or in response to the prompt in the
C		"usual" way
C
C
C
C         USER PARAMETERS:-
C
C         INPUT                               This is the name of the image
C                                             which  is  to be analysed. It
C                                             must be the one displayed  on
C                                             the ARGS.
C
C
C
C	 TRCOEFFS				These are the transformation
C						coefficients used to
C						convert from X & Y to
C						RA & DEC, and are six
C						real numbers (Usually
C						created by XYFIT qv.)
C
C
C         USE OF TRACKER-BALL BUTTONS:-
C
C         GREEN 1     Accept this cursor position and do a  fit  to  refine
C                     the position.
C
C         WHITE 2     Decrease the size of the box (to  5*5).  If  the  box
C                     becomes  too  small  no  fit  is  attempted  and  the
C                     position is not recorded.
C
C         WHITE 3     Increase the size of the box. If it  becomes  greater
C                     than  49*49  then  no  star  fitting  is  done  and a
C                     position not recorded.
C
C         RED   4     Exit from the program and do not accept  the  current
C                     position.
C
C
C
C
C
C
C	WRITTEN BY
C
C	B Harrison 		RGO	Version 1	July 1982
C
C
C--------------------------------------------------------------------------



C
C	Based on the program  GTSTAR by K. F. Hartley  RGO
C
      INTEGER*4 PIN,STATUS,AXIN(2),POUT,AXOUT(2)
      REAL POSN(4,100)
      CHARACTER*1 MODE
      CHARACTER*1 MO
      CHARACTER*80 TEXT
      REAL COFS(6)
      LOGICAL FLAG
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'

C
C	Open the file XYTORD.LIS
C

      OPEN(UNIT=1,FILE='XYTORD.LIS',STATUS='NEW')

C
C	If the value of the transformation coefficients have
C	undefined the prompt the user to enter in the value from
C	the keyboard.
C

	CALL RDKEYR('TRCOEFFS',.FALSE.,6,COFS,I,ISTAT)
	IF(ISTAT.NE.ERR_NORMAL) THEN
 	 CALL WRERR('TRERR')
	 GOTO 800
	ENDIF
	FLAG=.TRUE.
C
C      FIRST PICK UP THE IMAGE FRAME
C
      CALL RDIMAG('INPUT',FMT_R,2,AXIN,I,PIN,STATUS)
      IF (STATUS.NE.ERR_NORMAL) THEN
         CALL FRDATA(' ',STATUS)
         CALL EXIT
      END IF

C
C	Print the table headings on the terminal and in the file
C	XYTORD.LIS
C

      WRITE(1,600)
      WRITE(TEXT,600)
      CALL WRUSER(TEXT,ISTAT)
      WRITE(1,601)
      WRITE(TEXT,601)
      CALL WRUSER(TEXT,ISTAT)
      WRITE(1,602)
      WRITE(TEXT,602)
      CALL WRUSER(TEXT,ISTAT)
      WRITE(1,603)
      WRITE(TEXT,603)
      CALL WRUSER(TEXT,ISTAT)
      WRITE(1,601)
      WRITE(TEXT,601)
      CALL WRUSER(TEXT,ISTAT)
      WRITE(1,604)
      WRITE(TEXT,604)
      CALL WRUSER(TEXT,ISTAT)
600   FORMAT(1H ,'                  TITLE: X,Y TO RA,DEC')
601   FORMAT(1H ,' ')
602   FORMAT(1H ,4X,'X',9X,'Y',9X,'RA',18X,'DEC')
603   FORMAT(1H ,4X,'-',9X,'-',9X,'--',18X,'---')
604   FORMAT(1H ,23X,'H  M  S',13X,'D  M  S')
C
C   NOW INITIALIZE ARGS AND FIND OUT IF ANY IMAGES ARE KNOWN
C
      CALL SRINIT(0,.FALSE.,STATUS)
      IF (STATUS.NE.0) THEN
         CALL WRERR('HELLARGS')
         GO TO 800
      END IF
      CALL ARGS_NUMIM(NPIC)
      IF (NPIC.LT.1) THEN
         CALL WRERR('HELLIM')
         GO TO 800
      END IF
C
C      NOW GET POSITIONS OF SELECTED STARS
C
      CALL CROSS(%VAL(PIN),AXIN(1),AXIN(2),COFS,NTOT)
C
C      NOW TIDY UP AND GO HOME
C
  800 CONTINUE
      CALL FRDATA(' ',STATUS)
      CALL ARGS_OVCL(8,.TRUE.)
      CLOSE(UNIT=1)
      CALL EXIT
      END


      SUBROUTINE CROSS(DATA,NX,NY,COFS,NTOT)
C
C      WRITTEN BT K F HARTLEY
C
C      THIS ROUTINE USES THE ARGS CURSOR
C      TO GET POSITIONS OF SELECTED POINTS IN AN IMAGE DISPLAYED
C      ON THE ARGS
C
C      DATA (NX,NY) IS THE DATA ARRAY (NEEDED TO REFINE POSTIONS)
C      COFS IS USED TO STORE THE RESULTING POSITIONS
C      NTOT IS THE TOTAL NUMBER OF POSITIONS SELECTED.
C
      INTEGER NX,NY,N,M,NTOT
      INTEGER IRDATA(3)
      REAL DATA(0:NX-1,0:NY-1)
      REAL COFS(6)
      REAL PART(49,49),W(49)
      LOGICAL*1 INSIDE
      CHARACTER*1 INDCOL,CURCOL
      CHARACTER*80 TEXT
C
C      IPT IS USED TO COUNT THE POINTS SELECTED
C
      IPT=1
C
C      ISIZE IS THE SIZE OF THE BOX USED FOR STAR FITTING
C
      ISIZE=13
C
C   RESPONSES FROM THE CURSOR ARE :_
C      GREEN (LEFT) 1 FINISH THIS STAR - MOVE ON TO NEXT ONE
C      WHITE        2 DECREASE THE BOX SIZE
C      WHITE        3 INCREASE THE BOX SIZE
C      RED   (RIGHT)4 EXIT
C
C
C   LOOP ON THE BUTTONS
C
      NPOS=1
      ITYP=2
      CURCOL='B'
      INDCOL='R'
      IRR=ISIZE
      IXX=255
      IYY=255
      CALL ARGS_NUMIM(ID)
  100 CONTINUE
      CALL ACURS (ITYP,NPOS,IRR,IXX,IYY,IRET,CURCOL,INDCOL,IRDATA)
      IF (IRET.EQ.0) GO TO 200
      ISIZE=2*IRDATA(1)+1
      IXX=IRDATA(2)
C
C      Note the -1 to correct the cursor bug.
C
      IYY=IRDATA(3)-1
      IRR=IRDATA(1)
      IF (IRR.LE.2.OR.IRR.GT.24) GO TO 100
C
C   THEN IMPROVE THE STAR POSITION
C
      CALL ARGS_ATOU(ID,IXX,IYY,UX,UY,ISTAT)
      IX=UX
      IF (IX.LT.0.OR.IX.GE.NX) GO TO 100
      IY=UY
      IF (IY.LT.0.OR.IY.GE.NY) GO TO 100
      JJ=1
      ISH=(ISIZE-1)/2
      DO 120 J=IY-ISH,IY+ISH
         II=1
         DO 110 I=IX-ISH,IX+ISH
            INSIDE=I.GE.1.AND.I.LT.NX.AND.J.GE.1.AND.J.LT.NY
            IF (INSIDE) THEN
               PART(II,JJ)=DATA(I,J)
            ELSE
               PART(II,JJ)=0.0
            END IF
            II=II+1
  110    CONTINUE
         JJ=JJ+1
  120 CONTINUE
      CALL STAR(PART,ISIZE,ISIZE,W,X0,Y0,TINT,WIDE)
      XF=IX+(X0-(ISIZE-1)/2)
      YF=IY+(Y0-(ISIZE-1)/2)

C
C	Transform from the X Y frame to the RA DEC frame
C

      RAS=COFS(1)+COFS(2)*XF+COFS(3)*YF
      DECS=COFS(4)+COFS(5)*XF+COFS(6)*YF

C
C	Convert RA & DEC from radians to degrees minutes seconds
C

      CALL DECODE(RAS,DECS,IHRAS,IAMAS,SRAS,IDDECS,IMDECS,SDECS,
     :      FLAG)

C
C	Print out X, Y, RA, DEC on the terminal and in the file
C	XYTORD.LIS .
C


      IF (FLAG.EQ.0.0) THEN
       WRITE(1,700)XF,YF,IHRAS,IAMAS,SRAS,IDDECS,IMDECS,SDECS
       WRITE(TEXT,700)XF,YF,IHRAS,IAMAS,SRAS,IDDECS,IMDECS,SDECS
       CALL WRUSER(TEXT,ISTAT)
700    FORMAT(1H ,2X,F6.2,4X,F6.2,4X,I2,1X,I2,1X,F5.2,9X,I2,1X,I2,
     :	1X,F5.2)
      ENDIF
      IF (FLAG.EQ.1.0) THEN
	WRITE(1,701)XF,YF,IHRAS,IAMAS,SRAS,IDDECS,IMDECS,SDECS
	WRITE(TEXT,701)XF,YF,IHRAS,IAMAS,SRAS,IDDECS,IMDECS,SDECS
	CALL WRUSER(TEXT,ISTAT)
701	FORMAT(1H ,2X,F6.2,4X,F6.2,4X,I2,1X,I2,1X,F5.2,9XD,'-',I2,1X,
     :	I2,1X,F5.2)
      ENDIF
      CALL ARGS_CROSS(ID,XF,YF,IRR)
      IPT=IPT+1
C
C     NOW GO BACK FOR ANOTHER BUTTON
C
      GO TO 100
  200 CONTINUE
C
C      AT THE END RECORD NUMBER OF POINTS AND REMOVE THE CURSOR
C      FROM THE ARGS.
C
      NTOT=IPT-1
      END

      SUBROUTINE STAR(PIC,NX,NY,W,X0,Y0,TINT,FWHM)
*
*    WRITTEN BY P T WALLACE
*  DETERMINE BRIGHTNESS, POSITION & SIZE OF STAR
*
*  GIVEN:
*     PIC         2-D ARRAY CONTAINING STAR IMAGE
*     NX,NY       DIMENSIONS OF THE PART OF ARRAY USED
*     W           WORK ARRAY
*

      INTEGER NX,NY
      REAL PIC(49,49),W(49)

      CHARACTER*80 LINE


*  SAMPLE BACKGROUND
      BG=PIC(1,1)

*  COLLAPSE INTO X VERSUS Z
      DO IX=1,NX
         W(IX)=0.0
      END DO
      DO IY=1,NY
         DO IX=1,NX
            W(IX)=W(IX)+(PIC(IX,IY)-BG)
         END DO
      END DO

*  PROCESS X PROFILE
      CALL PP(W,NX,XINT,X0,XW)

*  COLLAPSE INTO Y VERSUS Z
      DO IY=1,NY
         S=0.0
         DO IX=1,NX
            S=S+(PIC(IX,IY)-BG)
         END DO
         W(IY)=S
      END DO

*  PROCESS Y PROFILE
      CALL PP(W,NY,YINT,Y0,YW)

*  ESTIMATE INTEGRATED INTENSITY AND FWHM
      TINT=(XINT+YINT)/2.0
      FWHM=(XW+YW)/2.0
      END

      SUBROUTINE PP(PRFL,N,TINT,C,W)
*
*  EVALUATE 1-D STAR PROFILE
*
*  GIVEN:
*     PRFL        1-D ARRAY CONTAINING STAR PROFILE
*     N           SIZE OF ARRAY
*
*  RETURNED:
*     TINT        TOTAL INTENSITY (ABOVE BACKGROUND)
*     C           CENTRE
*     W           FWHM
*

      INTEGER N
      REAL PRFL(N),TINT,C,W

      DOUBLE PRECISION SZ,SZP,SZP2


*  BASELINE PARAMETERS (STRAIGHT LINE THROUGH ENDPOINTS)
      Z0=PRFL(1)
      SLOPE=(PRFL(N)-Z0)/REAL(N-1)

*  PERFORM SUMMATIONS
      SZ=0D0
      SZP=0D0
      SZP2=0D0
      DO I=2,N-1
         P=REAL(I-1)
         Z=PRFL(I)-(Z0+SLOPE*P)
         SZ=SZ+DBLE(Z)
         ZP=Z*P
         SZP=SZP+DBLE(ZP)
         SZP2=SZP2+DBLE(ZP*P)
      END DO

*  FUDGE TOTAL INTENSITY IF VERY SMALL
      TINT=REAL(SZ)
      IF (ABS(TINT).LT.1E-10) SZ=1D0

*  DETERMINE CENTRE OF GRAVITY AND FWHM (ASSUMING GAUSSIAN)
      C=REAL(SZP/SZ)
      W=2.35482*SQRT(MAX(REAL((SZP2-SZP*SZP/SZ)/SZ),0.0))

      END

      SUBROUTINE ARGS_CROSS(ID,XF,YF,IS)
      REAL VX(2),VY(2),HX(2),HY(2)
      D=REAL(IS)
      VX(1)=XF-D
      VX(2)=XF+D
      VY(1)=YF
      VY(2)=YF
      HX(1)=XF
      HX(2)=XF
      HY(1)=YF-D
      HY(2)=YF+D
      CALL ARGS_OVC(3,'G')
      CALL ARGS_S1('ZWE1','0400'X)
      CALL ARGS_S1('ZDI1',1024)
      CALL ARGS_POLYL(ID,2,HX,HY,ISTAT)
      CALL ARGS_POLYL(ID,2,VX,VY,ISTAT)
      END
