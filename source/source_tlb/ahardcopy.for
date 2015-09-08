      PROGRAM AHARDCOPY
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     ***********************
C                     *                     *
C                     * Program   AHARDCOPY *
C                     *                     *
C                     ***********************
C
C
C
C          CALLING SEQUENCE:-
C
C   AHARDCOPY [XLIMITS=I1,I2] [YLIMITS=J1,J2] [SQUARE=logical] [THRESH=value]
C
C
C          FUNCTION:-
C               Copies  the  current  Args  display  onto  the   Printronix
C               lineprinter.  By default the entire 512 x 512 pixel area is
C               copied from the Args memory, and plotted on the printer  as
C               any  array  of 512 x 512 dots. The sign of the value THRESH
C               determines if  pixel  values  above/below  this  value  are
C               represented as blank/black dots on the printer. If the sign
C               is positive then pixel values below  THRESH will  be  blank
C               while pixel values equal to or above THRESH will  be black.
C               The converse applies if the sign of  the  value  THRESH  is
C               negative. Optionally, any rectangular region  of  the  Args
C               screen may be plotted by specifying XLIMITS and YLIMITS  on
C               the command line.  The  plot  is automatically  sent to the
C               line printer queue. Note that since the line printer pixels
C               do not  have equal  spacing in the X and Y  directions, the
C               plot is not square unless the SQUARE option is specified on
C               the command line.
C
C          USE:-
C               Useful for getting a hard copy of  any  graphics  currently
C               displayed  on  the  Args.   Since  the lineprinter can only
C               display a pixel as either white or black, this routine does
C               not work well on  grey  scale  images  unless  the  program
C               THRESH has been run to define a  threshold  value, in which
C               case set THRESH=THRESH_OFFSET on the command line.
C
C
C
C         USER PARAMETERS:-
C
C         LABEL                               Optional   label   for    the
C                                             lineprinter plot.
C
C
C         NORMALLY DEFAULTED PARAMETERS:-
C
C         XLIMITS         0,511               Minimum and  maximum  columns
C                                             on the Args to be plotted.
C
C         YLIMITS         0,511               Minimum and maximum  rows  on
C                                             the Args to be plotted.
C
C         SQUARE          FALSE               If true, every 5th row is
C                                             printed twice, so that the
C                                             overall image has the correct
C                                             proportions in X and Y.
C
C         THRESH          0                   Defines  the threshold  level
C                                             at which black  becomes white
C                                             or  vice  versa  depending on
C                                             the  sign  of  the  threshold
C                                             value.
C
C
C         W D Pence                AAO                            13-JAN-83
C
C
C
C
C
C--------------------------------------------------------------------------



C
C     COPIES THE ARGS IMAGE AND PRINTS IT ON THE PRINTRONIX
C
      LOGICAL SQUARE
      INTEGER IDIMN(2),IX(2),IY(2),THRESH
      CHARACTER*80 LABEL
      CALL SRINIT(0,.FALSE.,ISTAT)
      IF (ISTAT .NE. 0)THEN
        CALL WRUSER('UNABLE TO GET ARGS.',ISTAT)
        STOP
      END IF
C
C     GET BOUNDARY OF RECTANGULAR AREA TO BE OUTPUT
C
      GO TO 12
10    CALL CNPAR('XLIMITS',ISTAT)
      CALL CNPAR('YLIMITS',ISTAT)
12    CALL RDKEYI('XLIMITS',.FALSE.,2,IX,NXVALS,ISTAT)
      IF (NXVALS .LT. 2 )THEN
        CALL WRUSER('MUST INPUT MIN AND MAX LIMITS',ISTAT)
        GO TO 10
      END IF
      IF (IX(1) .GE. IX(2))THEN
        CALL WRUSER('FIRST LIMIT MUST BE LESS THAN THE SECOND',
     1       ISTAT)
        GO TO 10
      END IF
      IF (IX(1) .LT. 0 .OR. IX(2) .GT. 511)THEN
        CALL WRUSER('LIMITS MUST BE IN THE RANGE 0 TO 511',ISTAT)
        GO TO 10
      END IF
      CALL RDKEYI('YLIMITS',.FALSE.,2,IY,NYVALS,ISTAT)
      IF (NYVALS .LT. 2)THEN
        CALL WRUSER('MUST INPUT MIN AND MAX LIMITS',ISTAT)
        GO TO 10
      END IF
      IF (IY(1) .GE. IY(2))THEN
        CALL WRUSER('FIRST LIMIT MUST BE LESS THAN THE SECOND',
     1       ISTAT)
        GO TO 10
      END IF
      IF (IY(1) .LT. 0 .OR. IY(2) .GT. 511)THEN
        CALL WRUSER('LIMITS MUST BE IN THE RANGE 0 TO 511',ISTAT)
        GO TO 10
      END IF
C
      CALL RDKEYL('SQUARE',.FALSE.,1,SQUARE,NVAL,ISTAT)
C
      CALL RDKEYC('LABEL',.FALSE.,1,LABEL,NVAL,IS)
C
      IDIMN(1)=IX(2)-IX(1)+1
      IDIMN(2)=IY(2)-IY(1)+1
      NSIZE=IDIMN(1)*IDIMN(2)
C
C     GET WORK SPACE LARGE ENOUGH TO HOLD THE ARGS IMAGE
C
      CALL GETDYN('IMAGE',102,NSIZE,IPIN,JSTAT)
C
C     LOAD THE ARGS IMAGE INTO THE WORK SPACE
C
      CALL SRFPR(%VAL(IPIN),IDIMN(1),IDIMN(1),IDIMN(2),IX(1),IY(1))
C
C     GET THE THRESHOLD VALUE
C
      CALL RDKEYI('THRESH',.FALSE.,1,THRESH,IVALS,ISTAT)
C
C     DUMP THE IMAGE TO A FILE SUITABLE FOR PRINTING ON THE LINEPRINTER
C
      CALL HRDCOPY(%VAL(IPIN),IDIMN(1),IDIMN(2),LABEL,SQUARE,THRESH)
C
C     USE VMS COMMAND TO PRINT THE FILE
C
      ISTATUS=LIB$SPAWN(
     : '$PRINT/NOFEED/DELETE/PASSALL/QUEUE=SYS_PRINTRONIX AHARDCOPY')
      END
C***********************************************************************
      SUBROUTINE SRFPR(IB2,JD1,NX,NY,X,Y)
C     --------------------------------------------------------
C READS RECTANGULAR PIXEL ARRAY FROM THE ARGS. EACH PIXEL VALUE
C IS CONTAINED IN ONE INTEGER*2 ARRAY ELEMENT.
C 1ST SUBSCRIPT INCREASES WITH INCREASING X.
C 2ND SUBSCRIPT CORRESPONDS TO Y
C ARGUMENTS:
C    IB2   (EXIT) 2-D ARRAY CONTAINING PIXEL VALUES.
C    JD1   (ENTRY) 1ST DIMENSION OF ACTUAL ARRAY CORRESPDG TO IB2
C    NX,NY  (ENTRY) NUMBER OF PIXELS IN X- AND Y- DIRECTIONS
C    X,Y    (ENTRY) BOTTOM LEFT OF RECTANGLE TO BE COPIED
C ----------------------------------------------------------------
      PARAMETER BITS=16
      INTEGER JD1
      INTEGER*2 IB2(JD1,NY)
      INTEGER NX,NY, X,Y
      INTEGER PPE,HLINES
      INTEGER*2 CHECK
      REAL PPB
C   CHECK ARGUMENTS
      CHECK=IB2(1,1)
      CHECK=IB2(JD1,NY)
      CHECK=IB2(NX,NY)
C
C               NUMBER OF (P)IXELS (P)ER ARRAY (E)LEMENT
C               & PER (B)YTE (LATTER IS REAL BECAUSE IT COULD BE <1).
C
      PPE=16/BITS
      PPB=8.0/FLOAT(BITS)
      HLINES=10000
C
C     ADJUST HLINES TO THE SPACE ALLOWED BY ARGS_FPRI2
C
      HLINES=MIN(HLINES,(32767*PPE)/NX, (INT(32767.0*PPB)/NX))
C
C     CAN READ CHUNKS OF POSSIBLY MORE THAN 1 ROW.
C
      DO  JY=1,NY,HLINES
C       (NY2 IS # OF ROWS TO BE PUT THIS ITERATION)
        NY2=MIN(HLINES,NY-JY+1)
        CALL ARGS_FPRI2(IB2(1,JY),NY2*NX,X,Y+JY-1,
     1          NX,NY2)
      END DO
      END
C -----------------------------------------------------------------
      SUBROUTINE ARGS_FPRI2(BUFF,N,X1,Y1,NX,NY)
C     -----------------------------------------------
C READS PACKED PIXEL ARRAY FROM ARGS.
C ARGUMENTS
C   BUFF   (ARRAY) ON OUTPUT CONTAINS PACKED DATA (INTEGER*2)
C   N      (ENTRY) NUMBER OF INTEGER*2 ELEMENTS IN BUFF
C   X1,Y1  (ENTRY) BOTTOM LEFT OF PIXEL IMAGE.
C   NX,NY  (ENTRY) NUMBER OF PIXELS TO BE OCCUPIED IN X & Y RESP.
C
C -------------------------------------------------------------------
      INTEGER*2 BUFF(N),IBUFF(32771)
      INTEGER X1,Y1,NX,NY
      INTEGER NW,AW,PPAW,NAME
      INTEGER*2 CHECK,IRDINS(10)
C
      INTEGER*2 IJUNK
      LOGICAL LPRT,LARGSD,LARGSS,INITD
      COMMON /CARGS1/IJUNK(520),L1,L2,L3,LPRT,LARGSD,LARGSS,
     1 CHAN,INITD,L4
      DATA NW/1/
      DATA NAME/3HFPR/
C
C   CHECK ARRAY EXTREMITIES
      CHECK=BUFF(1)
      CHECK=BUFF(N)
C   CHECK NUMBER OF WORDS
      IF( N*NW.GT.32767 ) CALL ARGS_ITF(12,.TRUE.,N,N,Z)
C   (P)IXELS (P)ER (A)RGS (W)ORD
      PPAW=1
C   NEED EXACT NUMBER OF ARGS DATA WORDS OR FEWER.
      AW=MIN( N*NW, (NX*NY+PPAW-1)/PPAW)
C
C   MOVE PEN, OUTPUT ARGS ORDER & THEN READ THE IMAGE.
C
      CALL ARGS_S1(3HXMA,X1)
      CALL ARGS_S1(3HYMA,Y1)
C
C     OPCODE FOR READING ARGS MEMORY = '6B04'
C
      IRDINS(5)='6B04'X
      IRDINS(6)=X1
      IRDINS(7)=Y1
      IRDINS(8)=NX
      IRDINS(9)=NY
      IRDINS(10)=AW
C
C     ARGS_IDCALLRD SENDS 'IRDINS' TO THE ARGS AND RECEIVES
C        THE REPLY IN IBUFF.  THERE ARE 4 HEADER WORDS IN THE
C        BEGINNING OF EACH BUFFER WHICH MUST BE IGNORRED.
C
      CALL ARGS_IDCALLRD(1,CHAN,IRDINS,10,IBUFF,AW+4,NOK)
C
      DO I=1,AW
        BUFF(I)=IBUFF(I+4)
      END DO
      END
C
C***********************************************************************
      SUBROUTINE HRDCOPY(IRAY,ICOL,IROW,LABEL,SQUARE,THRESH)
C
C     HRDCOPY:
C     DEFINES PARAMETERS AND ARRAYS FOR MAKING A GREY SCALE PLOT
C     ON THE  PRINTRONIX PRINTER/PLOTTER
C
C     IRAY = 2 DIMENSIONAL IMAGE ARRAY (I*2)
C     ICOL = SIZE OF THE FIRST DIMENSION OF IRAY (I*4)
C     IROW = SIZE OF THE SECOND DIMENSION OF IRAY (I*4)
C
      PARAMETER WIDE=13.1, NUNIT=8
C
C     WIDE = WIDTH OF PAGE IN INCHES
C
      LOGICAL SQUARE
      INTEGER THRESH,FF
      INTEGER*2 IRAY(ICOL,IROW)
      DIMENSION DAT(2200)       
      CHARACTER LABEL*80
      DATA FF/'0A'X/
C
C     PLOT FROM TOP TO BOTTOM
C
      OPEN(UNIT=NUNIT,NAME='AHARDCOPY.LIS',TYPE='NEW',
     :     RECL=134,CARRIAGECONTROL='NONE')
C
	IF (THRESH.LT.O) THEN
		THRESH=-THRESH
		ISIGN=-1
	ELSE
		ISIGN=1
	ENDIF
      DO 100 J=IROW,1,-1
        K=0
        DO 90 I=1,ICOL
          K=K+1
	  IF (IRAY(I,J).GT.THRESH) THEN
		IF (ISIGN.EQ.1) THEN
			DAT(K)=1
		ELSE
			DAT(K)=0
		ENDIF
	  ELSE
		IF (ISIGN.EQ.1) THEN
			DAT(K)=0
		ELSE
			IF (IRAY(I,J).EQ.0) THEN
				DAT(K)=0
			ELSE
				DAT(K)=1
			ENDIF
		ENDIF
	  ENDIF
 90     CONTINUE
        IF (MOD(J,5) .EQ. 0 .AND. SQUARE)THEN
C
C       REPEAT EVERY FIFTH LINE ON THE PRINTRONIX SO PLOT IS SQUARE
C
          ITWO=2
        ELSE
          ITWO=1
        END IF
        CALL PLOTIT(DAT,K,NUNIT,ITWO)
100   CONTINUE
C
      WRITE(NUNIT,2000) FF
2000  FORMAT(1X,A1)
      WRITE(NUNIT,2001) LABEL,FF
2001  FORMAT(10X,A,A1)
      CLOSE(UNIT=NUNIT)              
C
      END
C***********************************************************************
      SUBROUTINE PLOTIT(DAT,NSAMP,UNIT,ITWO)
C
      PARAMETER MAXCHAR=131, NPWIDE=786
      LOGICAL*1 XMASK(6),FIRST,LAST
      INTEGER UNIT
      LOGICAL*1 IOUT(MAXCHAR)
      DIMENSION DAT(NSAMP)
      DIMENSION IBIT(NPWIDE),DATA(NPWIDE)
      DATA XMASK/'01'X,'02'X,'04'X,'08'X,'10'X,'20'X/
      DATA FIRST,LAST/'05'X,'0A'X/
C
C ..IPIX IS THE NUMBER OF BITS PER PIXEL...
C
      IPIX=1
      ILENGTH=1
      ISTART=50
C
      DO 400 IROW=1,ILENGTH
C
        DO ISET=1,NPWIDE
          IBIT(ISET)=0
        END DO
        DO ISET = 1,MAXCHAR
          IOUT(ISET)='C0'X
        END DO
C
C ..THIS LOOP COUNTS THROUGH THE NUMBER OF ELEMENTS OF DAT...
C
        DO 300 ISAMP=1,NSAMP
C
C ..THIS LOOP COUNTS THROUGH THE NUMBER OF BITS IN EACH PIXEL...
C
          DO 200 IBITS=1,IPIX
C
C ..THE POSITION OF THE BIT UNDER CONSIDERATION IS CALCULATED...
C
            K=(ISAMP-1)*IPIX+IBITS+ISTART
C
            IF (DAT(ISAMP).GT.0.) IBIT(K)=1
C
  200     CONTINUE
  300   CONTINUE
C
C ..THE WORDS CONTAINING THE BITS TO WRITE TO THE TAPE...
C ..ARE THEN ASSEMBLED FROM THE BITS ALREADY FORMED...
C
      DO I=1,NPWIDE
        IF (IBIT(I) .EQ. 1)THEN
          KWORD=1+((I-1)/6)
          KBIT=1+MOD(I-1,6)
          IOUT(KWORD)=IOUT(KWORD).OR.XMASK(KBIT)
        END IF
      END DO
C
C ..THE LINE IS NOW WRITTEN TO THE FILE...
C
      WRITE(UNIT,99)FIRST,IOUT,LAST
99    FORMAT(133A1)
C
C     DUPLICATE THE LINE IF ITWO=2
C
      IF (ITWO .EQ. 2)THEN
        WRITE(UNIT,99)FIRST,IOUT,LAST
      END IF
  400 CONTINUE
C
      END
