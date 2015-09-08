
C
C+
C	PROGRAM AFRAME
C
C	THIS PROGRAM PRODUCES A FRAME AROUND THE LAST IMAGE THAT
C	WAS DISPLAYED ON THE ARGS. A SCALE CAN BE OPTIONALLY INCLUDED.
C
C	PARAMETERS REQUIRED BY THE PROGRAM ARE:-
C
C	WIDTH: THE WIDTH OF THE FRAME (IN ARGS UNITS)
C	FCOL: THE COLOUR OF THE FRAME  W,R,G,B,Y,M OR C
C	WHERE:-    W  =  WHITE
C		   R  =  RED
C		   G  =  GREEN
C		   B  =  BLUE
C		   Y  =  YELLOW
C		   M  =  MAGENTA
C		   C  =  CYAN
C     SINC:  THE SCALE INCREMENT TO BE DISPLAYED
C
C	D.J.KING     -      16 MARCH 1981
C
C	AMENDED FOR NEW ARGSLIB  -  27 MAY 1981
C
C	AMENDED FOR OVERLAY PLANE  -  30 SEP 1981
C
C-
C
	IMPLICIT INTEGER (A-Z)
	CHARACTER*1 FCOL
	CALL ARGS_NUMIM(IDMAX)
	IF (IDMAX.EQ.0) THEN
		CALL WRERR('NOIMS')
	ELSE
C
C	INITIALSE THE ARGS - STOP IF BAD STATUS
C
		CALL SRINIT(0,.FALSE.,STATUS)
		IF (STATUS.NE.0) THEN
			CALL WRERR('NOARGS')
		ELSE
			CALL ARGS_RDIM(IX,IY,ISX,ISY,I,I,STATUS)
C
C	INQUIRE WIDTH OF FRAME - DEFAULT = 2
C
			WIDTH = 2
			CALL RDKEYI('WIDTH',.TRUE.,1,WIDTH,I,STATUS)
C
C	INQUIRE COLOUR OF FRAME
C
			FCOL='W'
			CALL RDKEYC('FCOL',.TRUE.,1,FCOL,I,STATUS)
			CALL ARGS_OVOP(8,FCOL)
C
C	CALCULATE ORIGIN FOR IMAGE
C
			XOR=IX-(ISX/2)
			YOR=IY-(ISY/2)
C
C	CALCULATE EXTREME VALUES OF X AND Y FOR IMAGE
C
			XEND=XOR+ISX
			YEND=YOR+ISY
C
C	IF WIDTH IS NEGATIVE ( FRAME INSIDE IMAGE ) THEN ADJUST
C	ORIGIN AND EXTREME VALUES
C
			IF (WIDTH.LT.0) THEN
				WIDTH=ABS(WIDTH)
				XOR=XOR+WIDTH
				YOR=YOR+WIDTH
				XEND=XEND-WIDTH
				YEND=YEND-WIDTH
			ENDIF
C
C	CALCULATE THE COORDINATES OF THE DIAGONALLY OPPOSITE
C	CORNERS OF A RECTANGLE WHICH WILL FORM ONE SIDE OF
C	THE FRAME.
C
C	DO THIS FOUR TIMES FOR EACH SIDE OF THE FRAME
C
			XS=XOR-WIDTH
			YS=YOR-WIDTH
			XE=XEND+WIDTH
			YE=YOR
			CALL BLOCK(XS,YS,XE,YE)
			XS=XEND
			YS=YOR-WIDTH
			XE=XEND+WIDTH
			YE=YEND+WIDTH
			CALL BLOCK(XS,YS,XE,YE)
			XS=XOR-WIDTH
			YS=YOR-WIDTH
			XE=XOR
			YE=YEND+WIDTH
			CALL BLOCK(XS,YS,XE,YE)
			XS=XOR-WIDTH
			YS=YEND
			XE=XEND+WIDTH
			YE=YEND+WIDTH
			CALL BLOCK(XS,YS,XE,YE)
C
			CALL RDKEYI('SINC',.TRUE.,1,SINC,I,STATUS)
			IF (SINC.NE.0) THEN
				CALL ARGS_PUT1('2800'X)
				YS=YOR-(WIDTH/2)
				YE=YOR
				DO K=1,2
				DO J=XOR,XEND,SINC
				XE=J+SINC-2
				IF (XE.GT.XEND) XE=XEND
				CALL BLOCK(J,YS,XE,YE)
				ENDDO
				YS=YEND
				YE=YEND+(WIDTH/2)
				ENDDO
				XS=XOR-(WIDTH/2)
				XE=XOR
				DO K=1,2
				DO J=YOR,YEND,SINC
				YE=J+SINC-2
				IF (YE.GT.YEND) YE=YEND
				CALL BLOCK(XS,J,XE,YE)
				ENDDO
				XS=XEND
				XE=XEND+(WIDTH/2)
				ENDDO
			ENDIF
			CALL ARGS_OVCL(8,.FALSE.)
		ENDIF
	ENDIF
	END
	SUBROUTINE BLOCK(XS,YS,XE,YE)
C
C	THIS SUBROUTINE PUTS A RECTANGLE ONTO THE ARGS SCREEN
C	AFTER CHECKING THAT THE COORDINATES ARE WITHIN THE BOUNDS
C	OF THE SCREEN; ANY COORDINATES OUTSIDE THESE BOUNDS WILL
C	BE SET EQUAL TO THE BOUNDS
C
	IMPLICIT INTEGER (A-Z)
	IF (XS.LT.0)  XS=0
	IF (XS.GT.511) XS=511
	IF (YS.LT.0)  YS=0
	IF (YS.GT.511) YS=511
	IF (XE.GT.511)  XE=511
	IF (XE.LT.0) XE=0
	IF (YE.GT.511)  YE=511
	IF (YE.LT.0) YE=0
	CALL ARGS_PUT1('0502'X)
	CALL ARGS_PUT1('C000'X+XS)
	CALL ARGS_PUT1('A000'X+YS)
	CALL ARGS_PUT1('C000'X+XE)
	CALL ARGS_PUT1('E000'X+YE)
	CALL ARGS_PUT1('0500'X)
	CALL SRSEND
	RETURN
	END
