	PROGRAM FFT
C
C++	STARLINK ENVIRONMENT PROGRAM ****FFT****
C
C	VERSION #1
C
C	WRITTEN BY W F LUPTON NOVEMBER 1980
C
C	GENERAL FFT ROUTINE TO WORK (AUTOMATICALLY) IN BOTH
C	1D AND 2D
C
C	PARAMETERS:-
C	RIN	REAL IN DATA FRAME
C	IIN	IMAGINARY IN DATA FRAME (IF GIVEN A NULL VALUE
C		THE IN FRAME IS ASSUMED TO BE REAL AND A ZERO
C		IMAGINARY PART IS DYNAMICALLY OBTAINED)
C	INV	LOGICAL VALUE TRUE IFF INV IS TO BE TAKEN
C	ROUT	REAL OUT DATA FRAME (IF NULL, NOT PRODUCED)
C	IOUT	IMAGINARY OUT DATA FRAME (IF NULL, NOT PRODUCED)
C	POWER	FRAME NAME FOR THE POWER SPECTRUM (R*R+I*I)
C		IF NULL, POWER SPECTRUM IS NOT CALCULATED
C	PHASE	FRAME NAME FOR THE PHASE (ARCTAN(I/R) IN [0,2*PI) )
C		IF NULL, PHASE IS NOT CALCULATED
C	QUIET	IFF TRUE NO ECHO OF PROGRESS IS GIVEN
C
      INCLUDE 'INTERIM(FMTPAR)'
      INCLUDE 'INTERIM(ERRPAR)'
	INTEGER RAXIS(2),IAXIS(2),AXIS2(2),ERROR,RDIM,IDIM,RSIZE,
     +	EXSIZE,PNTR(2),XPNTR(2),WPNTR1,WPNTR2,WPNTR3,PPNTR,PHPNTR
	LOGICAL INVERS,QUIET
	CHARACTER DYN(2)*4
	DATA DYN/'RDYN','IDYN'/
C
C++	READ NAMES OF INPUT FRAMES (THE REAL PART AND THE IMAGINARY
C--	PART ARE EACH STORED IN A SEPARATE FRAME).
C
10	CALL RDIMAG('RIN',FMT_R,2,RAXIS,RDIM,PNTR(1),ERROR)
	IF (ERROR.NE.ERR_NORMAL) THEN
		CALL WRUSER('ERROR READING REAL INPUT - RETRY',ERROR)
		CALL FRDATA('RIN',ERROR)
		CALL CNPAR('RIN',ERROR)
		GOTO 10
	ENDIF
	IF (RDIM.GT.2) THEN
		CALL WRUSER('DIM OF REAL INPUT FRAME GT 2 - RETRY',ERROR)
		CALL FRDATA('RIN',ERROR)
		CALL CNPAR('RIN',ERROR)
		GOTO 10
	ENDIF
	RSIZE=1
	DO I=1,RDIM
		RSIZE=RSIZE*RAXIS(I)
	ENDDO
C
C++	IF A NULL RESPONSE IS RETURNED FOR THE IMAGINARY PART,
C	THEN IT IS ASSUMED THAT A ZERO FRAME IS DYNAMICALLY TO BE
C	PROVIDED.
C--
20	CALL RDIMAG('IIN',FMT_R,2,IAXIS,IDIM,PNTR(2),ERROR)
	IF (ERROR.EQ.ERR_PARNUL) THEN
		CALL CNPAR('IIN',ERROR)
		CALL GETDYN('IIN',FMT_R,RSIZE,PNTR(2),ERROR)
		IF (ERROR.NE.ERR_NORMAL) THEN
			CALL WRERR('GETINP')
			GOTO 999
		ENDIF
		CALL REALCONST(%VAL(PNTR(2)),RSIZE,0.0)
	ELSE IF (ERROR.NE.ERR_NORMAL) THEN
		CALL WRUSER('ERROR GETTING IMAG INPUT FRAME - RETRY',ERROR)
		CALL CNPAR('IIN',ERROR)
		GOTO 20
	ELSE
		IF (IDIM.NE.RDIM) THEN
			CALL WRUSER('REAL AND IMAG INPUT FRAMES'//
     +			'ARE OF DIFF DIM - RETRY',ERROR)
			CALL FRDATA('IIN',ERROR)
			CALL CNPAR('IIN',ERROR)
			GOTO 20
		ENDIF
		DO I=1,RDIM
			IF (RAXIS(I).NE.IAXIS(I)) THEN
				CALL WRUSER('AN AXIS LENGTH DIFFERS '//
     +				'IN REAL AND IMAG INPUT FRAMES - RETRY',
     +				ERROR)
				CALL FRDATA('IIN',ERROR)
				CALL CNPAR('IIN',ERROR)
				GOTO 20
			ENDIF
		ENDDO
	ENDIF
C
C++	INCREASE FRAME SIZE TO NEXT POWER OF TWO (ZEROES ARE USED
C	FOR EXTENSION. THEN PERPORM THE TRANSFORM IN DYNAMIC
C	DOUBLE PRECISION STORAGE BEFORE COPYING BACK TO REALS.
C--
C
C	CALCULATE (IN AXIS2) THE LARGEST DIMENSION
C
	M1=0
	EXSIZE=1
	AXIS2(2)=1
	DO I=1,RDIM
		J=MAX(0,INT(LOG(RAXIS(I)-0.99)/LOG(2.0)))+1
		AXIS2(I)=2**J
		M1=MAX(M1,J+1)
		EXSIZE=EXSIZE*AXIS2(I)
	ENDDO
	DO I=1,2
		CALL GETDYN(DYN(I),FMT_DP,EXSIZE,XPNTR(I),ERROR)
		IF (ERROR.NE.ERR_NORMAL) THEN
			CALL WRERR(DYN(I))
			GOTO 999
		ENDIF
C
C	HAVING ASSIGNED THE DYNAMIC MEMORY, NOW COPY THE DATA INTO IT
C	USING SUBROUTINE RTODP
C
		CALL RTODP(%VAL(PNTR(I)),RAXIS(1),RAXIS(2),
     +			  %VAL(XPNTR(I)),AXIS2(1),AXIS2(2),ERROR)
		IF (ERROR.NE.ERR_NORMAL) THEN
			GOTO 999
		ENDIF
	ENDDO
C
C	FOR THE NAG (FOURIER TRANSFORM) ROUTINE AND FOR FFT2D NEED
C	THREE REAL*8 WORK ARRAYS. OBTAIN DYNAMIC STORAGE
C
	CALL GETDYN('WORKSP1',FMT_DP,M1,WPNTR1,ERROR)
	IF (ERROR.NE.ERR_NORMAL) THEN
		CALL WRERR('GETWORK')
		GOTO 999
	ENDIF
	CALL GETDYN('WORKSP2',FMT_DP,AXIS2(2),WPNTR2,ERROR)
	IF (ERROR.NE.ERR_NORMAL) THEN
		CALL WRERR('GETWORK')
		GOTO 999
	ENDIF
	CALL GETDYN('WORKSP3',FMT_DP,AXIS2(2),WPNTR3,ERROR)
	IF (ERROR.NE.ERR_NORMAL) THEN
		CALL WRERR('GETWORK')
		GOTO 999
	ENDIF
C
C	READ INV
C
30	CALL RDKEYL('INV',.FALSE.,1,INVERS,I,ERROR)
	IF (ERROR.NE.ERR_NORMAL) THEN
		CALL WRUSER('ERROR READING INV',ERROR)
		CALL CNPAR('INV',ERROR)
		GOTO 30
	ENDIF
C
C	READ QUIET
C
35	CALL RDKEYL('QUIET',.FALSE.,1,QUIET,I,ERROR)
	IF (ERROR.NE.ERR_NORMAL) THEN
		CALL WRUSER('ERROR READING QUIET',ERROR)
		CALL CNPAR('QUIET',ERROR)
		GOTO 35
	ENDIF
C++	NOW ALL THE STARLINK-SPECIFIC ROUTINES HAVE BEEN CALLED,
C	SO CALL C06ABF OR FFT2D DEPENDING ON THE FRAME DIMENSION
C--
	IF (RDIM.EQ.1) THEN
		CALL C06ABF(%VAL(XPNTR(1)),%VAL(XPNTR(2)),AXIS2(1),
     +		INVERS,M1,%VAL(WPNTR1))
	ELSE
		CALL FFT2D(%VAL(XPNTR(1)),%VAL(XPNTR(2)),AXIS2(1),
     +		AXIS2(2),INVERS,M1,%VAL(WPNTR1),%VAL(WPNTR2),
     +		%VAL(WPNTR3),QUIET)
	ENDIF
C
C	NOW MUST WRITE THE TRANSFORMED DATA TO THE OUTPUT FRAMES
C
40	CALL WRIMAG('ROUT',FMT_R,AXIS2,RDIM,PNTR(1),ERROR)
	IF (ERROR.EQ.ERR_NORMAL) THEN
		CALL DPTOR(%VAL(XPNTR(1)),%VAL(PNTR(1)),AXIS2(1)*AXIS2(2))
	ELSE IF (ERROR.NE.ERR_PARNUL) THEN
		CALL WRUSER('ERROR WRITING REAL OUTPUT FRAME - RETRY',
     +		ERROR)
		CALL CNPAR('ROUT',ERROR)
		GOTO 40
	ENDIF
50	CALL WRIMAG('IOUT',FMT_R,AXIS2,RDIM,PNTR(2),ERROR)
	IF (ERROR.EQ.ERR_NORMAL) THEN
		CALL DPTOR(%VAL(XPNTR(2)),%VAL(PNTR(2)),AXIS2(1)*AXIS2(2))
	ELSE IF (ERROR.NE.ERR_PARNUL) THEN
		CALL WRUSER('ERROR WRITING IMAG OUTPUT FRAME - RETRY',
     +		ERROR)
		CALL CNPAR('IOUT',ERROR)
		GOTO 50
	ENDIF
60	CALL WRIMAG('POWER',FMT_R,AXIS2,RDIM,PPNTR,ERROR)
	IF (ERROR.EQ.ERR_NORMAL) THEN
		CALL POWCALC(%VAL(XPNTR(1)),%VAL(XPNTR(2)),%VAL(PPNTR),
     +		AXIS2(1)*AXIS2(2))
	ELSE IF (ERROR.NE.ERR_PARNUL) THEN
		CALL WRUSER('ERROR WRITING POWER - RETRY',ERROR)
		CALL CNPAR('POWER',ERROR)
		GOTO 60
	ENDIF
70	CALL WRIMAG('PHASE',FMT_R,AXIS2,RDIM,PHPNTR,ERROR)
	IF (ERROR.EQ.ERR_NORMAL) THEN
		CALL PHASCALC(%VAL(XPNTR(1)),%VAL(XPNTR(2)),%VAL(PHPNTR),
     +		AXIS2(1)*AXIS2(2))
	ELSE IF (ERROR.NE.ERR_PARNUL) THEN
		CALL WRUSER('ERROR WRITING PHASE - RETRY',ERROR)
		CALL CNPAR('PHASE',ERROR)
		GOTO 70
	ENDIF
C
C	THAT'S IT
C
999	CALL EXIT
	END
C
	SUBROUTINE REALCONST(ARRAY,ISIZE,VAL)
C
C	ROUTINE TO SET A REAL ARRAY TO A CONSTANT VALUE
C
	REAL ARRAY(ISIZE),VAL
	DO I=1,ISIZE
		ARRAY(I)=VAL
	ENDDO
	RETURN
	END
C
	SUBROUTINE RTODP(SINGLE,I1,I2,DOUBLE,D1,D2,ERROR)
C
C	ROUTINE TO COPY A REAL*4 ARRAY TO A REAL*8 ONE.
C	THE INPUT ARRAY FILLS THE 'BOTTOM LEFT HAND CORNER' OF
C	THE OUTPUT ONE (AND ZEROES THE REST)
C
C	WFL NOV 1980
C
	INTEGER I1,I2,D1,D2,ERROR
	REAL SINGLE(I1,I2)
	REAL*8 DOUBLE(D1,D2)
C
	DO I=1,D2
		DO J=1,D1
			IF (J.LE.I1.AND.I.LE.I2) THEN
				DOUBLE(J,I)=SINGLE(J,I)
			ELSE
				DOUBLE(J,I)=0.0
			ENDIF
		ENDDO
	ENDDO
C
	RETURN
	END
C
	SUBROUTINE POWCALC(RDOUBLE,IDOUBLE,SINGLE,D)
C
C	ROUTINE TO CALCULATE POWER SPECTRUM FROM REAL & IMAGINARY
C	PARTS STORED IN RDOUBLE AND IDOUBLE AND PLACE IT IN SINGLE
C
C	WFL NOV 1980
C
	INTEGER D
	REAL SINGLE(D)
	REAL*8 RDOUBLE(D),IDOUBLE(D)
C
	DO I=1,D
		SINGLE(I)=RDOUBLE(I)*RDOUBLE(I)+IDOUBLE(I)*IDOUBLE(I)
	ENDDO
C
	RETURN
	END
C
	SUBROUTINE PHASCALC(RDOUBLE,IDOUBLE,SINGLE,D)
C
C	ROUTINE TO CALCULATE PHASE FROM REAL & IMAGINARY
C	PARTS STORED IN RDOUBLE AND IDOUBLE AND PLACE IT IN SINGLE
C
C	WFL FEB 1981
C
	INTEGER D
	REAL SINGLE(D),PP
	REAL*8 RDOUBLE(D),IDOUBLE(D),RR,II
	PARAMETER (PI=3.141592654)
C
	DO I=1,D
		RR=RDOUBLE(I)
		II=IDOUBLE(I)
		IF (RR.EQ.0.0.AND.II.EQ.0.0) THEN
			RR=1.0
		ENDIF
		PP=ATAN2(RR,II)
		IF (PP.LT.0.0) THEN
			PP=PP+PI
		ENDIF
		SINGLE(I)=PP
	ENDDO
C
	RETURN
	END
C
	SUBROUTINE DPTOR(DOUBLE,SINGLE,D)
C
C	ROUTINE TO COPY A R*8 ARRAY TO A REAL*4 ONE
C
C	WFL NOV 1980
C
	INTEGER D
	REAL SINGLE(D)
	REAL*8 DOUBLE(D)
C
	DO I=1,D
		SINGLE(I)=DOUBLE(I)
	ENDDO
C
	RETURN
	END
C
	SUBROUTINE FFT2D(A,B,N1,N2,INVERS,M1,C,D,E,QUIET)
C
C	INTERFACE BETWEEN STARLINK FFT PROGRAM (2D) AND C06ABF ROUTINE
C
C	WFL NOV 1980
C
	INTEGER N1,N2,M1
	LOGICAL INVERS,QUIET
	REAL*8 A(N1,N2),B(N1,N2),C(M1),D(N2),E(N2)
	CHARACTER MESSAGE*20,NUM*6
C
C	FIRST OF ALL TRANSFORM ALL THE ROWS OF A & B
C
	MESSAGE=' ROWS TRANSFORMED'
	INFO=0
	DO I=1,N2
		CALL C06ABF(A(1,I),B(1,I),N1,INVERS,M1,C)
		INFO=MOD(INFO+1,10)
		IF (.NOT.QUIET.AND.INFO.EQ.0) THEN
			WRITE (NUM,'(I6)') I
			CALL WRUSER(NUM//MESSAGE,ERROR)
		ENDIF
	ENDDO
C
C	NOW (SLIGHTLY MESSIER) TRANSFORM THE COLUMNS
C
	MESSAGE=' COLUMNS TRANSFORMED'
	INFO=0
	DO I=1,N1
		DO J=1,N2
			D(J)=A(I,J)
			E(J)=B(I,J)
		ENDDO
		CALL C06ABF(D,E,N2,INVERS,M1,C)
		INFO=MOD(INFO+1,10)
		IF (.NOT.QUIET.AND.INFO.EQ.0) THEN
			WRITE (NUM,'(I6)') I
			CALL WRUSER(NUM//MESSAGE,ERROR)
		ENDIF
		DO J=1,N2
			A(I,J)=D(J)
			B(I,J)=E(J)
		ENDDO
	ENDDO
C
C	RETURN
C
	RETURN
	END
