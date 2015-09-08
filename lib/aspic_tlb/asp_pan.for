	SUBROUTINE ASP_PAN(AX,AY,UX,UY)

*+
*
*  - - - - - -
*  :  A P A N :
*  - - - - - -
*
*    D.J.KING   R.G.O.   DEC 81
*
*-
	INTEGER IDMAX,STATUS,ID,AX,AY
	INTEGER*2 ARGSOUT(3),ARGSIN(4),IOFF(3),ION(3),IFLIP(3)
	CHARACTER VALUE*80
	DATA IOFF,ION,IFLIP/3*0,'0038'X,'0008'X,'0003'X,3*0/
	REAL UX,UY

* CHECK DATABASE TO FIND IF ANYTHING IS DISPLAYED

	CALL ARGS_NUMIM(IDMAX)
	IF (IDMAX.EQ.O) THEN
		CALL WRERR('NOIMS')
	ELSE

* GET INFORMATION ON LAST IMAGE DISPLAYED FROM DATABASE

		CALL ARGS_RDPAR('DISPZOOM',1,VALUE,NVALS,STATUS)
		CALL ASP_DZTOI('ZXC',VALUE,IXC,STATUS)
		CALL ASP_DZTOI('ZYC',VALUE,IYC,STATUS)
		CALL ASP_DZTOI('ZXF',VALUE,IXF,STATUS)
		CALL ASP_DZTOI('ZYF',VALUE,IYF,STATUS)

		IF (IXF.EQ.0) THEN
			IXF=1
			IYF=1
			IXC=256
			IYC=256
		ENDIF

* SET UP INFORMATION TO SEND TO ARGS PROGRAM

		ARGSIN(3) = IXC
		ARGSIN(4) = IYC
		ARGSIN(1) = ((IXF-1)*256) + IYF-1
		IF (IXF.LT.1) THEN
			ARGSIN(2)=0
		ELSE IF (IXF.LT.3) THEN
			ARGSIN(2)=1
		ELSE IF (IXF.LT.7) THEN
			ARGSIN(2)=2
		ELSE IF (IXF.LT.15) THEN
			ARGSIN(2)=3
		ELSE
			ARGSIN(2)=4
		ENDIF


*  SET SYSTEM CURSOR COLOUR AND ALLOW OVERLAYS

		CALL ARGS_VSR(IOFF,ION,IFLIP)

*  SWITCH ON LAMPS

		CALL ARGS_LAMPS(1,1,1,1)

*  LOAD AND RUN ARGS PROGRAM

		CALL LOAD_PANZ
		CALL WRITE_PANZ(ARGSIN)
		CALL RUN_PANZ
		CALL READ_PANZ(ARGSOUT)
		AX = ARGSOUT(1)
		AY = ARGSOUT(2)
		IXF =(ARGSOUT(3).AND.'FF'X)+1
		IYF = IXF
		CALL ASP_ITODZ('ZXF',IXF,VALUE,STATUS)
		CALL ASP_ITODZ('ZYF',IYF,VALUE,STATUS)
		CALL ASP_ITODZ('ZXC',AX,VALUE,STATUS)
		CALL ASP_ITODZ('ZYC',AY,VALUE,STATUS)
		CALL ARGS_WRPAR('DISPZOOM',VALUE,1,STATUS)

*  SWITCH OFF LAMPS

		CALL ARGS_LAMPS(0,0,0,0)

*  DISABLE CURSOR

		CALL ARGS_CURS('0')
		CALL ARGS_VSRRST
		CALL SRSEND
*
		CALL ARGS_DOPDB ('ARGS_DEVICE',STATUS)
		CALL ARGS_ATOU (IDMAX,AX,AY,UX,UY,STATUS)
		CALL ARGS_CLDB (STATUS)

	ENDIF

	END
