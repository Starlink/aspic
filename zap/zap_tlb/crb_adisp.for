	SUBROUTINE CRB_ADISP
      INTEGER IDIMN(99)
      LOGICAL TRIM,LLOG
      CHARACTER VALUE*80
      CHARACTER*1 CRBANS
	CHARACTER*6 ANS
	INTEGER ISTAT
      INCLUDE 'INTERIM(FMTPAR)'
      INCLUDE 'INTERIM(ERRPAR)'
202   CALL SRINIT(0,.FALSE.,JSTAT)
      IF (JSTAT.NE.0) THEN
         CALL WRUSER('DISPLAY UNAVAILABLE',JSTAT)
      ELSE
         CALL RDIMAG('IMAGE',FMT_R,99,IDIMN,NDIMS,IPIN,JSTAT)
         IF (NDIMS.NE.2.OR.JSTAT.NE.ERR_NORMAL) THEN
            CALL WRUSER('Image not found or not 2D',J)
         ELSE
            NELS=1
            IXC=256
            IYC=256
            DO I=1,NDIMS
               NELS=NELS*IDIMN(I)
            END DO
            CALL GETDYN('IWK',FMT_SW,NELS,IWKP,J)
            CALL RDKEYI('XC',.TRUE.,1,IXC,NVALS,JSTAT)
            CALL RDKEYI('YC',.TRUE.,1,IYC,NVALS,JSTAT)
            CALL RDKEYL('TRIM',.FALSE.,1,TRIM,NVALS,JSTAT)
            CALL RDKEYL('LOG ',.FALSE.,1,LLOG,NVALS,JSTAT)
            CALL ASP_SWSP(500,8)
            IF (TRIM.OR.LLOG) THEN
               CALL ASP_STAT('IMAGE',%VAL(IPIN),NELS,VTOT,VMEAN,VLO,
     &            VHI,VSIG,JSTAT)
               IF (TRIM) THEN
                  CALL RDKEYR('PVLO',.TRUE.,1,VLO,NVALS,JSTAT)
                  CALL RDKEYR('PVHI',.TRUE.,1,VHI,NVALS,JSTAT)
               ENDIF
            ENDIF
            CALL ADISP2(%VAL(IPIN),IDIMN(1),IDIMN(2),TRIM,LLOG,
     &                  IXC,IYC,VLO,VHI,%VAL(IWKP))
            CALL ASP_RWSP
            CALL FRDATA(' ',JSTAT)
	    CALL ARGS_NUMIM(IDMAX)
	    IF (IDMAX.GE.1) THEN
		CALL ARGS_RDPAR('DISPZOOM',1,VALUE,NVALS,JSTAT)
		CALL ASP_DZTOI('ZXC',VALUE,IZXC,JSTAT)
		CALL ASP_DZTOI('ZYC',VALUE,IZYC,JSTAT)
		CALL ASP_DZTOI('ZXF',VALUE,IXF,JSTAT)
		CALL ASP_DZTOI('ZYF',VALUE,IYF,JSTAT)
		CALL ARGS_WRPAR('DISPZOOM',VALUE,1,JSTAT)
	    ELSE
		IZXC=256
		IZYC=256
		IXF=1
		IYF=1
	    ENDIF
            CALL ARGS_WRIM (IXC,IYC,IDIMN(1),IDIMN(2),IDIMN(1),IDIMN(2),
     &         JSTAT)
            IF(JSTAT.NE.0)THEN
               CALL WRUSER('COULDN''T UPDATE ARGS DATABASE',JSTAT)
            ELSE
               CALL ARGS_RDPAR ('DISPZOOM',1,VALUE,NVALS,JSTAT)
               CALL ASP_LTODZ ('TRIM',TRIM,VALUE,JSTAT)
               CALL ASP_LTODZ ('LOG',LLOG,VALUE,JSTAT)
               CALL ASP_FTODZ ('PVLO',VLO,VALUE,JSTAT)
               CALL ASP_FTODZ ('PVHI',VHI,VALUE,JSTAT)
	       CALL ASP_ITODZ ('ZXC',IZXC,VALUE,JSTAT)
	       CALL ASP_ITODZ ('ZYC',IZYC,VALUE,JSTAT)
	       CALL ASP_ITODZ ('ZXF',IXF,VALUE,JSTAT)
	       CALL ASP_ITODZ ('ZYF',IYF,VALUE,JSTAT)
               CALL ARGS_WRPAR ('DISPZOOM',VALUE,1,JSTAT)
            ENDIF
         END IF
      ENDIF
	
	CALL CNPAR('XC',JSTAT)
	CALL CNPAR('YC',JSTAT)
	CALL CNPAR('PVLO',JSTAT)
	CALL CNPAR('PVHI',JSTAT)
	END
