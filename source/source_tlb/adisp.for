C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *******************
C                     *                 *
C                     * Program   ADISP *
C                     *                 *
C                     *******************
C
C
C
C          CALLING SEQUENCE:-
C               ADISP [ TRIM=FALSE LOG=TRUE ]
C
C
C          FUNCTION:-
C               It displays an image on the  ARGS  at  a  specified  center
C               after suitable scaling.
C
C
C          USE:-
C               This is the major routine used to look at  data.  Note  the
C               use  of  the  LOG  parameter  and remember that by choosing
C               suitable values for PVLO and PVHI  a  negative  display  is
C               generated.
C
C
C
C         USER PARAMETERS:-
C
C         IMAGE                               The 2-d Starlink image to  be
C                                             displayed.  It  may be of any
C                                             size,  but   no   more   than
C                                             512*512    pixels   will   be
C                                             visible.
C
C         XC              256                 The x co-ordinate on the ARGS
C                                             where the CENTER of the image
C                                             will be located.
C
C         YC              256                 The y co-ordinate on the ARGS
C                                             where the CENTER of the image
C                                             will be located.
C
C         PVLO            Min. value          Data  with  this  value   are
C                                             scaled to zero.
C
C         PVHI            Max. value          Data  with  this  value   are
C                                             scaled  to  255, which is the
C                                             largest value to be displayed
C                                             by the ARGS.
C
C
C         NORMALLY DEFAULTED PARAMETERS:-
C
C         TRIM            TRUE                If TRUE, values  outside  the
C                                             range PVLO to PVHI are set to
C                                             those values.  If FALSE  then
C                                             PVLO  and  PVHI  are  ignored
C                                             and the values reduced to  16
C                                             bits.  This may cause them to
C                                             wrap around on the ARGS.
C
C         LOG             FALSE               If  TRUE   then   logarithmic
C                                             scaling  is  used between the
C                                             defined   limits,   otherwise
C                                             linear scaling is used.
C
C
C
C         PTW/WFL/KFH              RGO                             6-JAN-82
C
C
C--------------------------------------------------------------------------



*
*  ADISP IMAGE [XCENTRE] [YCENTRE] [FLOOR] [CEILING] [TRIM] [LOG]
*
*  VERSION OF DISP PROGRAM BUT PERMITTING DISPLAY CENTRED
*  ON ANY ARGS COORDINATES XC, YC AND ALSO
*  UPDATING ARGS DATABASE FILE
*
*  ORIG PTW MODIF FOR DATABASE BY WFL JULY 1981
*
*
*  MODIF AUG 81 WFL - EXTRA TRIM AND LOG PARAMETERS
*
*   THE TRIM=T OPTION REQUESTS THAT DATA HIGHER OR LOWER THAN
*   THE PVHI & PVLO VALUES WILL BE SET TO THOSE VALUES, OTHERWISE
*   THEY WILL JUST WRAP AROUND THE LUT.
*   LOG=T WILL PERFORM A LOG TRANSFORMATION OF THE DATA BETWEEN
*   PVLO & PVHI AND SCALE THE OUTPUT TO 0-255 FOR DISPLAY PURPOSES.
*
*   EXPERIMENTAL VERSION USING 'DATA STRUCTURE'
*
      INTEGER IDIMN(99)
      LOGICAL TRIM,LLOG
      CHARACTER VALUE*80
      INCLUDE 'INTERIM(FMTPAR)'
      INCLUDE 'INTERIM(ERRPAR)'
      CALL SRINIT(0,.FALSE.,JSTAT)
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
      END
C
      SUBROUTINE ADISP2(PIC,NX,NY,TRIM,LLOG,IXC,IYC,VLO,VHI,IPIC)
      REAL PIC(NX*NY),VLO,VHI,VL,D,SCALE,V
      INTEGER NX,NY,IXC,IYC
      INTEGER*2 IPIC(NX*NY),IDUMMY
      LOGICAL TRIM,LLOG
      VL=VLO
      D=VHI-VLO
      IF (ABS(D).LT.1E-10) D=1E10
      IF (LLOG) THEN
         IF (VL.LT.1E-10) THEN
            SCALE=255.0/LOG(1E10*D+1)
         ELSE
            SCALE=255.0/LOG(VHI/VLO)
         ENDIF
      ELSE IF (TRIM) THEN
         SCALE=255.0/D
      ENDIF
      DO I=1,NX*NY
         IF (LLOG) THEN
            IF (VL.LT.1E-10) THEN
               V=SCALE*LOG(1E10*(PIC(I)-VL)+1)
            ELSE
		IF (PIC(I).EQ.0) THEN
			V=0.0
		ELSE
              		 V=SCALE*LOG(PIC(I)/VL)
		ENDIF
            ENDIF
         ELSE IF (TRIM) THEN
            V=SCALE*(PIC(I)-VL)
         ELSE
            V=MOD(PIC(I),32768.0)
         ENDIF
         IPIC(I)=NINT(MIN(MAX(V,0.0),255.0))
      END DO
      CALL SRPXI2(IPIC,NX,NX,NY,IXC-NX/2,IYC-NY/2,
     &               16,.FALSE.,IDUMMY,1)
      END
