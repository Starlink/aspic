C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     ********************
C                     *                  *
C                     * Program   ADISP3 *
C                     *                  *
C                     ********************
C
C
C
C          CALLING SEQUENCE:-
C               ADISP3  [ TRIM=FALSE LOG=TRUE ]
C
C
C          FUNCTION:-
C               This is a variation of the standard display  program  ADISP
C               which  handles  3-dimensional  Starlink  images by stepping
C               through a selected range of z-planes.
C
C
C          USE:-
C               It may be used to have a quick look at a 3-D data  set.  If
C               the  frames  are  small and the computer is not busy it may
C               even give a quasi-movie type of display.
C
C
C
C         USER PARAMETERS:-
C
C         IMAGE                               The  3-dimensional   Starlink
C                                             image to be displayed.
C
C         ZST             0                   The  first  z-plane   to   be
C                                             displayed.
C
C         ZEND            z-dim.-1            The  last   z-plane   to   be
C                                             displayed.
C
C         XC              256                 The x co-ordinate on the ARGS
C                                             where the CENTER of the image
C                                             will be displayed.
C
C         YC              256                 The y co-ordinate on the ARGS
C                                             where the CENTER of the image
C                                             will be displayed.
C
C         PVLO            Min. value          Data having this  value
C                                             are scaled to zero.
C
C         PVHI            Max. value          Data having  this  value  are
C                                             scaled to 255.
C
C
C         NORMALLY DEFAULTED PARAMETERS:-
C
C
C
C
C         TRIM            TRUE                If  TRUE  data  outside   the
C                                             range PVLO to PVHI are set to
C                                             those values. If FALSE values
C                                             outside that range are scaled
C                                             but allowed  to  wrap  around
C                                             the ARGS display.
C
C         LOG             FALSE               If TRUE  logarithmic  scaling
C                                             is  used between the selected
C                                             limits;   otherwise    linear
C                                             scaling is used.
C
C
C
C         C D Pike                 RGO                             6-JAN-82
C
C
C--------------------------------------------------------------------------



*
*  ADISP IMAGE [ZST] [ZEND] [XCENTRE] [YCENTRE] [FLOOR] [CEILING] [TRIM] [LOG]
*
*  ORIG PTW MODIF FOR DATABASE BY WFL JULY 1981
*  MOD BY CDP TO CHANGE ADISP > ADISP3 FOR 3-D
*  MODIF AUG 81 WFL - EXTRA TRIM AND LOG PARAMETERS
      INTEGER IDIMN(99)
      LOGICAL TRIM,LLOG
      INCLUDE 'INTERIM(FMTPAR)'
      CALL RDIMAG('IMAGE',FMT_R,99,IDIMN,NDIMS,IPIN,JSTAT)
      IF (NDIMS.NE.3) THEN
         CALL WRUSER('MUST BE 3D IMAGE!',J)
      ELSE
         NELS=1
         IXC=256
         IYC=256
         NZ1 = 0
         NZ2 = IDIMN(3)-1
         DO I=1,NDIMS
            NELS=NELS*IDIMN(I)
         END DO
         CALL GETDYN('IWK',FMT_SW,NELS,IWKP,J)
         CALL RDKEYI('ZST',.TRUE.,1,NZ1,NVALS,JSTAT)
         CALL RDKEYI('ZEND',.TRUE.,1,NZ2,NVALS,JSTAT)
         NZ1 = NZ1 + 1
         NZ2 = NZ2 + 1
         CALL RDKEYI('XC',.TRUE.,1,IXC,NVALS,JSTAT)
         CALL RDKEYI('YC',.TRUE.,1,IYC,NVALS,JSTAT)
         CALL RDKEYL('TRIM',.FALSE.,1,TRIM,NVALS,JSTAT)
         CALL RDKEYL('LOG ',.FALSE.,1,LLOG,NVALS,JSTAT)
         CALL ASP_SWSP(500,8)
         IF (TRIM.OR.LLOG) THEN
            CALL PVR(%VAL(IPIN),NELS,VLO,VHI)
            IF (TRIM) THEN
               CALL RDKEYR('PVLO',.TRUE.,1,VLO,NVALS,JSTAT)
               CALL RDKEYR('PVHI',.TRUE.,1,VHI,NVALS,JSTAT)
            ENDIF
         ENDIF
         CALL ADISP3(%VAL(IPIN),IDIMN(1),IDIMN(2),IDIMN(3),TRIM,
     &               LLOG,IXC,IYC,VLO,VHI,%VAL(IWKP),NZ1,NZ2)
         CALL ASP_RWSP
         CALL FRDATA(' ',JSTAT)
         CALL ARGS_WRIM (IXC,IYC,IDIMN(1),IDIMN(2),IDIMN(1),IDIMN(2),
     &      JSTAT)
         IF (JSTAT.NE.0) CALL WRUSER('COULDN''T UPDATE ARGS DATABASE',
     &      JSTAT)
      END IF
      END
      SUBROUTINE PVR(PIC,NELS,VMIN,VMAX)
      REAL PIC(NELS),VMIN,VMAX
      VMIN=PIC(1)
      VMAX=VMIN
      DO I=1,NELS
         V=PIC(I)
         VMIN=MIN(VMIN,V)
         VMAX=MAX(VMAX,V)
      END DO
      END
      SUBROUTINE ADISP3(PIC,NX,NY,NZ,TRIM,LLOG,IXC,IYC,VLO,VHI,IPIC,
     &                  NZ1,NZ2)
      REAL PIC(NX*NY*NZ),VLO,VHI,VL,D,SCALE,V
      INTEGER NX,NY,IXC,IYC
      INTEGER*2 IPIC(NX*NY),IDUMMY
      LOGICAL TRIM,LLOG
      CALL SRINIT(0,.FALSE.,JSTAT)
      IF(JSTAT.NE.0)  THEN
         CALL WRUSER(' ARGS NOT AVAILABLE',ISTAT)
         RETURN
      ENDIF
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
      DO 100 IFR=NZ1,NZ2
      IC = 0
      DO I=IFR*NX*NY-NX*NY+1,IFR*NX*NY
      IC = IC+1
         IF (LLOG) THEN
            IF (VL.LT.1E-10) THEN
               V=SCALE*LOG(1E10*(PIC(I)-VL)+1)
            ELSE
               V=SCALE*LOG(PIC(I)/VL)
            ENDIF
         ELSE IF (TRIM) THEN
            V=SCALE*(PIC(I)-VL)
         ELSE
            V=MOD(PIC(I),32768.0)
         ENDIF
         IPIC(IC)=NINT(MIN(MAX(V,0.0),255.0))
      END DO
         CALL SRPXI2(IPIC,NX,NX,NY,IXC-NX/2,IYC-NY/2,
     &               16,.FALSE.,IDUMMY,1)
  100 CONTINUE
      END
