C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C          *********************
C          *                   *
C          * SUBROUTINE FSTPRF *
C          *                   *
C          *********************
C
C
C
C      PURPOSE
C
C      The data in an array are fitted by a model of a sloping
C      background and up to 8 2-D Lorentzian profiled stars.
C
C      The NSFIT stars all have the same fixed input profile and
C      have their heights and positions calculated.
C
C
C      The Lorentzian profile is :-
C
C
C                  I  =      1
C                       --------------
C                               P(1+d1)
C                          1 + d1
C
C
C                                   where d1 = sqrt(x1*x1 + y1*y1)
C
C                                     and d2 = sqrt(x2*x2 + y2*y2)
C
C
C
C                                     with x1 = (X-X0)/RX
C                                          y1 = (Y-Y0)/RY
C
C                                      and x2 = (X-X0)/PRX
C                                          y2 = (Y-Y0)/PRY
C
C
C      The background is fitted as I = A.X + B.Y + C
C
C      The Y values are input as from bottom to top and output
C      the same.
C
C      The input profile and star heights and positions and the
C      background must be close to the real values for the fit to
C      work.
C
C      This is a version of the s/r PROFIT.
C      The profile is kept fixed and the calculations have been kept
C      to a minmium to speed things up.
C      The main difference from PROFIT is that instead of the whole
C      rectangle containing the stars being analysed, only the areas near
C      each star is done. The area is +/- 3.Gaussian radii from the
C      peak.
C      Also it deals with INVALID pixels, by not using any
C      pixels that are more negative than some preset value.
C      Also for each pixel, if one of the stars is so far away that
C      that star makes no contribution to the pixel, the pixel is
C      not used in the part of the iteration concerned with that
C      star.
C
C
C
C   ARGUMENTS
C    IN
C      DATA(NX,NY)    Real        The input array to fit
C      KDO(NX,NY)     Integer*2   Work space inside s/r
C      NX             Integer     The X length of DATA
C      NY             Integer     The Y length of DATA
C      JJFIT(32)      Integer     Flag for CCC parameter fit
C                                 -1=Fixed  0=Variable  1=Not used
C      NSFIT          Integer     No of stars to fit
C      ITSLIM         Integer     Max no of iterations allowed
C      GAURX          Real       'X Gaussian radius' of profile
C      GAURY          Real       'Y Gaussian radius' of profile
C      ALOW           Real        Limit below which pixels are ignored
C   INPUT/OUTPUT
C      CCC(32)        Real        The input profile and output fit
C                                 1=C  2=A  3=B             for background
C                                 4=RX 5=RY 6=P 7=PRX 8=PRY for profile
C                                 9=XO 10=YO 11=Height1     for 1st star
C                                 12=X2 13=Y2 14=Height2    for 2nd star
C                                      and so on for the other stars
C   OUT
C      ITER           Integer     No of iterations done
C
C
C   CALLS
C     Grasp
C       SIMULX
C
C   USES
C     I*2 array
C
C
C
C    A J PENNY                  RGO                            82-OCT-31
C --------------------------------------------------------------------
C
C
C
C
C
C
C
C      DATA AREAS ..... DATA IS DATA ARRAY
C                       CMAT IS THE NORMAL EQUATION MATRIX
C                       CVEC                        VECTOR
C                       RVEC                        SOLUTION
C                       CC   IS THE PLANE AND FORM COEFFICIENTS
C
C      DF is the damping factor
C      TINY IS "TOO SMALL" , RLIMIT IS SMALLEST RESIDUAL
C
C      MSTARS IS MAX NO OF STARS,NSTARS IS ACTUAL NO,NSTAR IS CURRENT STAR
C
C      ERF CONTAINS THE ERROR FUNCTIONS
C
C      JFIT IS THE FIT TABLE ; 1 TO 8 FOR FORM PARAMETERS
C                              9 TO 24 FOR STAR PARAMETERS
C      -1 MEANS FIXED
C      +1 MEANS FREE TO VARY
C       0 MEANS NOT USED (FOR STAR PARAMETERS)
C      NPF IS THE CURRENT NUMBER OF PARAMETERS TO BE FITTED
C
C      A,B,C ARE PLANE COEFFICIENTS,THE REST ARE CONSTANTS
C      FOR ALL PROFILES
C
C -----------------------------------------------------------------
C
C
      SUBROUTINE FSTPRF(DATA,KDO,NX,NY,CCC,JJFIT,NSFIT,ITER,ITSLIM,
     +                  GAURX,GAURY,ALOW)
C
C
C
      REAL DATA(NX,NY)
      REAL CMAT(1024),CVEC(32),RVEC(32),CCC(32),CC(32),CCS(32)
      REAL CCLAST(32)
      INTEGER*2 KDO(NX,NY)
      EQUIVALENCE (CC(1),A),(CC(2),B),(CC(3),C)
     1            ,(CC(4),GX),(CC(5),GY),(CC(6),HX),(CC(7),HY),(CC(8),P)
      INTEGER JFIT(32),NPF,JJFIT(32)
      EQUIVALENCE (JFIT(1),JFIT1),(JFIT(2),JFIT2),(JFIT(3),JFIT3)
     1           ,(JFIT(4),JFIT4),(JFIT(5),JFIT5),(JFIT(6),JFIT6)
     2            ,(JFIT(7),JFIT7),(JFIT(8),JFIT8)
      REAL ERF(32)
      REAL VALX(200),VALY(200),DXLIM(8),DYLIM(8)
      LOGICAL*1 AGAIN
      CHARACTER*80 TEXT
      DATA DF/0.5/
      DATA TINY/1E-10/
      DATA RLIMIT/1E-30/
C
C
C ----------------------------------------------------------
C
C  Translate CCC and JJFIT from outside
C
C   COPY CCC INTO CC AND JJFIT INTO JFIT
C
      DO I=1,32
         CC(I)=CCC(I)
         JFIT(I)=JJFIT(I)
      END DO
C
C  Rearrange profile parameters to match s/r order
C
      TEMP = CC(8)
      CC(8) = CC(6)
      CC(6) = CC(7)
      CC(7) = TEMP
      K = JFIT(8)
      JFIT(8) = JFIT(6)
      JFIT(6) = JFIT(7)
      JFIT(7) = K
C
C  Invert the profile radii to match s/r convention
C
      CC(4) = 1.0/CC(4)
      CC(5) = 1.0/CC(5)
      CC(6) = 1.0/CC(6)
      CC(7) = 1.0/CC(7)
C
C --------------------------------------------------------
C
C  Store the input paramters
C
      DO I = 1,32
         CCS(I) = CC(I)
      ENDDO
C
C   Store the initial parameters
C
      DO I=1,32
         CCLAST(I) = CC(I)
      ENDDO
C
C
C  Max no of stars: Actual no of stars: No of the current star
C
      MSTARS=8
      NSTARS=NSFIT
      NSTAR=0
C
C      NPSIZE IS TOTAL NUMBER OF PIXELS
C      NPFA IS NUMBER OF POSSIBLE FREE PARAMETERS
C
C
      NPSIZE = NX*NY
      NPFA = 3*MSTARS+8
C
C  Set up loop counter
C
      NLOOP = 0
C
C  Squares of the 4 radii
C
      GX2=GX*GX
      GY2=GY*GY
      HX2=HX*HX
      HY2 = HY*HY
C
C  Count the number of free parameters
C
      NPF = 0
      DO K = 1,32
         IF (JFIT(K).EQ.1) NPF = NPF + 1
      ENDDO
C
C  Calc profile along X and along Y to set up for seeing how far
C  from each star one has to go before it is less than 0.01 and
C  it can be ignored
C
      KCHX = 0
      KCHY = 0
      DO K = 1,200
         DD = REAL(K)
         IF (KCHX.EQ.0) THEN
            VALX(K) = 1.0/(1.0+(DD*GX)**(P*(1.0+(DD*HX))))
            IF (VALX(K).LE.1.0E-10) KCHX = 1
         ELSE
            VALX(K) = 1.0E-10
         ENDIF
         IF (KCHY.EQ.0) THEN
            VALY(K) = 1.0/(1.0+(DD*GY)**(P*(1.0+(DD*HY))))
            IF (VALY(K).LE.1.0E-10) KCHY = 1
         ELSE
            VALY(K) = 1.0E-10
         ENDIF
      ENDDO
C
C
C
C
C -----------------------------------------------------
C        END OF INITIALIZATION AND DATA DEFINITION
C        START OF ITERATION LOOP
C
  401 CONTINUE
C
C  Zeroise the normal equation arrays
C
      DO K = 1,1024
         CMAT(K)=0.0
      ENDDO
      DO K = 1,32
         CVEC(K) = 0.0
      ENDDO
C
C    Set up flags for pixels not to bother with
C
      DO J = 1,NY
         DO I = 1,NX
            KDO(I,J) = 0
         ENDDO
      ENDDO
C
      DRX = 3.0*GAURX
      DRY = 3.0*GAURY
      DO I = 1,NSTARS
         II = 3*I + 6
         IF (JFIT(II+2).EQ.1) THEN
            KXS = CC(II) - DRX
            KXE = CC(II) + DRX
            KYS = CC(II+1) - DRY
            KYE = CC(II+1) + DRY
            IF (KXS.LT.1) KXS = 1
            IF (KXE.GT.NX) KXE = NX
            IF (KYS.LT.1) KYS = 1
            IF (KYE.GT.NY) KYE = NY
            DO JL = KYS,KYE
               DO IL = KXS,KXE
                  KDO(IL,JL) = 1
               ENDDO
            ENDDO
         ENDIF
      ENDDO
C
      DO J = 1,NY
         DO I = 1,NX
            IF (DATA(I,J).LT.ALOW) KDO(I,J) = 0
         ENDDO
      ENDDO
C
C  Calc the distance from each star to calculate
C
      DO K = 1,NSTARS
         IF (JFIT(8+3*K).NE.0) THEN
            AH = CC(8+3*K)
            KK = DRX - 1.0
            IF (KK.LT.0) KK = 0
            IF (KK.GT.74) KK = 74
            AV = 1.0
            DO WHILE (AV.GT.0.01.AND.KK.LT.200)
               KK = KK + 1
               AV = AH*VALX(KK)
            ENDDO
            DXLIM(K) = REAL(KK)
            KK = DRY - 1.0
            IF (KK.LT.0) KK = 0
            IF (KK.GT.74) KK = 74
            AV = 1.0
            DO WHILE (AV.GT.0.01.AND.KK.LT.200)
               KK = KK + 1
               AV = AH*VALY(KK)
            ENDDO
            DYLIM(K) = REAL(KK)
         ENDIF
      ENDDO
C
C   Loop through the pixels
C
      DO NPIXEL = 1,NPSIZE
C
C
C
         JY = 1 + (NPIXEL-1)/NX
         JX = NPIXEL-NX*(JY-1)
         IF (KDO(JX,JY).EQ.1) THEN
C
C   If pixel worth doing, do it
C
            X = FLOAT(JX)
            Y = FLOAT(JY)
C
C
C
            ERF(1) = 1.0
            ERF(2) = X
            ERF(3) = Y
            DO NACO = 4,8
               ERF(NACO) = 0.0
            ENDDO
            Z = A + B*X + C*Y
C
C  Accumulate profile and error functions over all stars
C
            NAC = 3
            NAC2 = 0
            DO NSTAR = 1,MSTARS
               NAC0 = NAC
C
C  Finished stars
C
               N = 3*NSTAR+6
               JFITX = JFIT(N)
C
C  If there is a star
C
               IF (JFITX.NE.0) THEN
                  JFITY = JFIT(N+1)
                  JFITXY = MAX0(JFITX,JFITY)
C
C  Star dependant parameters
C
                  DX = X-CC(N)
                  IF (DX.EQ.0) DX = 0.0001
                  DY = Y-CC(N+1)
                  IF (DY.EQ.0.0) DY = 0.0001
C
C  See if calc effect on the pixel of this star
C
                  ABX = ABS(DX)
                  ABY = ABS(DY)
                  DXL = DXLIM(NSTAR)
                  DYL = DYLIM(NSTAR)
                  IF (ABX.LT.DXL.AND.ABY.LT.DYL) THEN
C
C  Calc the fit analysis parameters at this point
C
                     A0 = CC(N+2)
                     DX2 = DX*DX
                     DY2 = DY*DY
                     GG = DX2*GX2 + DY2*GY2
                     HH = DX2*HX2 + DY2*HY2
                     H = SQRT(HH)
                     HP1 = H+1.0
                     ALP = P*HP1
                     AON2 = ALP/2.0
                     ALG2 = ALOG(GG)
                     ALG = ALG2/2.0
                     ALG2A = ALG2*(AON2-1.0)
                     IF (ALG2A.LT.60.AND.ALG2A.GT.-60) THEN
                       G2AM2 = EXP(ALG2A)
                     ELSE
                       G2AM2 = 0.0
                     ENDIF
                     G2A = G2AM2*GG
                     IF (ALG2A.LT.60) THEN
                       F4 = 1.0/(1.0+G2A)
                       IF (F4.LT.TINY) F4 = 0.0
                     ELSE
                       F4 = 0.0
                     ENDIF
                     F4S = F4*F4
                     F9 = A0*F4S*G2AM2
                     Z = Z+A0*F4
C
C  Error functions for position and intensity for each star
C
                     IF (NAC2.EQ.0) NAC2 = NAC0
                     IF (JFITXY.EQ.1) THEN
                       GGALG = GG*ALG
                       GGPH = GGALG*P/H
                       IF (JFITX.EQ.1) THEN
                          NAC2 = NAC2 + 1
                          ERF(NAC2)=F9*DX*(ALP*GX2+GGPH*HX2)
                       ENDIF
                       IF (JFITY.EQ.1) THEN
                          NAC2 = NAC2 + 1
                          ERF(NAC2)=F9*DY*(ALP*GY2+GGPH*HY2)
                       ENDIF
                     ENDIF
                     NAC2 = NAC2 + 1
                     ERF(NAC2)=F4
C
                  ENDIF
C
               ENDIF
            ENDDO
C
C   Calcs residual
C
            RES = DATA(JX,JY) - Z
            IF (ABS(RES).LT.RLIMIT) RES = 0.0
C
C  Accumulate vector and matrix contributions at this pixel
C
            DO I=1,NPF
               ERFI = ERF(I)
               CVEC(I)=CVEC(I)+ERFI*RES
               IJ0 = (I-1)*NPF
               DO J=I,NPF
                 IJ = IJ0+J
                 CMAT(IJ)=CMAT(IJ)+ERFI*ERF(J)
               ENDDO
            ENDDO
         ENDIF
      ENDDO
C
C
C
      DO I=2,NPF
         IM1=I-1
         IMINAC = IM1*NPF
         DO J=1,IM1
            IJ = IMINAC + J
            JI = (J-1)*NPF+I
            CMAT(IJ)=CMAT(JI)
         ENDDO
      ENDDO
C
C      APPLY DAMPING FACTOR
C
      FACTOR = 1.0 + DF*DF
      DO I=1,NPF
         II = (I-1)*NPF+I
         CMAT(II)=CMAT(II)*FACTOR
      ENDDO
C
C  Solve the normal equations
C
      CALL SIMULX(CVEC,CMAT,RVEC,NPF)
C
C  Improve the old solution
C
      NAC = 0
      DO NC = 1,NPFA
         IF (JFIT(NC).EQ.1) THEN
            NAC = NAC + 1
            CC(NC)=CC(NC)+RVEC(NAC)
         ENDIF
      ENDDO
C
C  Done a loop
C
      NLOOP = NLOOP + 1
C
C  Compare new heights with the old ones.
C
      AGAIN=.FALSE.
      DO IL = 1,NSTARS
         I = 11 + 3*(IL-1)
         IF (JFIT(I).EQ.1) THEN
            DIFF=ABS(CC(I)-CCLAST(I))
            IF (CCLAST(I).LT.1.0E-10) CCLAST(I) = 1.0E-10
            PDIFF = DIFF/CCLAST(I)
            IF (PDIFF.GT.0.002) AGAIN = .TRUE.
            CCLAST(I)=CC(I)
         ENDIF
      ENDDO
C
C  If star has drifted too far or has small height, remove it from
C  the fitting operation.
C
      DO LST=1,NSTARS
         K = 9 + (LST-1)*3
         IF (ABS(CC(K)-CCS(K)).GT.GAURX.OR.
     +       ABS(CC(K+1)-CCS(K+1)).GT.GAURY.OR.
     +       CC(K+2).LT.0.000001) THEN
           IF (JFIT(K).EQ.1) NPF = NPF - 1
            IF (JFIT(K+1).EQ.1) NPF = NPF - 1
            IF (JFIT(K+2).EQ.1) NPF = NPF - 1
            JFIT(K) = 0
            JFIT(K+1) = 0
            CC(K) = CCS(K)
            CC(K+1) = CCS(K+1)
            JFIT(K+2) = 0
            CC(K+2) = 0.0
         ENDIF
      ENDDO
C
C  Loop again if star heights changing and done less than limit
C  or only done one loop and more allowed
C
      IF ((NLOOP.EQ.1).AND.(ITSLIM.GT.1)) GOTO 401
      IF ((AGAIN).AND.(NLOOP.NE.ITSLIM)) GOTO 401
C
C   Now store the number of iterations done.
C
      ITER = NLOOP
C
C ---------------------------------------------------------
C
C  Translate back to outside CCC and JFIT
C
      DO I=1,32
         CCC(I)=CC(I)
         JJFIT(I)=JFIT(I)
      END DO
C
C  Restore outside order and radii convention
C
      TEMP = CCC(8)
      CCC(8) = CCC(7)
      CCC(7) = CCC(6)
      CCC(6) = TEMP
      K = JJFIT(8)
      JJFIT(8) = JJFIT(7)
      JJFIT(7) = JJFIT(6)
      JJFIT(6) = K
      CCC(4) = 1.0/CCC(4)
      CCC(5) = 1.0/CCC(5)
      CCC(7) = 1.0/CCC(7)
      CCC(8) = 1.0/CCC(8)
C
C -------------------------------------------------------
C
C
C
      END
C
C
C



