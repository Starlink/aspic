C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C          *********************
C          *                   *
C          * SUBROUTINE PROFIT *
C          *                   *
C          *********************
C
C
C
C
C      PURPOSE
C
C      The data in an array are fitted by a model of a sloping
C      background and up to 8 2-D Lorentzian profiled stars.
C
C      The variables are the star profile (RX,RY,P,PRX,PRY); the
C      sloping background, and the star positions and heights.
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
C
C
C   ARGUMENTS
C    IN
C      DATA(NX,NY)    Real        The input array to fit
C      NX             Integer     The X length of DATA
C      NY             Integer     The Y length of DATA
C      JJFIT(32)      Integer     Flag for CCC parameter fit
C                                 -1=Fixed  0=Variable  1=Not used
C      NSFIT          Integer     No of stars to fit
C      ITSLIM         Integer     Max no of iterations allowed
C      KFLOUT         Integer     Flag for typing some result (no=0)
C   INPUT/OUTPUT
C      CCC(32)        Real        The input profile and output fit
C                                 1=C  2=A  3=B             for background
C                                 4=RX 5=RY 6=P 7=PRX 8=PRY for profile
C                                 9=XO 10=YO 11=Height1     for 1st star
C                                 12=X2 13=Y2 14=Height2    for 2nd star
C                                      and so on for the other stars
C   OUT
C      ITER           Integer     No of iterations done
C      FIT(NX,NY)     Real        The fit to the DATA
C      RESID(NX,NY)   Real        The residuals DATA - FIT
C      PERMS          Real        RMS percentage error over array
C
C
C   CALLS
C     Grasp
C       SIMULX
C
C
C
C
C
C -----------------------------------------------------------------
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
C
C      For historical reasons the calculations inside the s/r are
C      done with the Y posns running from top to bottom, so the
C      input Y posns are swopped round inside, but only inside,
C      the s/r. And inside the s/r the profile parameters
C      for the 4 radii are used as reciprocals and the order of the
C      profile parameters in the array CC is changed.
C
C
C
C    A J PENNY                  RGO                            82-OCT-31
C --------------------------------------------------------------
C
C
C
      SUBROUTINE PROFIT(DATA,FIT,RESID,NX,NY,CCC,JJFIT,NSFIT,
     :                   ITER,PERMS,ITSLIM,KFLOUT)
C
C
C
      REAL DATA(NX,NY),FIT(NX,NY),RESID(NX,NY)
      REAL CMAT(1024),CVEC(32),RVEC(32),CCC(32),CC(32),CCS(32)
      REAL HTA(8),CCLAST(32),POUT(5)
C
C      A,B,C ARE PLANE COEFFICIENTS,THE REST ARE CONSTANTS
C      FOR ALL PROFILES
C
      EQUIVALENCE (CC(1),A),(CC(2),B),(CC(3),C)
     1            ,(CC(4),GX),(CC(5),GY),(CC(6),HX),(CC(7),HY),(CC(8),P)
C
C      JFIT IS THE FIT TABLE ; 1 TO 8 FOR FORM PARAMETERS
C                              9 TO 24 FOR STAR PARAMETERS
C      -1 MEANS FIXED
C      +1 MEANS FREE TO VARY
C       0 MEANS NOT USED (FOR STAR PARAMETERS)
C      NPF IS THE CURRENT NUMBER OF PARAMETERS TO BE FITTED
C
      INTEGER JFIT(32),NPF,JJFIT(32)
      EQUIVALENCE (JFIT(1),JFIT1),(JFIT(2),JFIT2),(JFIT(3),JFIT3)
     1           ,(JFIT(4),JFIT4),(JFIT(5),JFIT5),(JFIT(6),JFIT6)
     2            ,(JFIT(7),JFIT7),(JFIT(8),JFIT8)
C
C      ERF CONTAINS THE ERROR FUNCTIONS
C      SUM CONTAINS THE RESIDUAL SUMMING ARRAY
C
      REAL ERF(32),SUM(60)
C
C      MSTARS IS MAX NO OF STARS,NSTARS IS ACTUAL NO,NSTAR IS CURRENT STAR
C
      INTEGER MSTARS,NSTARS,NSTAR
C
C     Set up an array to hold the heights from the previous
C     iteration and a logical to show if another iteration is needed.
C
      LOGICAL*1 AGAIN
      CHARACTER*80 TEXT
C
C      DF is the damping factor
C      TINY IS "TOO SMALL" , RLIMIT IS SMALLEST RESIDUAL
C
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
C  Set the Y values to s/r values from outside values
C
      DO I = 1,NSFIT
         K = 10 + (I-1)*3
         CC(K) = REAL(NY) - CC(K) + 1.0
      ENDDO
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
C  Perms is the percentage error
C
      PERMS = 100.0
C
C      IWH,IWW ARE PICTURE HEIGHT AND WIDTH
C
      IWH=NY
      IWW=NX
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
      NPSIZE=IWH*IWW
      NPFA = 3*MSTARS+8
C
C   ICO DEFINES THE MAX NO OF NUMBER OF ITERATIONS
C
      ICO = ITSLIM
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
C
C
C
C -----------------------------------------------------
C        END OF INITIALIZATION AND DATA DEFINITION
C        START OF ITERATION LOOP
C
  401 CONTINUE
C
C      SET LOGIC FLAGS
C
      JFIT45=MAX0(JFIT4,JFIT5)
      JFIT67=MAX0(JFIT6,JFIT7)
      JF678=MAX0(JFIT67,JFIT8)
C
C       ZEROIZES THE NORMAL EQUATION ARRAYS
C
      DO 1501 NXA = 1,1024
      IF (NXA.LE.60) SUM(NXA)=0.0
      CMAT(NXA)=0.0
      IF (NXA.GT.32) GO TO 1501
      CVEC(NXA)=0.0
 1501 CONTINUE
C
C      INITIALIZE SUM POINTER
C
      KMAX = 0
      SIZE = 0.0
      DO 1530 NPIXEL = 1,NPSIZE
 1502 CONTINUE
C
C      COMPUTE AND SUBTRACT STAR PROFILE
C      LOOPING THROUGH ALL PIXELS
C
C
C      DETERMINE POSITION IN ARRAY AND THEN THE X,Y
C      CO-ORDINATES RELATIVE TO THE BOTTOM LEFTHAND CORNER
C
      JROW = (NPIXEL-1)/IWW
      JCOL = NPIXEL-IWW*JROW-1
      X = FLOAT(JCOL)
      Y = FLOAT(IWH-JROW-1)
      NAC = 0
      IF (JFIT1.NE.1) GO TO 1504
C
C      A FIXED
C
      NAC = NAC + 1
      ERF(NAC)=1.0
 1504 CONTINUE
      IF (JFIT2.NE.1) GO TO 1505
C
C      B FIXED
C
      NAC = NAC + 1
      ERF(NAC)=X
 1505 CONTINUE
      IF (JFIT3.NE.1) GO TO 1506
C
C      C FIXED
C
      NAC = NAC + 1
      ERF(NAC)=Y
 1506 CONTINUE
C
C      THE NEXT FUNCTIONS (RELATING TO PROFILE FORM) HAVE TO
C      BE ACCUMULATED FOR EACH STAR
C
C      CLEAR THESE FUNCTIONS
C
      NAC0 = NAC
 1507 CONTINUE
      NAC0 = NAC0 + 1
      IF (NAC0.GT.8) GO TO 1508
      ERF(NAC0)=0.0
      GO TO 1507
C
C      ACCUMULATE PROFILE AND ERROR FUNCTIONS OVER ALL STARS
C
 1508 CONTINUE
      Z = A + B*X + C*Y
      NSTAR = 1
      NAC2 = 0
 1509 CONTINUE
      NAC0 = NAC
 1510 CONTINUE
      IF (NSTAR.GT.MSTARS) GO TO 1520
C
C      FINISHED STARS
C
      N = 3*NSTAR+6
      JFITX = JFIT(N)
      IF (JFITX.EQ.0) GO TO 1519
C
C      NO STAR
C
      JFITY = JFIT(N+1)
      JFITI = JFIT(N+2)
      JFITXY = MAX0(JFITX,JFITY)
C
C      STAR DEPENDENT PARAMETERS
C
      DX = X-CC(N)
      DY = Y-CC(N+1)
      A0 = CC(N+2)
      DX2 = DX*DX
      DY2 = DY*DY
      IF (DX.EQ.0.0.AND.DY.EQ.0.0) GO TO 1522
C
C      IGNORE THIS ONE
C
      GG = DX2*GX2 + DY2*GY2
      HH = DX2*HX2 + DY2*HY2
      H = SQRT(HH)
      HP1 = H+1.0
      ALPHA = P*HP1
      AON2 = ALPHA/2.0
      G2AM2 = 0.0
      G2A = 0.0
      F4 = 0.0
      ALG2 = ALOG(GG)
      ALG = ALG2/2.0
      ALGAM2 = ALG2*(AON2-1.0)
      IF (ALGAM2.LT.60.AND.ALGAM2.GT.-60)
     1                             G2AM2 = EXP(ALGAM2)
      G2A = G2AM2*GG
      IF (ALGAM2.LT.60) F4=1.0/(1.0+G2A)
      IF (F4.LT.TINY) F4 = 0.0
      F4S = F4*F4
      F9 = A0*F4S*G2AM2
      IF (JFIT45.NE.1) GO TO 1512
C
C      GX AND GY FIXED
C
      F6 = ALPHA*F9
      IF (JFIT4.NE.1) GO TO 1511
C
C      GX FIXED
C
      NAC0 = NAC0 + 1
      ERF(NAC0)=ERF(NAC0)-F6*DX2*GX
 1511 CONTINUE
      IF (JFIT5.NE.1) GO TO 1512
C
C      GY FIXED
C
      NAC0 = NAC0 + 1
      ERF(NAC0) = ERF(NAC0)-F6*DY2*GY
 1512 CONTINUE
      IF (JF678.NE.1.AND.JFITXY.NE.1) GO TO 1515
C
C      FORM AND STAR FIXED
C
      GGALG = GG*ALG
      F7 = F9*GGALG
      F8 = F7
      IF (JFIT67.NE.1) GO TO 1514
C
C      HX AND HY FIXED
C
      F7 = F7*P/H
      IF (JFIT6.NE.1) GO TO 1513
C
C      HX FIXED
C
      NAC0 = NAC0 + 1
      ERF(NAC0)=ERF(NAC0)-F7*DX2*HX
 1513 CONTINUE
      IF (JFIT7.NE.1) GO TO 1514
C
C      HY FIXED
C
      NAC0 = NAC0 + 1
      ERF(NAC0)=ERF(NAC0)-F7*DY2*HY
 1514 CONTINUE
      IF (JFIT8.NE.1) GO TO 1515
C
C      P FIXED
C
      NAC0 = NAC0 + 1
      ERF(NAC0)=ERF(NAC0)-F8*HP1
 1515 CONTINUE
C
C      ERROR FUNCTIONS FOR POSITION AND INTENSITY FOR EACH STAR
C
      IF (NAC2.EQ.0) NAC2 = NAC0
      IF (JFITXY.NE.1) GO TO 1517
C
C      STAR POSITION FIXED
C
      GGAGPH = GGALG*P/H
      IF (JFIT(N).NE.1) GO TO 1516
C
C      X0(NSTAR) FIXED
C
      NAC2 = NAC2 + 1
      ERF(NAC2)=F9*DX*(ALPHA*GX2+GGAGPH*HX2)
 1516 CONTINUE
      IF (JFIT(N+1).NE.1) GO TO 1517
C
C      Y0(NSTAR) FIXED
C
      NAC2 = NAC2 + 1
      ERF(NAC2)=F9*DY*(ALPHA*GY2+GGAGPH*HY2)
 1517 CONTINUE
      IF (JFITI.NE.1)  GO TO 1518
C
C      STAR INTENSITY FIXED
C
      NAC2 = NAC2 + 1
      ERF(NAC2)=F4
 1518 CONTINUE
      Z = Z+A0*F4
 1519 CONTINUE
      NSTAR = NSTAR + 1
      GO TO 1509
C
C      LOOP THROUGH ALL STARS
C
 1520 CONTINUE
      K = 1
      N = NPIXEL
C
C      NOW STORES THE MODEL VALUE OF THE FIT AND THE RESIDUAL
C
      ZOLD = DATA(JCOL+1,JROW+1)
      IF (ABS(ZOLD).LT.RLIMIT) ZOLD =0.0
      RES = ZOLD-Z
      IF (ABS(RES).LT.RLIMIT) RES = 0.0
      RESID(JCOL+1,JROW+1)=RES
      FIT(JCOL+1,JROW+1)=Z
      EE = RES*RES
      ZZ = ZOLD*ZOLD
      SIZE = SIZE+1.0
C
C      ACCUMULATE SUMS OF ERROR SQUARED,FIELD SQUARED,AND FIELD
C
      SUM(1)=SUM(1)+EE
      SUM(21)=SUM(21)+ZZ
      SUM(41)=SUM(41)+ZOLD
 1523 CONTINUE
      IF (MOD(N,2).EQ.1) GO TO 1524
      N = N/2
      K = K + 1
      IF (K.GT.20) GO TO 1524
      KMAX = MAX0(KMAX,K)
      SUM(K)=SUM(K)+SUM(K-1)
      SUM(K-1)=0.0
      SUM(K+20)=SUM(K+20)+SUM(K+19)
      SUM(K+19)=0.0
      SUM(K+40)=SUM(K+40)+SUM(K+39)
      SUM(K+39)=0.0
      GO TO 1523
 1524 CONTINUE
C
C      ACCUMULATE VECTOR AND MATRIX CONTRIBUTIONS AT THIS GRID POINT
C
      IF (NPF.LT.1) GO TO 1522
C
C      NOTHING TO BE FITTED
C
      DO 1521 I=1,NPF
      ERFI = ERF(I)
      CVEC(I)=CVEC(I)+ERFI*RES
       IJ0 = (I-1)*NPF
      DO 1521 J=I,NPF
      IJ = IJ0+J
      CMAT(IJ)=CMAT(IJ)+ERFI*ERF(J)
 1521 CONTINUE
 1522 CONTINUE
 1528 CONTINUE
 1530 CONTINUE
      WRITEF = 0
C
C      HAVING NOT USED  THE WRITE-RESIDUAL FLAG,
C      WE FORM ERROR FOR LAST FIT
C
      ZSUM=0.0
      ZZSUM = 0.0
      EESUM = 0.0
      DO 1531 K=1,KMAX
      EESUM = EESUM + SUM(K)
      ZZSUM = ZZSUM + SUM(K+20)
      ZSUM = ZSUM + SUM(K+40)
 1531 CONTINUE
      ERMS = SQRT(EESUM/(ZZSUM-ZSUM/SIZE))
      PERMS = ERMS*100.0
      IF (NPF.EQ.0) GO TO 1541
C
C      NO EQUATIONS TO SOLVE,SO COMPLETE MATRIX
C

      IF (NPF.LT.2) GO TO 1533
      DO 1532 I=2,NPF
      IM1=I-1
      IMINAC = IM1*NPF
      DO 1532 J=1,IM1
      IJ = IMINAC + J
      JI = (J-1)*NPF+I
      CMAT(IJ)=CMAT(JI)
 1532 CONTINUE
 1533 CONTINUE
C
C      APPLY DAMPING FACTOR
C
      FACTOR = 1.0 + DF*DF
      DO 1535 I=1,NPF
      II = (I-1)*NPF+I
      CMAT(II)=CMAT(II)*FACTOR
 1535 CONTINUE
C
C      SOLVE THE NORMAL EQUATIONS
C
      CALL SIMULX(CVEC,CMAT,RVEC,NPF)
C
C      IMPROVE THE OLD SOLUTION
C
      NAC = 0
      DO 1540 NC = 1,NPFA
      IF (JFIT(NC).NE.1) GO TO 1540
C
C      PARAMETER FIXED
C
      NAC = NAC + 1
      CC(NC)=CC(NC)+RVEC(NAC)
 1540 CONTINUE
C
C      ADJUST DEPENDANT PARAMETERS
C
      GX2 = GX*GX
      GY2 = GY*GY
      HX2 = HX*HX
      HY2 = HY*HY
 1541 CONTINUE
C
C  Done a loop
C
      ICO = ICO - 1
C
C      Compare new variable parameters with the old ones.
C
      AGAIN=.FALSE.
      DO I = 1,32
         IF (JFIT(I).EQ.1) THEN
            IF (CCLAST(I).LT.1.0E-10) CCLAST(I) = 1.0E-10
            DIFF=ABS(CC(I)-CCLAST(I))
            FDIFF = DIFF/CCLAST(I)
            IF (I.GE.4.AND.I.LE.8) THEN
               IF (FDIFF.GT.0.004) AGAIN=.TRUE.
            ENDIF
            IJ = I - 10
            IL = I - 9
            IF (I.GT.8) THEN
               IF ((MOD(IJ,3).EQ.0).OR.(MOD(IL,3).EQ.0)) THEN
                  IF (DIFF.GT.0.01) AGAIN = .TRUE.
               ENDIF
            ENDIF
            IK = I - 11
            IF (I.GT.8.AND.MOD(IK,3).EQ.0) THEN
               IF (FDIFF.GT.0.002) AGAIN = .TRUE.
            ENDIF
         ENDIF
         CCLAST(I)=CC(I)
      ENDDO
C
C  Type out heights if wanted
C
      IF (KFLOUT.EQ.1) THEN
         DO II = 1,NSTARS
            HTA(II) = CC(11+(II-1)*3)
            IF (HTA(II).GT.999999.0) HTA(II) = 999999.0
            IF (HTA(II).LT.-99999.0) HTA(II) = -99999.0
         ENDDO
         WRITE(TEXT,900)(HTA(LST),LST=1,NSTARS)
  900    FORMAT(' ',8F9.1)
         CALL WRUSER(TEXT,ISTAT)
      ENDIF
C
C  Type out profile if wanted and varaible
C
      IF (KFLOUT.EQ.1) THEN
         KW = 0
         DO I = 4,8
            IF (JFIT(I).EQ.1) KW = 1
         ENDDO
         IF (KW.EQ.1) THEN
            DO I = 4,8
               AP = CC(I)
               IF (ABS(AP).LT.0.00101) AP = SIGN(0.00101,AP)
               POUT(I-3) = 1.0/AP
            ENDDO
            TEMP = POUT(3)
            POUT(3) = 1.0/POUT(5)
            POUT(5) = POUT(4)
            POUT(4) = TEMP
            WRITE(TEXT,901)POUT
  901       FORMAT(' ',2F12.3,2X,F12.3,2X,2F11.2)
            CALL WRUSER(TEXT,ISTAT)
         ENDIF
      ENDIF
C
C  If star has drifted too far or has -ve height, remove it from
C  the fitting operation.
C
      DO LST=1,NSTARS
         K = 9 + (LST-1)*3
         IF (ABS(CC(K)-CCS(K)).GT.30.0.OR.
     +       ABS(CC(K+1)-CCS(K+1)).GT.30.0.OR.
     +       CC(K+2).LT.0.0) THEN
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
C
      IF ((AGAIN).AND.(ICO.GT.0)) GO TO 401
C
C   Now store the number of iterations done.
C
      ITER= ITSLIM - ICO
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
C  Allow for displacement of star position (fault in this s/r ?)
C  Only if the displacements were allowed to vary
C
      DO I = 1,NSFIT
         K = 9 + (I-1)*3
         IF(JJFIT(K).EQ.1)CCC(K) = CCC(K) + 1.0
         K = K + 1
         IF (JJFIT(K).EQ.1)CCC(K) = CCC(K) + 1.0
      ENDDO
C
C  Set Y values back to bottom to top
C
      DO I = 1,NSFIT
         K = 10 + (I-1)*3
         CCC(K) = REAL(NY) - CCC(K) + 1.0
      ENDDO
C
C -------------------------------------------------------
C
C
      END
C
C
C



