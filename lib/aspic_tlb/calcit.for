      SUBROUTINE CALCIT(POINTS,NUM,NCOEF,TAB)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*4 COEFF
      COMMON/CAL/Y1(100),X1(100),X2(100),Y2(100),NRUNS,IMAP
      COMMON/WAIT/WEIGHTC(100),IWEIGHTC
      DIMENSION POINTS(3,2,NUM)
      DIMENSION XTOL(8),RC(200),W(200),SCALE(8)
      DIMENSION B(20,20),G(21,21),D(20),VAR(20)
      DIMENSION TAB(2,8),XC(8),CX(8),CY(8)
      EXTERNAL REGISX,REGISY,LSXY,MONITX,MONITY
      IWEIGHTC=IWEIGHT
      DO 1058 I=1,NUM
        WEIGHTC(I)=1.0D0/(POINTS(3,1,NUM)+POINTS(3,2,NUM))
 1058 CONTINUE
      DO J = 1,8
      DO K = 1,2
      TAB(K,J) = 0.0
      ENDDO
      ENDDO
      DO 20 I=1,NUM
        X1(I)=POINTS(1,1,I)
        Y1(I)=POINTS(2,1,I)
        X2(I)=POINTS(1,2,I)
        Y2(I)=POINTS(2,2,I)
   20 CONTINUE
      M=NUM
C===============================================================
C
C
C              COMPUTE REGISTRATION POLYNOMIAL
C
C
C===============================================================
C DESCRIPTION
C
C              REGISTRATION OF THE IMAGES USING FIDUCIAL MARKS
C              
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C VARIABLE LIST
C =============
C    VARIABLE       TYPE                     PURPOSE
C    =========      ====                     ========
C
C    XTOL         1D ARRAY    ACCURACY REQUIRED ON EACH FIT PARAM
C                             USUALLY SUPPLIED BY      X02AAF
C    XC          1D ARRAY   PARAMS-ON INPUT=GUESSES OUTPUT=ANSWERS
C    CX          1D ARRAY   X-COEFFICIENTS FOR COMMON BLOCK
C    CY          1D ARRAY   Y-COEFFICIENTS FOR COMMON BLOCK
C    RC          1D ARRAY   RESIDUALS
C    W           1D ARRAY   WORKING SPACE
C    SCALE       1D ARRAY    STEP LENGTH IN OPTIMIZATIO
C    B           2D ARRAY    INVERSE OF G
C    G           2D ARRAY    HESSIAN
C    D          1D ARRAY     TRACE OF G
C   WEIGHT       1D ARRAY    OPTIMIZATION WEIGHTS
C    IMAP        INTEGER    CONTROL IMAP = 1 X-FITS IMAP=2 Y-FITS
C    IPASS       INTEGER    NO. OF PASSES MADE TO E04GAF DURING 1 FIT
C    NRUNS       INTEGER    ORDER OF FIT RUNS
C                           NRUNS = 1,2,3   GIVES  3,6,8 ORDER FITS
C    IW         INTEGER     DIMENSION OF W
C    NCOEF     INTEGER     NUMBER OF COEFFICIENTS IN CX AND CY
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      DO 987 I=1,8
      XC(I)=0.0
  987 CONTINUE
C  DETERMINE ORDERS OF FIT AND WEIGHTING
      IF(IWEIGHT.EQ.1)WRITE(6,225)
225   FORMAT(//,2X,'LEAST SQUARES WEIGHTED BY CENTROID')
      IF(NCOEF.EQ.3) NRUNS = 1
      IF(NCOEF.EQ.6) NRUNS = 2
      IF(NCOEF.EQ.8) NRUNS = 3
C=============================================================
C  TITLES
C ========
    3 OPEN(UNIT=7,NAME='PLATELOG',TYPE='UNKNOWN')
      WRITE(7,2001)M
 2001 FORMAT(1H ,'CALCULATION OF TRANSFORMATION FUNCTION',
     1 1H0,I3,' REFERENCE POINTS FOUND')
C=============================================================
      WRITE(7,226)
  226 FORMAT(3(1H0,/)
     11H ,'============================================')
      WRITE(7,228)
      WRITE(7,227)
227   FORMAT(1H ,'=            X-DIRECTION                   = ')
      WRITE(7,228)
228   FORMAT(1H ,'=                                          = ')
      WRITE(7,336)
  336 FORMAT(1H ,'============================================')
C=============================================================
C                         XSETUP
C=============================================================
C 3 VARIABLES
C=============
        IFAIL = 1
      MAXCAL = 100
      IPRINT=1
      CALL RDKEYI('ITERPR',.TRUE.,1,IPRINT,IVAL,ISTAT)
      METHOD = 1
      IMAP = 1
      DO 27 I = 1,8
      XTOL(I) = 1.0D-4
   27 CONTINUE
      GO TO (240,9,19),NRUNS
240   CONTINUE
      N = 3
      GO TO 4
9     CONTINUE
      IFAIL = 1
      N = 6
      GO TO 4
C==========================================================
C                   8 VARIABLE
C==========================================================
19    CONTINUE
      IFAIL = 1
      N = 8
      GO TO 4
C================================================================
C
C
C                  ============================
C                  =                          =
C                  =                          =
C                  =     Y  DIRECTION         =
C                  =                          =
C                  =                          =
C                  ============================
C
C
C=================================================================
C                         TITLES
C==================================================================
   29 WRITE(7,226)
      WRITE(7,228)
      WRITE(7,229)
      WRITE(7,228)
      WRITE(7,336)
229   FORMAT(1H  ,'=            Y-DIRECTION                   =')
C===================================================================
C                       Y SETUP
C===================================================================
C 3 VARIABLES
C ===========
         DO 300 I = 1,8
      XC(I)=0.0
  300 CONTINUE
      IMAP = 2
      GO TO (2900,39,49),NRUNS
2900  CONTINUE
      IFAIL = 1
      DO I = 1,200
      W(I) = 0.0D0
      RC(I) = 0.0D0
      ENDDO
      N=3
      GO TO 4
39    CONTINUE
      DO I = 1,200
         W(I) = 0.0D0
        RC(I) = 0.0D0
      ENDDO
      IFAIL = 1
      N = 6
      GO TO 4
C====================================================================
C 8 VARIABLE
C ==========
   49 DO 22 I =1,6
   22 CONTINUE
      IFAIL = 1
      N = 8
      DO 21 I = 1,200
      W(I) = 0.0D0
      RC(I) = 0.0D0
   21 CONTINUE
C====================================================================
C                       OPTIMIZATION
C====================================================================
    4 IW = N*(N+4)+M
      WRITE(7,207)
      WRITE(7,208) N,M
      WRITE(7,209)
      DO 742 IQ = 1,N
      IF(IMAP.EQ.1)THEN
        WRITE(7,210)IQ,XC(IQ)
        ELSE
        WRITE(7,2100)IQ,XC(IQ)
      ENDIF
  742 CONTINUE
207   FORMAT(3(1H0,/)10X,'E04GAF ALGORITHM')
208   FORMAT(//,2X,'PARAMETERS',//,2X,
     1'ORDER OF POLYNOMIAL = ',I2,//,2X,
     2'NUMBER OF REFERENCE POINTS SUPPLIED = ',I3)
209   FORMAT(/,2X,'INITIAL GUESSES')
210   FORMAT(/,(2X,' X(',I2,') = ',E16.8))
 2100 FORMAT(/,2X,'Y(',I2,') = ',E16.8)
211   FORMAT(//,5X,'SSQMIN FINAL VALUES OF FUNCTIONS AND VARIABLES')
212   FORMAT(//,2X,'FINAL SUM OF SQUARES = ',1PE16.8)
213   FORMAT(//,2X,'FINAL COEFFICIENT VALUES')

214   FORMAT(/,2X,'X(',I2,') = ',E16.8)
 2140 FORMAT(/,2X,'Y(',I2,') = ',E16.8)
      IF(IMAP.EQ.2) GO TO 5
C===================================================================
C X - FIT
C========
C=====================================================================
      CALL E04GAF(M,N,XC,RC,F,XTOL,METHOD,SCALE,W,IW,REGISX,LSXY
     1,MONITX,IPRINT,MAXCAL,IFAIL)
      WRITE(7,*) 'VALUE OF IFAIL IS',IFAIL

      DO 536 ICOPY=1,8
  536 CX(ICOPY)=XC(ICOPY)
C=====================================================================
      GO TO 6
C Y - FIT
C========
C=====================================================================
    5 CALL E04GAF(M,N,XC,RC,F,XTOL,METHOD,SCALE,W,IW,REGISY,LSXY,
     1MONITY,IPRINT,MAXCAL,IFAIL)
      WRITE(7,*) 'VALUE OF IFAIL IS',IFAIL
      DO 573 ICOPY=1,8
  573 CY(ICOPY)=XC(ICOPY)
C====================================================================

    6 CONTINUE
      WRITE(7,211)
      WRITE(7,212) F
      WRITE(7,213)
      WRITE(6,213)
      IMAP = IMAP
      DO 8 J = 1,N
      IF(IMAP.EQ.1)THEN
        WRITE(7,214)J,XC(J)
        WRITE(6,214)J,XC(J)
        ELSE
        WRITE(7,2140)J,XC(J)
        WRITE(6,2140)J,XC(J)
      ENDIF
      TAB(IMAP,J)=XC(J)
    8 CONTINUE
999   WRITE(7,211)
      N1 = N + 1
      CALL GOOD(M,N1,F,W,IW,N,B,G,D,VAR)
C====================================================================
C BRANCHING CONTROL
C====================================================================
        IF(IMAP.EQ.1) GO TO 29
 2004 CLOSE(UNIT=7)
 2007 RETURN
      END
