      SUBROUTINE REJECT(X,Y,NDIM,D,NN,KK,TT,M,J)
C
C *****************************************************************
C
C     SUBROUTINE REJECT(X,Y,NDIM,D,N,K,T,M,J)
C
C     WRITTEN BY JONES ET AL., 1967
C     MODIFIED BY W D PENCE, 1980
C
 
C     REJECT ANY POINT WHOSE RESIDUAL IS GREATER THAN BETA*SIGMA
C     FROM THE POLYNOMIAL DEFINED BY THE COEFFICIENTS D.
C
C     X = INDEPENDENT VARIABLE (R*4)
C     Y = THE DEPENDENT VARIABLE (R*4)
C     NDIM = FIRST DIMENSION OF X AND Y  (I*4)
C     D(30) = ARRAY CONTAINING THE POLYNOMIAL COEFFICIENTS (R*8)
C     N = NUMBER OF VALUES TO BE TESTED
C     K = THE NUMBER OF TERMS IN THE POLYNOMIAL
C     T = THE TEST VALUE USED TO SPECIFY THE REJECTION LEVEL
C     M = THE MODIFIED NUMBER OF VALUES AFTER REJECTION
C     J = AN INDICATOR WHICH IS ZERO ONLY ON THE LAST CALL
C         ( THIS PREVENTS ANY POINTS FROM BEING REJECTED)
C
C ***********************************************************
C
      DIMENSION X(NDIM,2),Y(NDIM),INDXP(21),INDXN(21)
      DOUBLE PRECISION D(30)
      CHARACTER CONT*1,BUFFER*80
C
2000  FORMAT(' ')
201   FORMAT(10X,F9.4,4X,2F8.3,F12.3)
2020  FORMAT('0',24X,'THE AVERAGE RESIDUAL IS ',E15.8)
2021  FORMAT(6X,'THE SUM OF THE SQUARES OF ALL RESIDUALS IS ',E15.8)
2022  FORMAT(23X,'THE STANDARD DEVIATION IS ',E15.8)
2023  FORMAT(22X,'THE MEASURE OF SKEWNESS IS ',E15.8)
2024  FORMAT(6X,'OF ALL THE RESIDUALS,',I5,' WERE NEGATIVE,'
     1 ,' AND ',I5,' WERE POSITIVE.')
2025  FORMAT(10X,I6,' RESIDUALS HAVE BEEN REJECTED.')
2030  FORMAT(14X,' HISTOGRAM OF THE RESIDUALS BEFORE REJECTION.')
2031  FORMAT(16X,'NUMBER OF',6X,'ABSOLUTE RANGE',5X,'NUMBER OF')
2032  FORMAT(12X,'NEGATIVE RESIDUALS  OF RESIDUALS  POSITIVE RESIDUALS')
204   FORMAT(' ',I21,F12.3,' < R < ',F5.3,I11)
205   FORMAT(' ',I21,F12.1,' AND GREATER ',I10)
206   FORMAT(12X,'RESIDUAL',7X,'X',7X,'Y',12X,'D')
C
      N=NN
      T=TT
      AVR=0.
      DO 9 M=1,21
      INDXN(M)=0
9     INDXP(M)=0
      U=0.
      V=0.
      NPOS=0
      NNEG=0
      M=1
      DO 23 I=1,N
      X(M,1)=X(I,1)
      X(M,2)=X(I,2)
      Y(M)=Y(I)
      XG=X(I,1)
      YG=X(I,2)
      S=POLY(XG,YG,D)
C
C     R=RESIDUAL FROM POLYNOMIAL FIT
C
      R=Y(I)-S
      AVR=AVR+R
      IF (R)11,15,12
11    NNEG=NNEG+1
      S=-R
      GO TO 13
12    NPOS=NPOS+1
      S=R
C
C     DO NOT INCREMENT COUNTER M IF RESIDUAL IS GREATER THAN T LIMIT
C
13    IF (S-T)15,15,14
14    IF (J)15,15,16
15    M=M+1
16    U=U+R*R
      V=V+R*R*R
      IF (R)17,22,20
17    IF (R+.2)18,19,19
18    INDXN(21)=INDXN(21)+1
      GO TO 23
19    JJ=1.-100.*R
      INDXN(JJ)=INDXN(JJ)+1
      GO TO  23
20    IF (R-0.2)22,22,21
21    INDXP(21)=INDXP(21)+1
      GO TO 23
22    JJ=1.+100.*R
      INDXP(JJ)=INDXP(JJ)+1
23    CONTINUE
      M=M-1
C
      END
