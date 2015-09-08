C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C        ***************
C        *             *
C        * S/R BGAUSS  *
C        *             *
C        ***************
C
C
*
* PURPOSE
*          To fit a single Gaussian to the data in an array
C          assuming a single star near the centre
C
C          The profile fitted is :-
C
C                                2
C                            -1.d
C                I = HEIGHT.e     + BASE
C
C          where d = sqrt(((x-XO)/RX)**2+((y-YO)/RY)**2)
C
C
C          Maximum size of array = 70
*
* METHOD
C         It takes an array and fits the Gaussian to the data
C         in a defined section at the lower left hand.
C
*          IT ASSUMES A SINGLE STAR NEAR THE CENTRE OF THE DATA AND
*          EITHER USING FIXED ORTHOGONAL RADII, OR FLOATING ONES WITH
*          A FIRST ESTIMATE OF BOXSIZE/6.0, THE S/R FITS THE DATA BY
*          MEANS OF AN ITERATIVE LINEARISED FULLY 2-D APPROXIMATION
*          METHOD USING LEAST SQUARES.
C
C      The Output mag is = 30 - 2.5*log  (HEIGHT.RX.RY)
C                                      10
C
C      But in the event of failure it is set to:-
C                MAG        Reason
C                 50   Fitted height less than 0.00001
C
C
C
C ARGUMENTS
C   IN
C     APT(MS,NS)   Real            The input array
C     MS           Integer         The X size of the array
C     NS           Integer         The Y size of the array
C     M            Integer         X length of the data in array
C     N            Integer         Y length of data in array
C     KW           Integer         Flag radii fixed (1), or to be found (0)
C     ORIX         Real            Value of X radius (if fixed to be used)
C     ORIY         Real            Value of Y radius (if fixed to be used)
C     ITLIM        Integer         Limit of no of iterations to try
C  OUT
C     AMAG         Real            Output magnitude (may be false if failed)
C     HEIGHT       Real            Fitted Gaussian height
C     BASE         Real            Fitted Gaussian base
C     XO           Real            X fitted position
C     YO           Real            Y fitted position
C     RX           Real            X fitted radius (=input if fixed)
C     RY           Real            Y fitted radius (=input if fixed)
C     RMS          Real            RMS error between fit and data in box
C     ITER         Integer         No of iterations done
*
* CALLS
*       Grasp
*         GAUPX,PRODCR,PRODRR,SOLSYM,GAURES
*
*  USES
*
* NOTES
*          WRITTEN UP MORE FULLY IN THE AJP PDS PROGRAMS USER GUIDE
*
* WRITTEN BY
*          A.J. PENNY              RGO                   82-7-9
*
* -------------------------------------------------------------------
C
C
C
      SUBROUTINE BGAUSS(APT,MS,NS,M,N,KW,ORIX,ORIY,
     +                  ITLIM,AMAG,HEIGHT,BASE,XO,YO,RX,RY,
     +                  RMS,ITER)
C
C
C
      REAL S(7),D(6,6),E(6),F(70,4),G(70,4)
      REAL RA(3),RB(3),SF(4,4),SG(4,4)
      REAL APT(MS,NS)
      REAL ISE(6),ISF(6),ISG(6)
      DATA ISE/6,1,2,4,3,5/,ISF/4,1,2,1,3,1/,ISG/4,1,1,2,1,3/
C
C ASSUME STAR IS AT CENTRE OF DATA
C
      XO = REAL(M)/2.0 + 0.5
      YO = REAL(N)/2.0 + 0.5
C
C SET TO FIXED OR FLOATING RADII
C
      IF (KW.EQ.0) THEN
         NE = 6
         NF = 3
         NG = 3
         RX = REAL(M)/6.0
         RY = REAL(N)/6.0
      ELSE
         NE = 4
         NF = 2
         NG = 2
         RX = ORIX
         RY = ORIY
      ENDIF
C
C  Set up default output values
C
      AMAG = 0.0
      HEIGHT = 0.001
      BASE = 0.001
      RMS = 0.0
      ITER = 20
      NINVAL = 0
C
C  Get sum of data
C
      SP = 0.0
      DO K = 1,N
         DO J = 1,M
            SP = SP + APT(J,K)
         ENDDO
      ENDDO
C
C -----------------------------------------------------------
C
C  Iterate until solved or too many iterations
C
      ITERA = 0
      ITER = 0
      DO WHILE (ITER.LT.20.AND.ITERA.EQ.0)
C
C  Set up the simultaneous equations
C
         CALL GAUPX(M,RX,XO,F,SF,RA,NF)
         CALL GAUPX(N,RY,YO,G,SG,RB,NG)
         S(6) = SP
         DO J = 1,M
            CALL PRODCR(APT,MS,NS,J,G,70,4,1,1,N,PR)
            F(J,4) = PR
         ENDDO
         DO J = 1,N
            CALL PRODRR(APT,MS,NS,J,F,70,4,1,1,M,PR)
            G(J,4) = PR
         ENDDO
         DO L = 1,NF
            CALL PRODRR(F,70,4,4,F,70,4,L,1,M,PR)
            S(L) = RA(L)*PR
         ENDDO
         DO L = 2,NG
            CALL PRODRR(G,70,4,4,G,70,4,L,1,N,PR)
            S(L+2) = RB(L)*PR
         ENDDO
         DO K = 1,NE
            DO J = K,NE
               KS = ISF(K)
               KST = ISF(J)
               KSU = ISG(K)
               KSV = ISG(J)
               D(J,K) = SF(KS,KST)*SG(KSU,KSV)
               D(K,J) = D(J,K)
            ENDDO
            KSX = ISE(K)
            E(K) = S(KSX)
         ENDDO
C
C  Solve the simultaneous equations
C
         CALL SOLSYM(D,E,6,NE)
C
C  Update the estimates as a result of this iteration and
C  decide if converged
C
         CALL GAURES(E,A,B,XO,YO,RX,RY,ITERA,M,N)
C
C
C
         ITER = ITER + 1
      ENDDO
C
C --------------------------------------------------------
C
C  Calculate the RMS
C
      RMS = 0.0
      DO K = 1,N
         DO J = 1,M
            RMS = RMS + ABS(APT(J,K)-(A*F(J,1)*G(K,1)+B))**2.0
         ENDDO
      ENDDO
      RMS=SQRT(RMS/REAL(N*M-1))
C
C  Calculate the magnitude
C
      AMAG = 30.0
      HEIGHT = A
      BASE = B
      IF (A.GT.1.0E-5) THEN
         AMAG = 30.0 - 2.5*ALOG10(A*RX*RY)
      ELSE
         AMAG = 50.0
      ENDIF
C
C  Check for bad fit
C
      IF (A.LE.1.0E-5.OR.RX.GT.500.0.OR.RY.GT.500.0) THEN
         IF (KW.EQ.0) THEN
            RX = 0.0
            RY = 0.0
         ENDIF
         BASE = 0.0
         HEIGHT = 0.0
         RMS = 0.0
         AMAG = 50.0
         XO = REAL(M)/2.0 + 0.5
         YO = REAL(N)/2.0 + 0.5
      ENDIF
C
C
C
C
C
      END



