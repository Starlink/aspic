C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C        ***************
C        *             *
C        * S/R AGAUSS  *
C        *             *
C        ***************
C
C
C
C PURPOSE
C          TO SELECT A SMALL BOX FROM AN IMAGE AND FIT A SINGLE
C          GAUSSIAN TO THE DATA, ASSUMING A SINGLE STAR NEAR THE
C          CENTRE
C
C          The profile fitted is :-
C
C                               2
C                           -1.d
C                   I  = HEIGHT.e      + BASE
C
C          where d = sqrt(((x-XO)/RX)**2+((y-YO)/RY)**2)
C
C
C METHOD
C          IT ASSUMES A SINGLE STAR NEAR THE CENTRE AND EITHER
C          USING FIXED ORTHOGONAL RADII, OR FLOATING ONES WITH
C          A FIRST ESTIMATE OF BOXSIZE/6, THE S/R FITS THE DATA BY
C          MEANS OF AN ITERATIVE LINEARISED FULLY 2-D APPROXIMATION
C          METHOD USING LEAST SQUARES.
C
C      The Output mag is = 30 - 2.5*log  (HEIGHT.RX.RY)
C                                      10
C
C      But in the event of failure it is set to:-
C                 50   Fitted height less than 0.00001
C                 60   Star too near edge (all of box must be in image)
C                 61   Box size greater than 70
C                 70   No valid pixels in box
C
C
C
C ARGUMENTS
C   IN
C     KPT(KX,KY)   Integer*2       The input image
C     KX           Integer         X length of the image
C     KY           Integer         Y length of image
C     AX           Real            X position of star in image
C     AY           Real            Y position of star in image
C     M            Integer         X size of box round star
C     N            Integer         Y size of box round star
C     KW           Integer         Flag radii fixed (1), or to be found (0)
C     ORIX         Real            Value of X radius (if fixed to be used)
C     ORIY         Real            Value of Y radius (if fixed to be used)
C     INVAL        Integer         Flag value of a pixel if Invalid
C     ITLIM        Integer         Limit of no of iterations to try
C  OUT
C     AMAG         Real            Output magnitude (may be false if failed)
C     HEIGHT       Real            Fitted Gaussian height
C     BASE         Real            Fitted Gaussian base
C     DXO          Real            X diff of fitted posn from input posn
C     DYO          Real            Y diff of fitted posn from input posn
C     ANX          Real            X fitted position
C     ANY          Real            Y fitted position
C     RX           Real            X fitted radius (=input if fixed)
C     RY           Real            Y fitted radius (=input if fixed)
C     RMS          Real            RMS error between fit and data in box
C     ITER         Integer         No of iterations done
C     NINVAL       Integer         No of Invalid pixels in box
C
C
C CALLS
C       Grasp
C         BGAUSS
C
C  USES
C     Integer*2 input array
C
C NOTES
C          WRITTEN UP MORE FULLY IN THE AJP PDS PROGRAMS USER GUIDE
C
C WRITTEN BY
C          A.J. PENNY              RGO                   82-7-9
C
C -------------------------------------------------------------------
C
C
C
      SUBROUTINE AGAUSS(KPT,KX,KY,AX,AY,M,N,KW,ORIX,ORIY,INVAL,
     +                  ITLIM,AMAG,HEIGHT,BASE,DXO,DYO,ANX,ANY,RX,RY,
     +                  RMS,ITER,NINVAL)
C
C
C
      REAL DATA(70,70)
      INTEGER*2 KPT(KX,KY)
      LOGICAL VALID
C
C GET X,Y COORD AT STAR
C
      NX = INT(AX)
      NY = INT(AY)
C
C  Set up default output values
C
      AMAG = 0.0
      HEIGHT = 0.001
      BASE = 0.001
      DXO = 0.0
      DYO = 0.0
      ANX = AX
      ANY = AY
      RMS = 0.0
      ITER = 20
      NINVAL = 0
C
C  Set continuation flag
C
      VALID = .TRUE.
C
C CHECK IF BOX LIES TOTALLY IN IMAGE
C
      JS = NX - M/2
      JE = JS + M - 1
      KS = NY - N/2
      KE = KS + N - 1
      IF(JS.LT.1.OR.JE.GT.KX.OR.KS.LT.1.OR.KE.GT.KY) THEN
         AMAG = 60.0
         VALID = .FALSE.
      ENDIF
C
C  Check if box size ok
C
      IF (VALID) THEN
         IF(M.LT.2.OR.M.GT.70.OR.N.LT.2.OR.N.GT.70) THEN
            VALID = .FALSE.
            AMAG = 61.0
         ENDIF
      ENDIF
C
C  Extract data.
C  If any Invalid pixels, set to pixel before
C
      IF (VALID) THEN
C
C  Find first valid value
C
         JS = NX - M/2 - 1
         KS = NY - N/2 - 1
         DO K = 1,N
            KA = KS + K
            DO J = 1,M
               JA = JS + J
               L = KPT(JA,KA)
               IF(L.NE.INVAL) GO TO 32
            ENDDO
         ENDDO
   32    CONTINUE
         LAST = L
C
         NINVAL = 0
         DO K = 1,N
            KA = KS + K
            DO J = 1,M
               JA = JS + J
               L = KPT(JA,KA)
               IF(L.EQ.INVAL) THEN
                  L = LAST
                  NINVAL = NINVAL + 1
               ENDIF
               LAST = L
               DATA(J,K) = L
            ENDDO
         ENDDO
         IF(NINVAL.EQ.(M*N)) THEN
            AMAG = 70.0
            VALID = .FALSE.
         ENDIF
      ENDIF
C
C  Solve for Gaussian
C
      IF (VALID) THEN
          CALL BGAUSS(DATA,70,70,M,N,KW,ORIX,ORIY,ITLIM,AMAG,
     +                HEIGHT,BASE,XO,YO,RX,RY,RMS,ITER)
C
C  Calc position in main image
C
         ANX = REAL(NX) + XO - M/2 - 1
         ANY = REAL(NY) + YO - N/2 - 1
         DXO = ANX - AX
         DYO = ANY - AY
C
C
C
      ENDIF
C
C
C
      END



