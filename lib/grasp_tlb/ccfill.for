CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C               *************
C               *            *
C               * S/R CCFILL *
C               *            *
C               **************
C
C
C  This takes the profile parameters and star (x,y,height) parameters
C  and loads them into CC and JFIT for use in the Lorentz 2-D profile
C  fitting subroutines.
C
C   Parameters
C      INPUT
C        DATA    This array contains part of the full array
C        NX      This is the first dimension of DATA
C        NY      This the second dimension of DATA
C        PARFIX  This flags wether the profile parameters are fixed
C                or floating
C        PROF    This contains the star profile
C        X       The X coords of the stars in the DATA area
C        Y       The Y coords of the stars in the DATA area
C
C   INPUT/OUTPUT
C
C        NSTAR   The number of stars (1 to 8; if outside that a single
C                star at the centre is assumed and NSTAR returned as 1)
C
C     OUTPUT
C        CC      This array will contain the initial values for
C                background (1,2,3)
C                star profile (4,5,6,7,8)
C                and star posns and heights (9,10,...,32)
C        JFIT    This holds flags as to wether the parameters in CC
C                are fixed (-1)
C                not being used (0)
C                are allowed to vary (1)
C
C   A.J.Penny                                          82-7-12
C --------------------------------------------------------------
C
C
C
      SUBROUTINE CCFILL(DATA,NX,NY,PARFIX,PROF,X,Y,NSTAR,CC,JFIT)
C
C
C
      REAL PROF(5),CC(32),DATA(NX,NY),X(8),Y(8)
      INTEGER JFIT(32)
      LOGICAL PARFIX
C
C   Estimate the background parameters from the edge of the array
C
      CC(1) = DATA(1,1)
      CC(2) = (DATA(NX,1)-DATA(1,1))/REAL(NX-1)
      CC(3) = (DATA(1,NY)-DATA(1,1))/REAL(NY-1)
      JFIT(1)=1
      JFIT(2)=1
      JFIT(3)=1
C
C  Set the profile parameters to be fixed or to be variable
C  If variable set at input starting values
C
      IF (PARFIX) THEN
         DO K = 4,8
            CC(K) = PROF(K-3)
            JFIT(K) = -1
         ENDDO
      ELSE
         DO K = 4,8
            CC(K) = PROF(K-3)
            JFIT(K) = 1
         ENDDO
      ENDIF
C
C  If NSTAR outside range 1 to 8, assume a single star at centre
C
      IF (NSTAR.LT.1.OR.NSTAR.GT.8) THEN
         NSTAR = 1
         X(1) = REAL(NX)/2.0 + 0.5
         Y(1) = REAL(NY)/2.0 + 0.5
      ENDIF
C
C  Put in star positions and rough heights and flag these to be fitted
C  Calculate heights from height above base of highest point in 3x3 box 
C  round the position.
C
      DO L = 1,NSTAR
         LD = (L-1)*3
         CC(9+LD) = X(L)
         CC(10+LD) = Y(L)
         AMAX = 0.0
         DO J = 1,3
            DO K = 1,3
               KX = X(L) - 2 + K
               KY = Y(L) - 2 + J
               IF(KX.GE.1.AND.KX.LE.NX.AND.KY.GE.1.AND.KY.LE.NY) THEN
                  BASE = CC(1) + CC(2)*REAL(KX) + CC(3)*REAL(KY)
                  TOP = DATA(KX,KY) - BASE
                  IF (TOP.GT.AMAX) AMAX = TOP
               ENDIF
            ENDDO
         ENDDO
         CC(11+LD) = AMAX
         JFIT(9+LD) = 1
         JFIT(10+LD) = 1
         JFIT(11+LD) = 1
      ENDDO
C
C  Set other stars to be not done
C
      IF (NSTAR.LT.8) THEN
         DO K = 12+LD,32
            CC(K) = 0.0
            JFIT(K) = 0
         ENDDO
      ENDIF
C
C
C
      END



