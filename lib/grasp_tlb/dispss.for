
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R DISPSS *
C      *            *
C      **************
C
C  This s/r finds the mean and std dev of a section of an I*2 image with
C  invalid points, iterating round 3 times to only use points that are
C  within 3 std dev of the calculated mean.
C  If the section is more than 50 on either side, a grid of points
C  is taken so that less than 100 on a side is sampled.
C
C
C   ARGUMENTS
C
C   INPUT
C     KDATA    I*2(NX,NY)  The image
C     NX       I           The X size of the image
C     NY       I           The Y size of the image
C     KX       I(2)        The X window in the image
C     KY       I(2)        The Y window in the image
C     INVAL    I           The pixel value flag for invalid pixel
C   OUTPUT
C     AM       R           The mean value
C     STD      R           The std dev of the pixels about the mean
C     IERR     I           The error flag  (=0 for success)
C
C
C     AJPENNY                RGO                      82-12-29
C ------------------------------------------------------------
C
C
C
      SUBROUTINE DISPSS(KDATA,NX,NY,KX,KY,INVAL,AM,STD,IERR)
C
C
C
      INTEGER*2 KDATA(NX,NY)
      INTEGER KX(2),KY(2)
      DOUBLE PRECISION S,SS,SN
C
C
C
      NXA = 1 + (KX(2)-KX(1))/50
      NYA = 1 + (KY(2)-KY(1))/50
C
      MIN = 32767
      MAX = -32768
      DO K = KY(1),KY(2),NYA
         DO J = KX(1),KX(2),NXA
            L = KDATA(J,K)
            IF (L.NE.INVAL) THEN
               IF (L.LT.MIN) MIN = L
               IF (L.GT.MAX) MAX = L
            ENDIF
         ENDDO
      ENDDO
C
C
C
      IF (MIN.GE.MAX) THEN
         IERR = 1
      ELSE
         IERR = 0
         DO ITER = 1,3
            S = 0.0
            SN = 0.0
            SS = 0.0
            DO K = KY(1),KY(2),NYA
               DO J = KX(1),KX(2),NXA
                  L = KDATA(J,K)
                  IF (L.NE.INVAL.AND.L.GE.MIN.AND.L.LE.MAX) THEN
                     AN = DBLE(L)
                     S = S + AN
                     SS = SS + AN*AN
                     SN = SN + 1.0
                  ENDIF
               ENDDO
            ENDDO
            IF (SN.GT.0.01) THEN
               AM = S/SN
            ELSE
               AM = 0.0
            ENDIF
            IF (SN.GT.1.1) THEN
               SS = (SS-S*S/SN)/(SN-1.0)
               IF (SS.GT.1.0E-20) THEN
                  STD = SQRT(SS)
               ELSE
                  STD = 0.0
               ENDIF
            ELSE
               STD = 0.0
            ENDIF
            AMIN = AM - 3.0*STD
            AMAX = AM + 3.0*STD
            IF (AMIN.GT.REAL(MIN)) MIN = INT(AMIN)
            IF (AMAX.LT.REAL(MAX)) MAX = INT(AMAX)
         ENDDO
      ENDIF
C
C
C
      END



