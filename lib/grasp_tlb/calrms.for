C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R CALRMS *
C      *            *
C      **************
C
C
C
C   PURPOSE
C       This takes the results of a star fitting program which has
C     found the positions and parameters of up to 8 stars in an image.
C     It takes the image, the input residuals and the input positions.
C     It then takes a box round each position and works out the RMS error
C     of the residuals in this box and the number of Invalid pixels in
C     this box.
C
C     The residuals are in an array which is only a section of the image.
C     The size of the box round each star is designed to cover the
C     main part of the star image, being 4 times the input star 'radius'.
C
C
C   ARGUMENTS
C  IN
C    RESID(NX,NY)     Real       Residuals between fit and image
C    NX               Integer    X size of Residuals array
C    NY               Integer    Y size of Residuals array
C    CC(32)           Real       Position values are in (CC(9),CC(10));
C                                (CC(12),CC(13));(CC(15),CC(16)); etc
C    XHALF            Real       X 'radius' of star profile
C    YHALF            Real       Y 'radius' of star profile
C    NST              Integer    No of stars in fit
C    KPT(NPIX,NLINE)  Integer*2  Image fit made to
C    IX1              Integer    X image pixel at RESID start
C    IY1              Integer    Y image pixel at RESID start
C    INVAL            Integer    Flag value of Invalid pixel
C  OUT
C    RMS(8)           Real       RMS residual in box round each star
C    NINSTS(8)        Integer    No of Invalid pixels in each star box
C
C   CALLS
C    None
C
C   USES
C     I*2 arrays
C
C
C   A.J.PENNY                   RGO                    83-2-22
C
C -----------------------------------------------------------------
C
C
C
      SUBROUTINE CALRMS(RESID,NX,NY,CC,XHALF,YHALF,NST,RMS,
     +                  KPT,NPIX,NLINE,IX1,IY1,INVAL,NINSTS)
C
C
C
      REAL RESID(NX,NY),CC(32),RMS(8)
      DOUBLE PRECISION SUM
      INTEGER*2 KPT(NPIX,NLINE)
      INTEGER NINSTS(8)
C
C  For the NST stars, get the area round each star (checking if any
C  falls outside image) and get sum of square of residuals and sum of
C  number of Invalid points. Then calc RMS of residuals.
C
      DX = ABS(2.0*XHALF)
      IF (DX.LT.1.0) DX = 1.0
      DY = ABS(2.0*YHALF)
      IF (DY.LT.1.0) DY = 1.0
      DO K = 1,NST
         I = 9 + 3*(K-1)
         X = CC(I)
         Y = CC(I+1)
         AX1 = X - DX
         AX2 = X + DX
         AY1 = Y - DY
         AY2 = Y + DY
         KX1 = 1
         IF (AX1.GT.REAL(NX)) KX1 = NX
         IF (AX1.GE.1.0.AND.AX1.LE.REAL(NX)) KX1 = AX1
         KX2 = 1
         IF (AX2.GT.REAL(NX)) KX2 = NX
         IF (AX2.GE.1.0.AND.AX2.LE.REAL(NX)) KX2 = AX2
         KY1 = 1
         IF (AY1.GT.REAL(NY)) KY1 = NY
         IF (AY1.GE.1.0.AND.AY1.LE.REAL(NY)) KY1 = AY1
         KY2 = 1
         IF (AY2.GT.REAL(NY)) KY2 = NY
         IF (AY2.GE.1.0.AND.AY2.LE.REAL(NY)) KY2 = AY2
         IF (KX2.GT.KX1.AND.KY2.GT.KY1) THEN
            SUM = 0.0
            NIN = 0.0
            DO LY = KY1,KY2
               DO LX = KX1,KX2
                  SUM = SUM + DBLE(RESID(LX,LY))*DBLE(RESID(LX,LY))
                  LXA = LX + IX1 - 1
                  LYA = LY + IY1 - 1
                  IF (KPT(LXA,LYA).EQ.INVAL) NIN = NIN + 1
               ENDDO
            ENDDO
            NP = (KX2-KX1+1)*(KY2-KY1+1)
            RMS(K) = SNGL(SQRT(SUM/DBLE(NP-1)))
            NINSTS(K) = NIN
         ELSE
            RMS(K) = 0.0
            NINSTS(K) = 0
         ENDIF
      ENDDO
C
C
C
      END



