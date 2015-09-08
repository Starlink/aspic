      PROGRAM FCTHSI
C+
C      ASPIC PROGRAM *** FCTHSI ***
C
C      Written by K F Hartley at RGO on 23/7/81
c
C      This is part of the FALSECOLOUR package (FC****)
C
C      This program takes in 3 2-D Starlink images
C      and converts them into 3 new images of the same size.
C      The conversion is a 3-D co-ordinate transformation
C      applied to each pixel.
C
C      The result is to convert Red, Green and Blue images
C      into Hue, Saturation and Intensity.
C
C      Version #1
C-
      INCLUDE 'INTERIM(FMTPAR)'
      INCLUDE 'INTERIM(ERRPAR)'
C
C      IXR,IXB etc. are the dimensions of R,G etc.
C      IPR,IPB etc. are the memory mapping pointers
C      ISTAT is a general status reply.
C
      INTEGER IXR(2),IXG(2),IXB(2)
      INTEGER IPR,IPG,IPB,IPH,IPS,IPI
      INTEGER ISTAT
C
C   First get in the three input images RED , GREEN and BLUE
C   Note that no defaults are allowed and only 2-dimensional
C   arrays are accepted.
C   No second tries are allowed either.
C   The data are read in as REAL values.
C
      CALL RDIMAG('RED',FMT_R,2,IXR,NDIM,IPR,ISTAT)
      IF (ISTAT.NE.ERR_NORMAL.OR.NDIM.NE.2) THEN
         CALL WRERR('ERRIN')
         GO TO 800
      END IF
      CALL RDIMAG('GREEN',FMT_R,2,IXG,NDIM,IPG,ISTAT)
      IF (ISTAT.NE.ERR_NORMAL.OR.NDIM.NE.2) THEN
         CALL WRERR('ERRIN')
         GO TO 800
      END IF
      CALL RDIMAG('BLUE',FMT_R,2,IXB,NDIM,IPB,ISTAT)
      IF (ISTAT.NE.ERR_NORMAL.OR.NDIM.NE.2) THEN
         CALL WRERR('ERRIN')
         GO TO 800
      END IF
C
C   Now we check that all three input arrays are the same size
C   and exit if they are not.
C
      IF (IXR(1).NE.IXG(1).OR.IXR(1).NE.IXB(1)) THEN
         CALL WRERR('ERRXDIM')
         GO TO 800
      END IF
      IF (IXR(2).NE.IXG(2).OR.IXR(2).NE.IXB(2)) THEN
         CALL WRERR('ERRYDIM')
         GO TO 800
      END IF
C
C   Since the input dimensions agree we can use any one
C   as dimensions of the output arrays HUE , SAT , INTEN
C   so IXR is used.
C
C   Now get the three ouput images.
C
      CALL WRIMAG ('HUE',FMT_R,IXR,2,IPH,ISTAT)
      IF (ISTAT.NE.ERR_NORMAL) THEN
         CALL WRERR('ERROUT')
         GO TO 800
      END IF
      CALL WRIMAG ('SAT',FMT_R,IXR,2,IPS,ISTAT)
      IF (ISTAT.NE.ERR_NORMAL) THEN
         CALL WRERR('ERROUT')
         GO TO 800
      END IF
      CALL WRIMAG ('INT',FMT_R,IXR,2,IPI,ISTAT)
      IF (ISTAT.NE.ERR_NORMAL) THEN
         CALL WRERR('ERROUT')
         GO TO 800
      END IF
C
C   Having obtained the input and output images , do the transformation
C
      CALL TOHSI(%VAL(IPR),%VAL(IPG),%VAL(IPB),%VAL(IPH),
     :           %VAL(IPS),%VAL(IPI),IXR(1),IXR(2))
C
C   We can now free the data and exit.
C   The same should be done if there was an error earlier,so
C   on error it branches to here.
C
  800 CONTINUE
      CALL FRDATA (' ',ISTAT)
      CALL EXIT
      END
      SUBROUTINE TOHSI(RED,GREEN,BLUE,HUE,SAT,XINTEN,N,M)
C
C   This subroutine applies the HSI transformation as defined by
C   Lorre et al in a JPL Image Processing Report (1979)
C
C   The Red,Green and Blue intensities are considered to be
C   orthogonal 3-dimensional co-ordinates of a point in space.
C   The transformation then consists of a 3-D rotation to X,Y,Z
C   where the new Y axis lies along the old R=G=B line.
C   Then this new system is expressed in polar co-ordinates,
C   with I as r, H as azimuth and S as co-lattitude.
C
C   RED, GREEN and BLUE are the three input arrays.
C   HUE, SAT and XINTEN are the output arrays.
C   All of them of type real and of dimension N by M.
C
C   K F Hartley   RGO   24/11/80
C
       REAL RED(N,M),GREEN(N,M),BLUE(N,M)
       REAL HUE(N,M),SAT(N,M),XINTEN(N,M)
C
C   First define some useful constants.
C
      ROOT2=SQRT(2.0)
      ROOT3=SQRT(3.0)
      ROOT6=SQRT(6.0)
      PI=3.14159
C
C   Now loop through all the pixels applying the transformation.
C
      DO 200 K=1,M
         DO 100 J=1,N
C
C         First rotate (R,G,B) to (X,Y,Z)
C
            X=(BLUE(J,K)-2.0*GREEN(J,K)+RED(J,K))/ROOT6
            Y=(BLUE(J,K)+GREEN(J,K)+RED(J,K))/ROOT3
            Z=(-BLUE(J,K)+RED(J,K))/ROOT2
C
C         Now convert to polar co-ordinates
C
            XINTEN(J,K)=SQRT(X*X+Y*Y+Z*Z)
C
C         Note that saturation is not meaningful at the origin
C
            IF (XINTEN(J,K).GT.0.0) THEN
               SAT(J,K)=ACOS(Y/XINTEN(J,K))
            ELSE
               SAT(J,K)=0.0
            END IF
C
C         Likewise Hue may give problems on the Y-axis
C
            IF (Z.NE.0.0.AND.X.NE.0.0) THEN
               HUE(J,K)=ATAN2(Z,-X)
            ELSE
               HUE(J,K)=PI
            END IF
C
C         Finally, ensure that Hue is in the range 0 to 2*PI
C         and resolve ambiguity between 0 and PI
C
            IF (HUE(J,K).LT.0.0) HUE(J,K)=HUE(J,K)+2.0*PI
C            IF (HUE(J,K).GT.2.0*PI) HUE(J,K)=2.0*PI
            IF (HUE(J,K).EQ.PI .AND. (GREEN(J,K).GT.RED(J,K)) ) THEN
               HUE(J,K)=0.0
            END IF
C
C         This completes the pixel loops
C
  100    CONTINUE
  200 CONTINUE
      END
