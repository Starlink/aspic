      PROGRAM FCTRGB
C+
C      ASPIC PROGRAM *** FCTRGB ***
C
C      Written by K F Hartley at RGO on 24/7/81
C
C
C      This is the reverse process to FCTHSI
C      This program takes in 3 2-D Starlink images
C      and converts them into 3 new images of the same size.
C      The conversion is the inverse co-ordinate transformation
C      to the one discussed in FCTHSI.
C
C      The result is to convert Hue, Saturation and Intensity
C      images into a set of Red, Green and Blue images.
C
C      Version #1
C
C-
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
C   IXH etc. are the dimensions of the input frames
C   IPH etc. are the pointers to the input and output frames.
C   ISTAT is a general status reply.
C
      INTEGER IXH(2),IXS(2),IXI(2)
      INTEGER IPH,IPS,IPI,IPR,IPG,IPB
      INTEGER ISTAT
C
C   First get in the three input frames.
C   Note that there are no defaults,only one try is allowed,
C   only 2-D frames are accepted and the data are stored as REAL
C
      CALL RDIMAG('HUE',FMT_R,2,IXH,NDIM,IPH,ISTAT)
      IF (ISTAT.NE.ERR_NORMAL.OR.NDIM.NE.2) THEN
         CALL WRERR('ERRIN')
         GO TO 800
      END IF
      CALL RDIMAG('SAT',FMT_R,2,IXS,NDIM,IPS,ISTAT)
      IF (ISTAT.NE.ERR_NORMAL.OR.NDIM.NE.2) THEN
         CALL WRERR('ERRIN')
         GO TO 800
      END IF
      CALL RDIMAG('INT',FMT_R,2,IXI,NDIM,IPI,ISTAT)
      IF (ISTAT.NE.ERR_NORMAL.OR.NDIM.NE.2) THEN
         CALL WRERR('ERRIN')
         GO TO 800
      END IF
C
C   Now check that all three arrays are the same size
C   and exit if they are not.
C
      IF ( (IXH(1).NE.IXS(1)) .OR. (IXH(1).NE.IXI(1)) ) THEN
         CALL WRERR('ERRXDIM')
         GO TO 800
      END IF
      IF ( (IXH(2).NE.IXS(2)) .OR. (IXH(2).NE.IXI(2)) ) THEN
         CALL WRERR('ERRYDIM')
         GO TO 800
      END IF
C
C   Since the input dimensions are all the same , we can use
C   any one of them as dimensions of the output images,so we
C   choose IXH , and now get the 3 output images.
C
      CALL WRIMAG('RED',FMT_R,IXH,2,IPR,ISTAT)
      IF (ISTAT.NE.ERR_NORMAL) THEN
         CALL WRERR('ERROUT')
         GO TO 800
      END IF
      CALL WRIMAG('GREEN',FMT_R,IXH,2,IPG,ISTAT)
      IF (ISTAT.NE.ERR_NORMAL) THEN
         CALL WRERR('ERROUT')
         GO TO 800
      END IF
      CALL WRIMAG('BLUE',FMT_R,IXH,2,IPB,ISTAT)
      IF (ISTAT.NE.ERR_NORMAL) THEN
         CALL WRERR('ERROUT')
         GO TO 800
      END IF
C
C   Having found the input and output frames, it is now possible
C   to apply the transformation.
C
      CALL TORGB(%VAL(IPH),%VAL(IPS),%VAL(IPI),%VAL(IPR),
     :           %VAL(IPG),%VAL(IPB),IXH(1),IXH(2))
C
C   The data may now be cleared.
C
C   The same must be done if ay of the earlier error conditions arose,
C   so there are several branches to this point.
C
  800 CONTINUE
      CALL FRDATA (' ',ISTAT)
      CALL EXIT
      END
      SUBROUTINE TORGB(HUE,SAT,XINTEN,RED,GREEN,BLUE,N,M)
C
C   This routine applies the inverse HSI transformation as
C   described by Lorre et al in the 1979 JPL Report on Image Processing
C
C   The transformation takes H,S,I as polar co-ordinates which are first
C   converted to Cartesian, and then rotated to give the R,G,B images.
C
C   HUE , SAT and XINTEN are the three input images.
C   RED , GREEN and BLUE are the output images.
C   All 6 are REAL images of dimensions N by M
C
      REAL HUE(N,M),SAT(N,M),XINTEN(N,M)
      REAL RED(N,M),GREEN(N,M),BLUE(N,M)
C
C   K F Hartley   RGO   24/11/80
C
C   First some useful Constants are defined.
C
      ROOT2=SQRT(2.0)
      ROOT3=SQRT(3.0)
      ROOT6=SQRT(6.0)
      PI=3.14159
C
C   Now loop through all the pixels applying the transformation
C
      DO 200 K=1,M
         DO 100 J=1,N
C
C         First convert from polar co-ordinates
C
            SS=SIN(SAT(J,K))
            CS=COS(SAT(J,K))
            SH=SIN(HUE(J,K))
            CH=COS(HUE(J,K))
            XIN=XINTEN(J,K)
            X=-XIN*SS*CH
            Y=XIN*CS
            Z=XIN*SS*SH
C
C         Now apply the rotation.
C
            BLUE(J,K)=X/ROOT6 + Y/ROOT3 - Z/ROOT2
            GREEN(J,K)=(-X*ROOT2 + Y)/ROOT3
            RED(J,K)=X/ROOT6 + Y/ROOT3 + Z/ROOT2
C
C         Finally, if any of the values of Red, Green or Blue
C         have become negative, either through round-off or
C         because of saturation enhancement, set them to zero.
C         This corresponds to a maximum saturation value.
C
            IF (BLUE(J,K).LT.0.0) BLUE(J,K)=0.0
            IF (GREEN(J,K).LT.0.0) GREEN(J,K)=0.0
            IF (RED(J,K).LT.0.0) RED(J,K)=0.0
  100    CONTINUE
  200 CONTINUE
C
C   This completes the transformation.
C
      END
