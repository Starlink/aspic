      PROGRAM FCPACK
C+
C      ASPIC PROGRAM *** FCPACK ***
C
C      Written by K F Hartley at RGO on 24/11/81
C
C      This is part of the FALSECOLOUR package ( FC**** )
C
C      This progarm takes three 2-D Starlink images and forms
C      a single coded "falsecolour" image of the same size.
C
C      Version #1
C
C-
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
C   IXR , IXG and IXB contain the dimensions of Red Green and Blue.
C   IPR , IPG and IPB are pointers to Red , Green and Blue.
C   ISTAT is a general status reply.
C   IPOUT is a pointer to the output image.
C
      INTEGER IXR(2),IXG(2),IXB(2)
      INTEGER IPR,IPG,IPB
      INTEGER ISTAT,IPOUT
C
C   First get the 3 input images.
C   Note that only one try is allowed, the images must all be 2-D
C   and of the same size.
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
C   Now check that the arrays have the same dimensions - exit if not.
C
      IF ( (IXR(1).NE.IXG(1)) .OR. (IXR(1).NE.IXB(1)) ) THEN
         CALL WRERR('ERRXDIM')
         GO TO 800
      END IF
      IF ( (IXR(2).NE.IXG(2)) .OR. (IXR(2).NE.IXB(2)) ) THEN
         CALL WRERR('ERRYDIM')
         GO TO 800
      END IF
C
C   Now set up the output frame - of the same size as all
C   three of the input frames - so use IXR for it's dimensions.
C
      CALL WRIMAG('PACKED',FMT_R,IXR,2,IPOUT,ISTAT)
      IF (ISTAT.NE.ERR_NORMAL) THEN
         CALL WRERR('ERROUT')
         GO TO 800
      END IF
C
C   It is now possible to do the packing.
C
      CALL PACK(%VAL(IPR),%VAL(IPG),%VAL(IPB),%VAL(IPOUT),
     :          IXR(1),IXR(2))
C
C   All that remains is to free the data areas and exit.
C   This must be done if there was an error - so there are branches
C   to here from above.
C
  800 CONTINUE
      CALL FRDATA(' ',ISTAT)
      CALL EXIT
      END
      SUBROUTINE PACK (RED,GREEN,BLUE,OUT,N,M)
C
C      This routine takes three input images and packs
C      them into a single output image , using a coding which,
C      when displayed with the right look-up-table, gives
C      a falsecolour display.
C
C      The coding used is the one devised for the Starlink
C      inauguration, whereby
C
C      output = blue + 6*green + 36*red
C
C      This is the best choice given the present ARGS hardware,
C      but the position may change.
C
C      RED , GREEN and BLUE are the three input arrays.
C      OUT is the output array.
C      All of them are REAL and of dimension N by M
C
C      K F Hartley   RGO   24/11/81
C
      REAL RED(N,M),GREEN(N,M),BLUE(N,M),OUT(N,M)
C
C   First find the minimum and maximum values present amongst the
C   three arrays.
C
      XMIN=RED(1,1)
      XMAX=RED(1,1)
      DO 200 J=1,M
         DO 100 I=1,N
            IF (RED(I,J).GT.XMAX) XMAX=RED(I,J)
            IF (GREEN(I,J).GT.XMAX) XMAX=GREEN(I,J)
            IF (BLUE(I,J).GT.XMAX) XMAX=BLUE(I,J)
            IF (RED(I,J).LT.XMIN) XMIN=RED(I,J)
            IF (GREEN(I,J).LT.XMIN) XMIN=GREEN(I,J)
            IF (BLUE(I,J).LT.XMIN) XMIN=BLUE(I,J)
  100    CONTINUE
  200 CONTINUE
C
C   Now set the scaling factor.
C   This should give results so that , when integerized , the
C   results are in the range 0 to 5 inclusive. This is achieved
C   by the factor of 6 here.
C
      IF (XMAX.NE.XMIN) THEN
         FACTOR = 6.0/(XMAX-XMIN)
      ELSE
C
C      Note that if maximum=minimum the scaling factor can have
C      any finite value.
C
         FACTOR=1.0
      END IF
C
C   Now loop through all the pixels doing the scaling
C   and the packing.
C
      DO 400 J=1,M
         DO 300 I=1,N
            IR=INT((RED(I,J)-XMIN)*FACTOR)
            IF (IR.LT.0) IR=0
            IF (IR.GT.5) IR=5
            IG=INT((GREEN(I,J)-XMIN)*FACTOR)
            IF (IG.LT.0) IG=0
            IF (IG.GT.5) IG=5
            IB=INT((BLUE(I,J)-XMIN)*FACTOR)
            IF (IB.LT.0) IB=0
            IF (IB.GT.5) IB=5
C
C         Note that any value up to 5.99999..... should
C         give 5 as its integer value.
C
C         Now calculate and store the value for packing.
C
            OUT(I,J)=REAL(IB + 6*IG + 36*IR)
  300    CONTINUE
  400 CONTINUE
      END
