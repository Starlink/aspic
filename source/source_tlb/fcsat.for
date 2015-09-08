      PROGRAM FCSAT
C+
C      ASPIC PROGRAM *** FCSAT ***
C
C      Written by K F Hartley at RGO on 24/11/81
C
C      This is part of the FALSECOLOUR package ( FC*** )
C
C      This program takes in the Saturation image from
C      program FCTHSI and enhances the saturation.
C
C      Version #1
C
C-
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
C      IPIN,IPOUT are pointers to the input and output frames.
C      IXIN holds the dimensions of the input (and output) frame.
C      ISTAT is a general status return
C
      INTEGER IPIN,IPOUT,IXIN(2),ISTAT
C
C   First pick up the input frame, which must be 2-dimensional.
C
      CALL RDIMAG('INPUT',FMT_R,2,IXIN,NDIM,IPIN,ISTAT)
      IF (ISTAT.NE.ERR_NORMAL.OR.NDIM.NE.2) THEN
         CALL WRERR('ERRIN')
         GO TO 800
      END IF
C
C   Now the output  frame. which is the same size as the input one.
C
      CALL WRIMAG('OUTPUT',FMT_R,IXIN,2,IPOUT,ISTAT)
      IF (ISTAT.NE.ERR_NORMAL) THEN
         CALL WRERR('ERROUT')
         GO TO 800
      END IF
C
C   Now pick up the cut-off value (see below) - a single real value
C
      CALL RDKEYR('CUTOFF',.FALSE.,1,CUT,NVALS,ISTAT)
      IF (ISTAT.GT.ERR_PARNUL.OR.NVALS.NE.1) THEN
         CALL WRERR('ERRCUT')
         GO TO 800
      END IF
C
C   Now ready to do the enhancement.
C
      CALL SAT(%VAL(IPIN),IXIN(1),IXIN(2),CUT,%VAL(IPOUT))
C
C   Finally free the data and exit.
C   This point is also reached from errors earlier.
C
  800 CONTINUE
      CALL FRDATA(' ',ISTAT)
      CALL EXIT
      END
      SUBROUTINE SAT (SATIN,N,M,CUT,SATOUT)
C
C      This subroutine enhances the saturation of an image.
C      That is to say the "S" image formed by FCTHSI is modified
C      to give a new image which has more significant values in it.
C
C      As computed by FCTHSI S is an angle in radians, which in
C      principle may have values from 0 to 0.955316.
C      0 gives points on the grey axis
C      0.955316 gives points on one of the pure primary axes
C      ( 0.6154798 give values of pure secondary colours )
C
C      However for typical astronomical images most of the pixels
C      lie close to the grey axis and hence have small values.
C      This routine increases the range of values present.
C
C      Parameters :  SATIN  input saturations (unchanged)
C                    N      x-dimension of SATIN and SATOUT
C                    M      y-dimension of SATIN and SATOUT
C                    CUT    a cut-off for scaling (unchanged)
C                    SATOUT output saturations (written)
C
C      K F Hartley   RGO   24/11/80
C
      REAL SATIN(N,M),SATOUT(N,M),CUT
      INTEGER ISHIST(100)
C
C   ISHIST is used to store the histogram of the input image.
C
C   First the histogram is zeroized.
C
      DO 100 I=1,100
         ISHIST(I)=0
  100 CONTINUE
C
C   Then the histogram is formed , and the actual largest value
C   present is stored.
C   Note that this is not needed to compute the histogram as all
C   the input values must be less than 1.0
C
      SMAX=SATIN(1,1)
      DO 200 J=1,M
         DO 150 I=1,N
            IF (SATIN(I,J).GT.SMAX) SMAX=SATIN(I,J)
            KPOS=INT(100.0*SATIN(I,J)) + 1
            IF (KPOS.GT.100) KPOS=100
            ISHIST(KPOS)=ISHIST(KPOS)+1
  150    CONTINUE
  200 CONTINUE
C
C   It would be possible to scale so that SMAX was mapped onto
C   the largest possible value, but it is posssible that a single
C   rogue point eg. a dust grain on one of the images
C   would ruin the scaling.
C   Consequently CUT is used to exclude the largest (CUT/100)% values
C   from the calculation of the scaling.
C   The scaling is later applied to ALL the values, so the largest
C   may overflow. Experiment shows that this is acceptable provided
C   values are scaled to a maximum of 0.6154 not 0.9553
C
C   First find out how many pixels need be considered.
C
      NPER=INT(REAL(N)*REAL(M)*(100.0-CUT)/100.0)
      NSUM=0
C
C   Now find the value for which there are NPER smaller pixel values.
C   Call it STOL
C   NB The actual values were multiplied by 100 to give
C   the positions stored in the histogram ISHIST.
C
      DO 300 K=1,100
         NSUM=NSUM+ISHIST(K)
         IF (NSUM.GE.NPER) THEN
            STOL=REAL(K-1)/100.0
            GO TO 400
         END IF
  300 CONTINUE
C
C   If a limit has not been found (because of the histogram bins)
C   then set the maximum value found as the cut-off value
C
      STOL=SMAX
  400 CONTINUE
C
C   It is now possible to do the scaling
C   The formula is :-
C      New value = old value * maximum allowed value / cut-off value
C
C   As mentioned above, the maximum allowed value is chosen
C   so that the cut-off value is scaled to the value
C   corresponding to pure secondary colours.
C
C   Geometrically this means scaling all values so that those lying on
C   the cut-off cone just touch the planes defining the limits.
C
      SMAXA=0.6154798
      DO 600 J=1,M
         DO 500 I=1,N
            SATOUT(I,J)=SATIN(I,J)*SMAXA/STOL
  500    CONTINUE
  600 CONTINUE
C
C   The transformation is now complete
C
      END
