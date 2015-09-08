      SUBROUTINE IAMHISS (IXEXT,IYEXT,INPICT,WORK,HMIN,HMAX,
     :                    IXR,IYR,NQUAD,STATUS)
C+
C  IAMHISS
C
C	called from program IAMHIS
C	calculates and plots an intensity histogram of image to quadrant of args
C
C  Given (arguments)
C    IXEXT,IYEXT (I)     Dims of INPICT.
C    INPICT      (RA)    Image to be histogrammed.
C    WORK        (I*2A)  Work array for CLRQUADS.
C    HMIN,HMAX   (R)     Values of min. and max. bins in histogram.
C    IXR,IYR     (I)     Quadrant origin.
C    NQUAD       (I)     Number of quadrant histogram plotted in.
C
C  Returned (arguments)
C    STATUS      (I)     Return status. = 0 for success, = 1 if
C                        the histogram had (essentially) zero range.
C
C  B. D. KELLY/ROE/1981
C  D. Tudhope/ROE/Sept 1982
C  A C Davenhall/ROE/13.7.84
C       {Added status return that is triggered by being given a
C        histogram with 0 range. No plotting is attempted if this
C        case is detected.}
C-

      INTEGER IXEXT,IYEXT,NQUAD,IXR,IYR,STATUS
      REAL INPICT(IXEXT,IYEXT),HMIN,HMAX
      INTEGER*2 WORK(256,256)
C*
      INTEGER NBIN
      PARAMETER (NBIN=128)
      REAL OUT
      PARAMETER (OUT=1.0E-6)
C*
      REAL RHIST(NBIN),SPEC(128),BINEND(129)
      REAL SPECMIN,SPECMAX,RANGE
      INTEGER JHIST(NBIN)
      INTEGER J
C*  clear quadrant to be plotted in

      CALL CLRQUADS(WORK,IXR,IYR)
      CALL GENHIS(INPICT,IXEXT,IYEXT,1,IXEXT,1,IYEXT,HMAX,HMIN,
     :            JHIST,NBIN)
C*  put histogram into 128-element real array for plotting
      DO J=1,NBIN
        RHIST(J)=JHIST(J)
      ENDDO
      DO J=1,129
        BINEND(J)=FLOAT((J-1)*NBIN)/128.0
      ENDDO
      CALL REBIN(RHIST,NBIN,SPEC,128,BINEND,129)
C*  remove end bins as these contain a count of all the
C*  out-of-range values, and effect the autoscaling of plots
      SPEC(1)=0.0E0
      SPEC(128)=0.0E0
C*
C*  Check if the histogram contains a sensible range of numbers
C*  if it does then plot the histogram. If it does not then
C*  set the return status and do not attempt a plot.
C*
      SPECMIN=SPEC(2)
      SPECMAX=SPEC(2)
      DO J=3,127
        IF (SPEC(J).LT.SPECMIN) SPECMIN=SPEC(J)
        IF (SPEC(J).GT.SPECMAX) SPECMAX=SPEC(J)
      END DO
      RANGE=SPECMAX-SPECMIN
      IF (RANGE.GT.OUT) THEN
        STATUS=0
        CALL ARGPLOT(SPEC,NQUAD)
      ELSE
        STATUS=1
      END IF
      END
