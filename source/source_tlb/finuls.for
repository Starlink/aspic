      PROGRAM FINULS
C+
C
C      ASPIC PROGRAM *** FINULS ***
C
C      WRITTEN BY K F HARTLEY AT RGO ON 11/8/81
C
C      FINULS STANDS FOR
C      "FILTERING OF IMAGE NOISE USING LOCAL STATISTICS"
C      AS DESCRIBED BY JONG-SEN LEE IN
C      COMPUTER GRAPHICS AND IMAGE PROCESSING 15 , 380-389 (1981)
C
C      AN INPUT FRAME IS FILTERED TO REDUCE NOISE
C      IN RELATIVELY FLAT AREAS BUT LEAVING SHARP EDGES ,
C      GIVING AN OUTPUT FRAME AND A SECOND FRAME SHOWING
C      THE LOCATION OF THE EDGES WHICH WERE DETECTED.
C
C    PARAMETERS ARE:
C        INPUT:   THE INPUT IMAGE
C        OUTPUT:  THE SMOOTHED OUTPUT IMAGE
C        EDGES:   THE OUTPUT IMAGE CONTAINING THE EDGES FOUND
C        BOX:     THE SIZE OF THE BOX FOR SMOOTHING AND IN WHICH
C                 EDGES ARE SEARCHED FOR
C        FACTOR:  FACTOR FOR DEFINING THE PRESENCE OF AN EDGE.
C                 IF THE VARIANCE WITHIN A SAMPLE BOX AREA IS
C                 GREATER THAN FACTOR*VARIANCE IN THE SURROUNDING AREA
C                 THEN AN EDGE IS CONSIDERED TO BE PRESENT
C
C
C-
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
      INTEGER IPIN,IPOUT,IPMEAN,IPVAR,ISTAT
      INTEGER IX(2)
C
C   First pick up the input and output frames , and then get
C   dynamic storage for the means and variances.
C
      CALL RDIMAG('INPUT',FMT_R,2,IX,NDIM,IPIN,ISTAT)
      IF (ISTAT.NE.ERR_NORMAL.OR.NDIM.NE.2) THEN
         CALL WRERR('HELL')
         GO TO 800
      END IF
      CALL WRIMAG('OUTPUT',FMT_R,IX,2,IPOUT,ISTAT)
      IF (ISTAT.NE.ERR_NORMAL) THEN
         CALL WRERR('HELL')
         GO TO 800
      END IF
      CALL WRIMAG('EDGES',FMT_R,IX,2,IPEDGE,ISTAT)
      IF (ISTAT.NE.ERR_NORMAL) THEN
         CALL WRERR('HELL')
         GO TO 800
      END IF
      NVALS=IX(1)*IX(2)
      CALL GETDYN('MEANS',FMT_R,NVALS,IPMEAN,ISTAT)
      IF (ISTAT.NE.ERR_NORMAL) THEN
         CALL WRERR('HELL')
         GO TO 800
      END IF
      CALL GETDYN('VARS',FMT_R,NVALS,IPVAR,ISTAT)
      IF (ISTAT.NE.ERR_NORMAL) THEN
         CALL WRERR('HELL')
         GO TO 800
      END IF
C
C   NBOX is the size of the box used for computing the
C   means and variances.
C
      NBOX=7
      CALL RDKEYI('BOX',.TRUE.,1,NBOX,I,ISTAT)
      IF (ISTAT.GT.ERR_PARNUL) THEN
         CALL WRERR('HELL')
         GO TO 800
      END IF
C
C   NBOX has to be odd.
C
      IF (MOD(NBOX,2).EQ.0) THEN
         CALL WRUSER('NBOX SHOULD BE ODD ... 1 ADDED',ISTAT)
         NBOX=NBOX+1
      END IF
C
C   FAC IS THE "EDGE" FACTOR
C
C   IF VAR > VARMIN*FAC THEN IT IS AN "EDGE" POINT
C   THINK OF IT AS THE "SIGMA" FOR DETECTION OF A REAL SIGNAL
C
      FAC=3.0
      CALL RDKEYR( 'FACTOR',.TRUE.,1,FAC,I,ISTAT)
      IF (ISTAT.GT.ERR_PARNUL.OR.FAC.LT.1.0) THEN
         CALL WRERR('HELL')
         GO TO 800
      END IF
C
C   Now compute the mean and variance of each pixel of the
C   input array over an NBOX by NBOX box centred on that pixel.
C
      CALL GETSTATS(%VAL(IPIN),%VAL(IPMEAN),%VAL(IPVAR),IX(1),IX(2)
     :              ,NBOX)
C
C   Apply the filter
C
      CALL FILT(%VAL(IPIN),%VAL(IPMEAN),%VAL(IPVAR),%VAL(IPOUT),
     :          IX(1),IX(2),NBOX,FAC,%VAL(IPEDGE))
C
C   Tidy up and exit.
C
  800 CALL FRDATA(' ',ISTAT)
      CALL EXIT
      END
      SUBROUTINE GETSTATS(DATA,AVE,VAR,N,M,IB)
C
C   Compute average and variance for each pixel in DATA
C   storing the results in AVE and VAR.
C   All arrays are of size N by M and the box used
C   is IB by IB.
C
C   The edges of the array are not treated.
C
      REAL DATA(N,M),AVE(N,M),VAR(N,M)
      IHW=(IB-1)/2
      RVALS=REAL(IB)*REAL(IB)
      RVALS1=RVALS-1.0
      DO 200 J=1+IHW,M-IHW
         DO 100 I=1+IHW,N-IHW
            SUM=0.0
            DO 20 J1=J-IHW,J+IHW
               DO 10 I1=I-IHW,I+IHW
                  SUM=SUM+DATA(I1,J1)
   10          CONTINUE
   20       CONTINUE
            AVE(I,J)=SUM/RVALS
  100    CONTINUE
  200 CONTINUE
      DO 400 J=1+IHW,M-IHW
         DO 300 I=1+IHW,N-IHW
            SUM=0.0
            DO 220 J2=J-IHW,J+IHW
               DO 210 I2=I-IHW,I+IHW
                  VAL=DATA(I2,J2)-AVE(I,J)
                  SUM=SUM+VAL*VAL
  210          CONTINUE
  220       CONTINUE
            VAR(I,J)=SUM/RVALS1
  300    CONTINUE
  400 CONTINUE
      END
      SUBROUTINE FILT(IN,AVE,VAR,OUT,N,M,IB,FAC,EDGES)
C
C   Apply the filter - a weighted mean of the local average
C   and the data value.
C
C   The weighting factor (RK) is derived from the local variance
C   VAR(i,j) and the local noise variance VMIN
C
      REAL IN(N,M),AVE(N,M),VAR(N,M),OUT(N,M)
      REAL EDGES(N,M)
C
C   So there is no problem with the edges , the OUTPut array
C   is first filled with a copy of the input array.
C   THE "EDGES" ARRAY IS FILLED WITH ZEROS
C
      IHW2=IB-1
      IHW=(IB-1)/2
      DO 20 J=1,M
         DO 10 I=1,N
            OUT(I,J)=IN(I,J)
            EDGES(I,J)=0.0
   10    CONTINUE
   20 CONTINUE
C
C   The filter can only be applied at points which are centred
C   on IB by IB boxes where the variances have been calculated.
C   This means that the first IB pixels in from all edges must
C   be ignored.
C   THE ORIGINAL DATA VALUES ARE USED IN THOSE PLACES.
C
      DO 200 J=1+IHW2,M-IHW2
         DO 100 I=1+IHW2,N-IHW2
            IF (VAR(I,J).LE.0.0) GO TO 100
            CALL VARMIN(VAR,N,M,I,J,IHW,VMIN)
            IF (VAR(I,J).GT.VMIN*FAC) THEN
               EDGES(I,J)=1.0
               CALL DOEDGE(AVE,VAR,N,M,I,J,IB,A,V)
               IF (V.LE.0.0) GO TO 100
               Q=V-VMIN
               IF (Q.LT.0.0) Q=0.0
               RK=Q/(Q+VMIN)
               OUT(I,J)=A+RK*(IN(I,J)-A)
            ELSE
               Q=VAR(I,J)-VMIN
               RK=Q/(Q+VMIN)
               OUT(I,J)=AVE(I,J)+RK*(IN(I,J)-AVE(I,J))
            END IF
  100    CONTINUE
  200 CONTINUE
      END
      SUBROUTINE VARMIN(VAR,N,M,IC,JC,IHW,VMIN)
C
C   To apply the filter it is necessary to estimate the variance
C   of the noise at the point in question.
C   In principle a good estimate of this is the smallest variance
C   in a box centred on the point in question.
C   However for the typically small boxes being used, it is
C   better to take the average of the 5 smallest variances
C   in the box.
C
      REAL VAR(N,M),WORK(1000)
C
C   First the data are copied into a 1-D work space.
C
      IPOS=1
      DO 20 J=JC-IHW,JC+IHW
         DO 10 I=IC-IHW,IC+IHW
            WORK(IPOS)=VAR(I,J)
            IPOS=IPOS+1
   10    CONTINUE
   20 CONTINUE
      IPOS=IPOS-1
C
C   Then the five smallest are sorted to the top.
C
      DO 40 J=1,5
         DO 30 I=J+1,IPOS
            IF (WORK(I).LT.WORK(J)) THEN
               TEMP=WORK(I)
               WORK(I)=WORK(J)
               WORK(J)=TEMP
            END IF
   30    CONTINUE
   40 CONTINUE
C
C   And finally the average taken.
C
      VMIN=(WORK(1)+WORK(2)+WORK(3)+WORK(4)+WORK(5))/5.0
      END
      SUBROUTINE DOEDGE(AVE,VAR,N,M,IC,JC,IB,A,V)
C
C      THIS SUBROUTINE IS ONLY APPLIED AT EDGES
C
C      THE OBJECT IS TO DEFINE THE DIRECTION OF THE EDGE
C      AND THEN USE THE MEAN AND VARIANCE APPROPRIATE TO
C      THE SIDE OF THE EDGE CONTAINING THE PIXEL AT (IC,JC)
C
      REAL AVE(N,M),VAR(N,M),D(3,3)
      IHW=(IB-1)/2
C
C   FIRST APPLY THE DIFFERENCE OPERATOR
C   TO THE MEANS AROUND IC,JC
C
      DO 200 J=1,3
         J1=(J-2)*IHW
         DO 100 I=1,3
            I1=(I-2)*IHW
            D(I,J)=ABS(AVE(IC+I1,JC+J1)-AVE(IC,JC))
  100    CONTINUE
  200 CONTINUE
C
C   NOW FIND THE DIRECTION OF THE EDGE
C
      DMAX=0.0
      DO 400 J=1,3
         DO 300 I=1,3
            IF (D(I,J).GE.DMAX) THEN
               DMAX=D(I,J)
               ID=I
               JD=J
            END IF
  300    CONTINUE
  400 CONTINUE
C
C   NOW PICK THE POINT APPROPRIATE TO THE "CORRECT SIDE OF THE EDGE
C
      ID=(2-ID)*IHW+IC
      JD=(2-JD)*IHW+JC
      A=AVE(ID,JD)
      V=VAR(ID,JD)
      END
