C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *********************
C                     *                   *
C                     * Program   FCSCALE *
C                     *                   *
C                     *********************
C
C
C
C          CALLING SEQUENCE:-
C               FCSCALE
C
C
C          FUNCTION:-
C               It reads in three Starlink images, computes and plots their
C               histograms and  allows the user to define six points, which
C               must consist of  two  sets  of  three  points.  These  will
C               normally  be  the  location  of the peak of each histogram,
C               going from bottom of the screen to the top, and likewise  a
C               sensible  upper limit to the signal in each histogram.  The
C               data  are  rescaled  and  the   new   histograms   plotted.
C               Interaction   is   via   the  cursor  on  any GKS device.
C
C
C          USE:-
C               It was originally designed to allow data which had not been
C               properly  calibrated  and  reduced to a common scale, to be
C               used to create a False Colour display. It may also be  used
C               before mosaicing images and so on.
C
C
C
C         USER PARAMETERS:-
C
C         INA                                 First  input   2-D   Starlink
C                                             image
C
C         INB                                 Second input image.
C
C         INC                                 Third input image.
C
C         OUTA                                The  output  Starlink   image
C                                             corresponding to INA.
C
C         OUTB                                The second output image.
C
C         OUTC                                The third output image.
C
C         DEVICE                              The GKS device, which must
C                                             have a cursor.
C
C         K F Hartley              RGO                            15-MAR-82
C
C
C--------------------------------------------------------------------------



C      Written by K F HARTLEY at RGO on 12/12/80
C      Revised to use GKS by KFH at RGO on 13/11/84
C
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
      INTEGER AX(2),PA,PB,PC,STATUS
      INTEGER AXA(2),AXB(2),AXC(2)
      INTEGER PAO,PBO,PCO
C
C     READ THE INPUT FRAMES
C
      CALL RDIMAG('INA',FMT_R,2,AXA,I,PA,STATUS)
      IF (STATUS.NE.0.OR.I.NE.2) CALL EXIT
      CALL RDIMAG('INB',FMT_R,2,AXB,I,PB,STATUS)
      IF (STATUS.NE.0.OR.I.NE.2) CALL EXIT
      CALL RDIMAG('INC',FMT_R,2,AXC,I,PC,STATUS)
      IF (STATUS.NE.0.OR.I.NE.2) CALL EXIT
C
C      NOW CHECK AXES AGREE
C
      IF (AXA(1).NE.AXB(1).OR.AXA(1).NE.AXC(1).
     1    OR.AXA(2).NE.AXB(2).OR.AXA(2).NE.AXC(2)) THEN
         CALL WRUSER ('AXES DO NOT AGREE',STATUS)
         CALL EXIT
      END IF
C
C      USE THE COMMON DIMENSIONS FOR THE OUTPUT FRAMES
C
      AX(1)=AXA(1)
      AX(2)=AXA(2)
C
C
C      NOW SET UP THE CORRESPONDING OUTPUT FRAMES
C
      CALL WRIMAG('OUTA',FMT_R,AX,2,PAO,STATUS)
      IF (STATUS.NE.0) CALL EXIT
      CALL WRIMAG('OUTB',FMT_R,AX,2,PBO,STATUS)
      IF (STATUS.NE.0) CALL EXIT
      CALL WRIMAG('OUTC',FMT_R,AX,2,PCO,STATUS)
      IF (STATUS.NE.0) CALL EXIT
C
C      NOW DO THE WORK
C
      CALL HISTSCALE(%VAL(PA),%VAL(PB),%VAL(PC),AX(1),AX(2),
     1           %VAL(PAO),%VAL(PBO),%VAL(PCO))
C
C      FINALLY TIDY UP AND GO HOME
C
      CALL FRDATA(' ',STATUS)
      CALL EXIT
      END
      SUBROUTINE HISTSCALE(A,B,C,N,M,AO,BO,CO)
C
C      THIS ROUTINE ORGANIZES THE MATCHING
C
C      A,B,C ARE THE INPUT FRAMES
C      AO,BO,CO ARE THE CORRESPONDING OUTPUT FRAMES
C      ALL OF DIMENSION N BY M
C
      REAL A(N,M),B(N,M),C(N,M)
      REAL AO(N,M),BO(N,M),CO(N,M)
      REAL MIN,MAX
      CHARACTER*20 DEV
      DEV = 'ARGS'
C
C      Get device and open it.
C
      CALL RDKEYC('DEVICE',.TRUE.,1,DEV,I,ISTAT)
      CALL SGS_OPEN(DEV,IZ,ISTAT)
      IF (ISTAT.NE.0) THEN
         CALL WRUSER('Failed to open that device',ISTAT)
         RETURN
      END IF
      CALL SGS_SW(0.0,256.0,0.0,1.0,ISTAT)
C
C     FIND THE MINIMUM AND MAXIMUM VALUES FOR ALL THREE ARRAYS
C
      MIN=A(1,1)
      MAX=A(1,1)
      DO 200 J=1,M
         DO 100 I=1,N
            IF (A(I,J).GT.MAX) MAX=A(I,J)
            IF (B(I,J).GT.MAX) MAX=B(I,J)
            IF (C(I,J).GT.MAX) MAX=C(I,J)
            IF (A(I,J).LT.MIN) MIN=A(I,J)
            IF (B(I,J).LT.MIN) MIN=B(I,J)
            IF (C(I,J).LT.MIN) MIN=C(I,J)
  100    CONTINUE
  200 CONTINUE
C
C      IF MIN=MAX THE ARRAYS ARE ALL CONSTANT ( = 0 ? )
C
      IF (MAX.EQ.MIN) THEN
        CALL WRUSER('MAX = MIN ... TOUGH!',ISTAT)
        RETURN
      END IF
      CALL WRUSER ('USE CURSOR TO SELECT THE "MODES" ',ISTAT)
      CALL WRUSER ('START WITH THE BOTTOM ONE AND WORK UP',ISTAT)
      CALL WRUSER ('THEN REPEAT FOR THE CUTOFFS',ISTAT)
C
C      The zones are defined so that each histogram occupies
C      60% of the screen height , and is 20% higher than the
C      previous one
C
C      HIST1 COMPUTES THE HISTOGRAM USING THE SAME BINS FOR ALL THREE
C      ARRAYS .
C
      CALL SGS_ZONE(0.0,256.0,0.0,0.6,IZ1,ISTAT)
      CALL HIST1(A,N,M,MIN,MAX)
      CALL SGS_RELZ(IZ1)
      CALL SGS_SELZ(IZ,ISTAT)
      CALL SGS_ZONE(0.0,256.0,0.2,0.8,IZ2,ISTAT)
      CALL HIST1(B,N,M,MIN,MAX)
      CALL SGS_RELZ(IZ2)
      CALL SGS_SELZ(IZ,ISTAT)
      CALL SGS_ZONE(0.0,256.0,0.4,1.0,IZ3,ISTAT)
      CALL HIST1(C,N,M,MIN,MAX)
      CALL SGS_RELZ(IZ3)
      CALL SGS_SELZ(IZ,ISTAT)
      CALL SGS_SW(0.0,256.0,0.0,1.0,ISTAT)
C
C      USE CURSOR TO PICK 2 SETS OF THREE CORRESPONDING POINTS
C
      CALL SGS_REQCU(X1,Y1,NBUT)
      CALL SGS_REQCU(X2,Y2,NBUT)
      CALL SGS_REQCU(X3,Y3,NBUT)
      CALL SGS_REQCU(XX1,Y1,NBUT)
      CALL SGS_REQCU(XX2,Y2,NBUT)
      CALL SGS_REQCU(XX3,Y3,NBUT)
C
C      FORM THE (AVERAGE) REFERENCE POINTS
C      AND SCALE TO REAL COUNTS FROM "HISTOGRAM" SPACE
C
      F=(MAX-MIN)/256.0
      X1=X1*F+(MIN)
      X2=X2*F+(MIN)
      X3=X3*F+(MIN)
      XX1=XX1*F+(MIN)
      XX2=XX2*F+(MIN)
      XX3=XX3*F+(MIN)
      X=(X1+X2+X3)/3.0
      XX=(XX1+XX2+XX3)/3.0
C
C      THREE LINEAR TRANSFORMATIONS ARE NOW COMPUTED.
C      EACH OF THE FORM
C         NEW = A * OLD + B
C      WHICH MAP X1 TO X AND XX1 TO XX
C
      A1=(XX-X)/(XX1-X1)
      A2=(XX-X)/(XX2-X2)
      A3=(XX-X)/(XX3-X3)
      B1=X-A1*X1
      B2=X-A2*X2
      B3=X-A3*X3
C
C     THEN APPLY THE TRANSFORMATIONS
C
      DO 400 J=1,M
         DO 300 I=1,N
            AO(I,J)=A(I,J)*A1+B1
            BO(I,J)=B(I,J)*A2+B2
            CO(I,J)=C(I,J)*A3+B3
  300    CONTINUE
  400 CONTINUE
C
C      FINALLY (AS A CHECK) RECALCULATE AND DISPLAY THE HISTOGRAMS
C      OF THE TRANSFORMED HISTOGRAMS
C
      MIN=AO(1,1)
      MAX=AO(1,1)
      DO 600 J=1,M
         DO 500 I=1,N
            IF (AO(I,J).GT.MAX) MAX=AO(I,J)
            IF (BO(I,J).GT.MAX) MAX=BO(I,J)
            IF (CO(I,J).GT.MAX) MAX=CO(I,J)
            IF (AO(I,J).LT.MIN) MIN=AO(I,J)
            IF (BO(I,J).LT.MIN) MIN=BO(I,J)
            IF (CO(I,J).LT.MIN) MIN=CO(I,J)
  500    CONTINUE
  600 CONTINUE
      CALL SGS_ICURW(IWK)
      CALL GKS_CLRWK(IWK)
      CALL SGS_ZONE(0.0,256.0,0.0,0.6,IZ1,ISTAT)
      CALL HIST1(AO,N,M,MIN,MAX)
      CALL SGS_RELZ(IZ1)
      CALL SGS_SELZ(IZ,ISTAT)
      CALL SGS_ZONE(0.0,256.0,0.2,0.8,IZ2,ISTAT)
      CALL HIST1(BO,N,M,MIN,MAX)
      CALL SGS_RELZ(IZ2)
      CALL SGS_SELZ(IZ,ISTAT)
      CALL SGS_ZONE(0.0,256.0,0.4,1.0,IZ3,ISTAT)
      CALL HIST1(CO,N,M,MIN,MAX)
      CALL SGS_CLOSE
      RETURN
      END
      SUBROUTINE HIST1(DAT,N,M,MIN,MAX)
C
C      HIST1 TAKES AN REAL ARRAY DAT OF DIMENSION N BY M
C      AND COMPUTES A HISTOGRAM USING BINS FROM 0 TO 256
C      SO THAT BIN 0 CORRESPONDS TO MIN AND 256 TO MAX
C      MIN AND MAX ARE TAKEN FROM THE CALLING SEGMENT
C
      INTEGER HIST(0:256)
      REAL DAT(N,M),MIN,MAX
C
C      EACH BIN IN THE HISTOGRAM IS "F" SAMPLES WIDE
C
      F=256.0/(MAX-MIN)
C
C      FIRST ZEROIZE THE HISTOGRAM
C
      DO 10 I=0,256
         HIST(I)=0
   10 CONTINUE
C
C      THEN FORM THE HISTOGRAM
C
      DO 200 J=1,M
         DO 100 I=1,N
            IX=IFIX((DAT(I,J)-MIN)*F)
            IF (IX.LT.0) IX=0
            IF (IX.GT.256) IX=256
            HIST(IX)=HIST(IX)+1
  100    CONTINUE
  200 CONTINUE
C
C      THE HISTOGRAM IS GOING TO BE PLOTTED AS LOG(HIST) SO...
C      ... FIND LOG OF MAX HISTOGRAM VALUE AND DEFINE THE WINDOW
C      SO THAT IT FITS ON THE VUPORT
C
      MAXH=0
      DO 300 I=0,256
         IF (HIST(I).GT.MAXH) MAXH=HIST(I)
  300 CONTINUE
      YMAX=ALOG10(FLOAT(MAXH)+1.0)
      CALL SGS_SW(0.0,256.0,0.0,YMAX,ISTAT)
C
C      FINALLY PLOT THE HISTOGRAM
C
      CALL SGS_BPOLY(0.0,0.0)
      DO 400 I=0,256
         X=FLOAT(I)
         Y=ALOG10(FLOAT(HIST(I))+1.0)
         CALL SGS_APOLY(X,Y)
  400 CONTINUE
      CALL SGS_OPOLY
      RETURN
      END
