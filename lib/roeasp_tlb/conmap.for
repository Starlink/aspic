      SUBROUTINE CONMAP (A,ISIZE,JSIZE,ISTART,IFIN,JSTART,JFIN
     &                  ,BASE,STEP,NOCON,CTRLC)
C+
C     CONMAP
C
C  CONTOUR PLOTTING ROUTINE
C
C     Given      (arguments)
C     A      -    image array
C     ISIZE  -    X-dimension of image
C     JSIZE  -    Y-dimension of image
C     ISTART -    starting X coordinate to be contoured
C     IFIN   -    finishing X coordinate
C     JSTART -    starting Y coordinate
C     JFIN   -    finishing Y coordinate
C     BASE   -    lowest intensity level to be contoured
C     STEP   -    intensity interval between contours
C     NOCON  -    number of contour levels required
C     CTRLC  -    .FALSE. on entry
C
C     Returned   (arguments)
C     CTRLC  -    reset to .FALSE. before routine exits
C                 CTRLC allows user interrupt by control-C
C
C    Originator A.Pickup (ROE)
C    B.D.Kelly/ROE/1981
C
C-
      REAL A(ISIZE,JSIZE)
      LOGICAL*2 CTRLC
      DIMENSION B(5),CX(4),CY(4),DX(4),DY(4),
     1X(5),Y(5),D(4)
      DATA CX/0.0,1.0,1.0,0.0/,CY/0.0,0.0,1.0,1.0/
     1,DX/1.0,0.0,-1.0,0.0/,DY/0.0,1.0,0.0,-1.0/
C
      IF(STEP.EQ.0.0)STEP=1.0
      IF(STEP.LT.0.0) THEN
        STEP=-STEP
        BASE=BASE-STEP*FLOAT(NOCON-1)
      ENDIF
      HTMAX=BASE+STEP*FLOAT(NOCON-1)
C
C  TAKE (2X2) SQUARES OF GRIDPOINTS FROM ARRAY A
C
      DO 120 LY=JSTART,JFIN-1
      BY=FLOAT(LY)
C
      DO 110 KX=ISTART,IFIN-1
      BX=FLOAT(KX)
C
C  B(1-4) CONTAIN CORNERS OF (2X2) GRID
C
      B(1)=A(KX,LY)
      B(2)=A(KX+1,LY)
      B(3)=A(KX+1,LY+1)
      B(4)=A(KX,LY+1)
C
C  BMIN=LOWEST CORNER VALUE
C  BMAX=HIGHEST CORNER VALUE
C
      BMIN=AMIN1(B(1),B(2),B(3),B(4))
      IF (BMIN.GT.HTMAX) GO TO 110
      BMAX=AMAX1(B(1),B(2),B(3),B(4))
      IF (BMAX.LT.BASE) GO TO 110
      B(5)=B(1)
C
C   STEP THROUGH CONTOUR HEIGHTS UP TO HTMAX.
C
      DO 100 JCONT=1,NOCON
      HT=BASE+(JCONT-1)*STEP
      IF(HT.LE.BMIN)GOTO100
      IF(HT.GT.BMAX)GOTO110
C
C  INTERPOLATE INTERSECTION POINTS OF CONTOUR
C  WITH BORDERS OF CURRENT (2X2) SQUARE
C
      IT=0
      DO N=1,4
        IF((B(N).LT.HT).AND.(B(N+1).GE.HT)
     &   .OR.(B(N).GE.HT).AND.(B(N+1).LT.HT)) THEN
          IT=IT+1
          P=(HT-B(N))/(B(N+1)-B(N))
          X(IT)=BX+CX(N)+DX(N)*P-0.5
          Y(IT)=BY+CY(N)+DY(N)*P-0.5
        ENDIF
      ENDDO
 
C
C  JOIN INTERSECTION POINTS IN CURRENT (2X2) SQUARE
C
C
C  JUST TWO INTERSECTIONS : THEY ARE JOINED
C
      IF(IT.EQ.2) THEN
        CALL DRACON(X,Y,1,2,1,2)
        GO TO 100
      ENDIF
C
C   FOUR INTERSECTIONS
C   ONLY TWO NON-CROSSING LINES WILL BE DRAWN.
C   TESTS ARE MADE TO DETERMINE PAIRING OF INTERSECTIONS.
C   TEST ONE.   PAIR INTERSECTIONS ONE WAY IF RESULTS
C   IN TOTAL LINE-LENGTH SAVING BY A FACTOR OF 4 OR
C   MORE OVER OTHER PAIRING.
C
      X(5)=X(1)
      Y(5)=Y(1)
      DO N=1,4
        DA=X(N)-X(N+1)
        DB=Y(N)-Y(N+1)
        D(N)=SQRT(DA*DA+DB*DB)
      ENDDO
 
      IF((D(1)+D(3)).GT.4.0*(D(2)+D(4))) THEN
        CALL DRACON(X,Y,1,4,3,2)
        GO TO 100
      ENDIF
 
      IF((D(2)+D(4)).GT.4.0*(D(1)+D(3))) THEN
        CALL DRACON(X,Y,1,2,4,3)
        GO TO 100
      ENDIF
C
C   TEST TWO.
C  THE (4X4) SQUARE CENTRED ON THE (2X2) SQUARE
C  IS EXAMINED : IT=NO OF GRIDPOINTS IN THE (4X4)
C  SQUARE; IC=NO OF THESE GRIDPOINTS BELOW CURRENT
C  CONTOUR LEVEL.
C IF MAJORITY OF GRIDPOINTS ARE ABOVE CONTOUR LEVEL,
C  TWO LINES CUT DIAGONAL BETWEEN TWO HIGHEST
C  GRIDPOINTS OF (2X2) SQUARE; OTHERWISE TWO
C  LINES CUT DIAGONAL BETWEEN TWO LOWEST GRIDPOINTS.
C
      IC=0
      IT=0
      DO N=1,4
        K=KX+N-2
        IF (K.GE.ISTART .AND. K.LE.IFIN) THEN
          DO M=1,4
            L=LY+M-2
            IF (L.GE.JSTART .AND. L.LE.JFIN) THEN
              IF (A(K,L).LT.HT) IC=IC+1
              IT=IT+1
            ENDIF
          ENDDO
        ENDIF
      ENDDO
 
      IF(IC.EQ.IT/2) THEN
        IF((D(1)+D(3)).LT.(D(2)+D(4))) THEN
          CALL DRACON(X,Y,1,2,4,3)
        ELSE
          CALL DRACON(X,Y,1,4,3,2)
        ENDIF
      ELSE IF(IC.LT.IT/2) THEN
        IF(B(1).GT.B(2)) THEN
          CALL DRACON(X,Y,1,4,3,2)
        ELSE
          CALL DRACON(X,Y,1,2,4,3)
        ENDIF
      ELSE IF(IC.GT.IT/2) THEN
        IF(B(1).LT.B(2)) THEN
          CALL DRACON(X,Y,1,4,3,2)
        ELSE
          CALL DRACON(X,Y,1,2,4,3)
        ENDIF
      ENDIF
  100 CONTINUE
C
C     NEXT SQUARE
C
  110 CONTINUE
C
C     ALLOW USER INTERRUPT
C
      IF(CTRLC) THEN
        CTRLC=.FALSE.
        RETURN
      ENDIF
  120 CONTINUE
      RETURN
      END
