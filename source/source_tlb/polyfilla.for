C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     ***********************
C                     *                     *
C                     * Program   POLYFILLA *
C                     *                     *
C                     ***********************
C
C
C
C          CALLING SEQUENCE:-
C               POLYFILLA
C
C
C          FUNCTION:-
C               It allows the user to define a set of polygonal  shapes  by
C               using  the  cursor on an image displayed on the ARGS. These
C               are then stored as an array of 0s and 1s in an image of the
C               same size and shape as the image displayed on the ARGS.
C
C
C          USE:-
C               It creates a mask which may be used by an increasing number
C               of  ASPIC  programs. For example ADDMSK allows images to be
C               added  but  rejecting  all  points  defined  by   a   mask.
C               Multiplying the image by the mask can also be of value.
C
C
C         USER PARAMETERS:-
C
C         OUTPUT                              This is the name of  the  2-D
C                                             Starlink image which contains
C                                             the mask.
C
C
C         USE OF TRACKER-BALL BUTTONS:-
C
C         GREEN 1     Accept the cursor position and consider it to be  the
C                     last point in this polygon. Fill the polygon and move
C                     on to another one.
C
C         WHITE 2     Accept this position and draw a cross (if  the  first
C                     point) or join it to the previous point.
C
C         WHITE 3     Delete the previous point and continue.
C
C         RED   4     Accept this as the last point of  the  last  polygon.
C                     Fill the shape and exit from the program.
C
C
C
C
C
C
C         C D Pike                 RGO                             8-JAN-82
C
C
C
C
C--------------------------------------------------------------------------



      INTEGER PX(512),PY(512)
      INTEGER POUT,AXES(2),PSTORE
      REAL XPOS(2),YPOS(2)
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'

C
C   ALLOCATE ARGS
C
      CALL SRINIT (0,.FALSE.,STATUS)
      IF (STATUS.NE.0) THEN
          CALL WRERR ('NOARGS')
          GOTO 999
      ENDIF
*
*  OPEN AND READ ARGS DATABASE TO SEE IF IMAGE THERE
*
      CALL ARGS_RDIM(IX,IY,ISX,ISY,IS,JS,ISTAT)
      IF(ISTAT.NE.0)  THEN
         CALL WRUSER('NO IMAGE DISPLAYED',ISTAT)
         GO TO 999
      ENDIF
*
*  GET OUTPUT IMAGE FOR LOGICAL MASK
*


*
*  SWITCH ON THE CURSOR AND COLLECT THE POINTS
*
      IDLOC = 0
      IP = 0
      CALL ARGS_CUROP('1234','W')
      CALL ARGS_OVOP(8,'B')

   10 CALL ARGS_RDCUR('IMAG',ID,IB,UX,UY)
      IF(ID.EQ.0) GO TO 10

      IF(IDLOC.EQ.0)  THEN
         IDLOC = ID
         CALL ARGS_QDBI(ID,'PSIZX',1,AXES(1),I,ISTAT)
         CALL ARGS_QDBI(ID,'PSIZY',1,AXES(2),I,ISTAT)
         CALL GETDYN('STORE',FMT_SW,AXES(1)*AXES(2),PSTORE,ISTAT)
         IF(ISTAT.NE.ERR_NORMAL)  THEN
            CALL WRUSER('FAILED TO ALLOCATE SPARE ROOM',ISTAT)
            GO TO 999
         ENDIF
         CALL ZERO(%VAL(PSTORE),AXES)
      ELSE IF(ID.NE.IDLOC)  THEN
              GO TO 10
      ENDIF


*
*  DELETE LAST VERTEX IF BUTTON 3 PRESSED
*
      IF(IB.EQ.3.AND.IP.GT.1)  THEN
         CALL ARGS_S1('ZDI1','0000'X)
         CALL ARGS_POLYL(ID,2,XPOS,YPOS,ISTAT)
         CALL ARGS_S1('ZDI1','0100'X)
         IP = IP - 1
         GO TO 10
      ENDIF

      IP = IP + 1
      CALL ARGS_UTOP(ID,UX,UY,PX(IP),PY(IP),ISTAT)

*
*  DRAW CROSS FOR FIRST POINT OR JOIN WITH PREVIOUS
*
      IF(IP.EQ.1)  THEN
         CALL CROSS(ID,UX,UY)
      ELSE
      CALL ARGS_PTOU(ID,PX(IP-1),PY(IP-1),XPOS(1),YPOS(1),ISTAT)
      CALL ARGS_PTOU(ID,PX(IP),PY(IP),XPOS(2),YPOS(2),ISTAT)
      CALL ARGS_POLYL(ID,2,XPOS,YPOS,ISTAT)
      ENDIF

*
*  LOOP IF MORE POINTS OR CLOSE UP POLYGON AND FILL
*
      IF(IB.EQ.2.OR.IB.EQ.3)  GO TO 10

      IP = IP + 1
      PX(IP) = PX(1)
      PY(IP) = PY(1)

*
*  IF BUTTON 4 PRESSED SWITCH OFF CURSOR
*
      IF(IB.EQ.4)  CALL ARGS_CURCL

      CALL FILL(PX,PY,IP,%VAL(PSTORE),AXES,IDLOC)

*
*  IF BUTTON 1 PRESSED CLEAR COUNTERS AND START AGAIN
*
      IF(IB.EQ.1)  THEN
         IP = 0
         GO TO 10
      ELSE
         CALL ARGS_OVCL(8,.FALSE.)
      ENDIF

*
*  FINISHED
*
      ITRY = 0
   20 CALL WRIMAG('OUTPUT',FMT_R,AXES,2,POUT,ISTAT)
      IF(ISTAT.NE.ERR_NORMAL)  THEN
         CALL WRUSER(' BAD OUTPUT FRAME SPECIFICATION',ISTAT)
         CALL CNPAR('OUTPUT',ISTAT)
         ITRY = ITRY + 1
         IF(ITRY.GT.1) THEN
            GO TO 999
         ELSE
            CALL WRUSER(' ONE MORE TRY ONLY',ISTAT)
            GO TO 20
         ENDIF
      ENDIF
*
*  STORE MASK AS REAL ARRAY
*
      CALL STORE_OUT(%VAL(POUT),%VAL(PSTORE),AXES)

  999 CONTINUE
      CALL FRDATA(' ',ISTAT)
      CALL EXIT
      END

      SUBROUTINE STORE_OUT(OUT,STORE,AXES)
      INTEGER AXES(2)
      REAL OUT(AXES(1),AXES(2))
      INTEGER*2 STORE(AXES(1),AXES(2))

      DO 100 J=1,AXES(2)
      DO 100 I=1,AXES(1)
         OUT(I,J) = STORE(I,J)
  100 CONTINUE

      END


      SUBROUTINE ZERO(DATA,AXES)
      INTEGER AXES(2)
      INTEGER*2 DATA(AXES(1),AXES(2))

      DO J=1,AXES(2)
         DO I=1,AXES(1)

           DATA(I,J) = 0

         ENDDO
      ENDDO

      END

      SUBROUTINE FILL(XVERTS,YVERTS,NVERTS,DATA,AXES,IDLOC)

      INTEGER XVERTS(512),YVERTS(512),XSECTS(512),NVERTS,ENLINE,STLINE
      INTEGER AXES(2)
      INTEGER*2 DATA(AXES(1),AXES(2))
      INTEGER PIX(2),PIY(2)
      REAL XPOS(2),YPOS(2)
      LOGICAL INSIDE


*
*   FIND START LINE AND END LINE FOR SMALLEST BOX CONTAINING POLYGON
*
      STLINE = 512
      ENLINE = 1

      DO 150 I=1,NVERTS
      IF(YVERTS(I).GT.ENLINE)  ENLINE = YVERTS(I)
      IF(YVERTS(I).LT.STLINE)  STLINE = YVERTS(I)
  150 CONTINUE

      ENLINE = MIN0(512,ENLINE+1)
      STLINE = MAX0(1,STLINE-1)

*
*  FIND ALL THE INTERSECTIONS OF THE LINE NUMBER 'LINE' WITH THE
*  LINE SEGMENTS OF THE POLYGON
*

      DO 200 LINE=STLINE,ENLINE

      CALL XSECT(XVERTS,YVERTS,NVERTS,LINE,XSECTS,NXSECT)

      IF(NXSECT.LE.1)  GO TO 200
*
*  IF REGIONS BETWEEN INTERSECTIONS ARE INTERIOR POINTS FILL THEM IN
*  ELSE GO ON.
*
      DO 180 I=2,NXSECT
      IX = (XSECTS(I-1) + XSECTS(I))/2

      IF(.NOT.INSIDE(XVERTS,YVERTS,NVERTS,IX,LINE))  GO TO 180

      NPIXEL = 1 + XSECTS(I) - XSECTS(I-1)

*
*  SET MASK IN OUTPUT
*
      DO IPIX=XSECTS(I-1),XSECTS(I)
      DATA(IPIX,LINE) = 1
      ENDDO

*
*  DRAW LINE ON ARGS
*
      PIX(1) = XSECTS(I-1)
      PIY(1) = LINE
      PIY(2) = LINE
      PIX(2) = PIX(1) + NPIXEL - 1

      CALL ARGS_PTOU(IDLOC,PIX(1),PIY(1),XPOS(1),YPOS(1),ISTAT)
      CALL ARGS_PTOU(IDLOC,PIX(2),PIY(2),XPOS(2),YPOS(2),ISTAT)

      CALL ARGS_POLYL(IDLOC,2,XPOS,YPOS,ISTAT)


  180 CONTINUE
  200 CONTINUE

      END



      SUBROUTINE XSECT(XVERTS,YVERTS,NVERTS,LINE,XSECTS,NXSECT)

      INTEGER XVERTS(512),YVERTS(512),XSECTS(512),NVERTS,LINE,NXSECT
      INTEGER X1,X2,Y1,Y2
      LOGICAL SORTED


      NXSECT = 0

      DO 100 I=2,NVERTS

      Y1 = YVERTS(I-1)
      Y2 = YVERTS(I)
      IF(Y1.EQ.Y2)  GO TO 100
      IF(LINE.LT.MIN0(Y1,Y2))  GO TO 100
      IF(LINE.GT.MAX0(Y1,Y2))  GO TO 100
      NXSECT = NXSECT + 1
      X1 = XVERTS(I-1)
      X2 = XVERTS(I)
      XSECTS(NXSECT) = 0.5 + REAL(X1) +
     *                 REAL(X2-X1)*REAL(LINE-Y1)/REAL(Y2-Y1)

  100 CONTINUE

*
* NOW SORT THEM
*

  150 CONTINUE

      SORTED = .TRUE.
      DO 120 I=2,NXSECT
      IF(XSECTS(I-1).LE.XSECTS(I))  GO TO 120
      ITEMP = XSECTS(I-1)
      XSECTS(I-1) = XSECTS(I)
      XSECTS(I) = ITEMP
      SORTED = .FALSE.
  120 CONTINUE

      IF(.NOT.SORTED)  GO TO 150

      END




      LOGICAL FUNCTION INSIDE(XVERTS,YVERTS,NVERTS,XPOS,YPOS)
      INTEGER XVERTS(512),YVERTS(512),NVERTS,XPOS,YPOS
*
*  USES CAUCHY'S INTEGRAL THEOREM TO TEST FOR INSIDE. CONNECT TEST
*  POINT TO EACH VERTEX IN TURN AND IF THE SUM OF THE ROTATION ANGLES
*  IS ZERO THEN THE POINT IS EXTERIOR

      INSIDE = .FALSE.
      IF(NVERTS.LT.3)   RETURN
      ANGSUM = 0.0

      DO 100 I=2,NVERTS

      ANGSUM = ANGSUM + ANGLE(XPOS,YPOS,XVERTS(I-1),YVERTS(I-1),
     *                        XVERTS(I),YVERTS(I))

  100 CONTINUE

      IF(ABS(ANGSUM).GT.3.1416)  INSIDE = .TRUE.

      END



      REAL FUNCTION ANGLE(X0,Y0,X1,Y1,X2,Y2)
      INTEGER X0,X1,X2,Y0,Y1,Y2
      REAL CROSS,DOT,XX1,XX2,YY1,YY2

*
*  TO FIND THE ROTATION ANGLE BETWEEN (X1,Y1) AND (X2,Y2) RELATIVE
*  TO THE CENTRAL POINT (X0,Y0). IT IS CALCULATED USING THE RATIO OF
*  THE CROSS PRODUCT(PROP TO SIN(ANG)) TO THE DOT PRODUCT(PROP TO COS)
*  THE SENSE +/- IS IMPORTANT
*

      XX1 = X1 - X0
      YY1 = Y1 - Y0
      XX2 = X2 - X0
      YY2 = Y2 - Y0

      CROSS = XX1*YY2 - XX2*YY1
      DOT   = XX1*XX2 + YY1*YY2

*
*  CHECK FOR RIGHT ANGLE
*
      IF(ABS(DOT).GT.0.00001)  GO TO 40
      ANGLE = 1.5708
      GO TO 50
   40 CONTINUE

      ANGLE = ATAN(ABS(CROSS/DOT))
      IF(DOT.LT.0.0)  ANGLE = 3.1416 - ANGLE
   50 CONTINUE

      IF(CROSS.LT.0.0)  ANGLE = -ANGLE

      END

      SUBROUTINE CROSS (ID,X,Y)
C
C   DRAW CROSS ON ARGS AT POSITION (USER UNITS) (X,Y) OF WIDTH 5 PIXELS
C   (ID IS ID OF IMAGE)
C
      INTEGER PX,PY,STATUS
      REAL X,Y,UX,UY,DX,DY,XVAL(2),YVAL(2)
      CALL ARGS_UTOP (ID,X,Y,PX,PY,STATUS)
      CALL ARGS_PTOU (ID,PX+2,PY+2,UX,UY,STATUS)
      DX = UX - X
      DY = UY - Y
      XVAL(1) = X - DX
      YVAL(1) = Y
      XVAL(2) = X + DX
      YVAL(2) = Y
      CALL ARGS_POLYL (ID,2,XVAL,YVAL,STATUS)
      XVAL(1) = X
      YVAL(1) = Y - DY
      XVAL(2) = X
      YVAL(2) = Y + DY
      CALL ARGS_POLYL (ID,2,XVAL,YVAL,STATUS)
C
      END
