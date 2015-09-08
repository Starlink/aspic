	subroutine crb_slice
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *******************
C                     *                 *
C                     * Program   SLICE *
C                     *                 *
C                     *******************
C
C
C
C          CALLING SEQUENCE:-
C               SLICE
C
C
C          FUNCTION:-
C               It allows cursor definition  of  two  points  in  an  image
C               displayed  on the ARGS and display of the slice through the
C               image with the two points as end points.  Interpolation  is
C               used  to ensure that the spacing of samples in the slice is
C               always at the pixel spacing, irrespective of the angle.
C
C
C          USE:-
C               Obviously - to look at sections in any direction through an
C               image.  The resulting slice may be stored as a 1-D Starlink
C               image.
C
C
C
C         USER PARAMETERS:-
C
C         INPUT                               This is the input image -  it
C                                             ought  to  be  visible on the
C                                             ARGS.
C
C         OUTPUT                              This is used, if present,  as
C                                             the   name   for  the  stored
C                                             slice.
C
C
C         USE OF TRACKER-BALL BUTTONS:-
C
C         GREEN 1     This may be used to pick two points from one  of  the
C                     images  displayed  on  the  ARGS. Both points must be
C                     from the same image, and there must be at  least  one
C                     image displayed.
C
C         WHITE 2     If this button is pressed after the first point
C                     has been entered then the sampling line that
C                     would result from these two points is displayed.
C                     To accept that line, press button 1 again otherwise
C                     move the cursor and hit button 2 to try again.
C
C         WHITE 3     Same as 2.
C
C         RED   4     This forces unconditional exit from the program.
C
C
C
C
C
C
C
C
C
C         C D Pike - W F Lupton    RGO                            13-JAN-82
C         M D Lawden               RAL                            26-NOV-85
C
C
C--------------------------------------------------------------------------



C   WRITTEN BY C D PIKE (AND OTHERS) AT RGO ON 23/2/81
C   THIS VERSION BY WFL 1/7/81 TO DEMONSTRATE ARGS DATABASE
C   EXTENSIVELY PRUNED BY WFL TO DEMONSTRATE ARGS OVERLAYS SEP 1981
C
      INTEGER PIN,STATUS,AXIN(2),POUT,AXOUT(2)
      REAL LINDAT(0:1023,2),LINE(0:1023)
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
C   ALLOCATE ARGS AND RESET VSR
C
      CALL SRINIT (0,.FALSE.,STATUS)
      IF (STATUS.NE.0) THEN
          CALL WRERR ('NOARGS')
          GOTO 999
      ENDIF
      CALL ARGS_VSRRST
C
C   GET INPUT FRAME
C
      CALL RDIMAG('INPUT',FMT_R,2,AXIN,I,PIN,STATUS)
      IF (STATUS.NE.ERR_NORMAL) THEN
          CALL WRERR ('BADIN')
          GOTO 999
      ENDIF
C
C   CHECK THAT AT LEAST ONE IMAGE IS DISPLAYED
C
      CALL ARGS_NUMIM (IDMAX)
      IF (IDMAX.EQ.0) THEN
          CALL WRERR ('NOIMS')
      ELSE
C
C       DEFINE LINE
C
          CALL LINDEF(ID,X1,X2,Y1,Y2,STATUS)
          IF (STATUS.EQ.1) THEN
              CALL WRERR ('ERRCUR')
          ELSE IF (STATUS.EQ.0) THEN
              IF (ABS(X2-X1).LT.1E-18.AND.ABS(Y2-Y1).LT.1E-18) THEN
                  CALL WRERR ('ZEROLENG')
              ELSE
                  CALL LINSET(X1,X2,Y1,Y2,LINDAT,NPTS)
                  CALL INTERPOLATE(ID,LINDAT,NPTS,%VAL(PIN),AXIN(1),
     :            AXIN(2),LINE)
                  CALL LINPLOT(LINE,NPTS)
                  CALL WRIMAG('OUTPUT',FMT_R,NPTS,1,POUT,STATUS)
                  IF (STATUS.EQ.ERR_NORMAL) THEN
                      CALL ASP_COPCON(FMT_R,FMT_R,LINE,%VAL(POUT),NPTS,
     :                STATUS)
                  ENDIF
              ENDIF
          ENDIF
      ENDIF
C
C   TIDY UP AND GO HOME
C
  999 CALL FRDATA(' ',STATUS)
      CALL ARGS_OVCL (8,.FALSE.)
      END
C
      SUBROUTINE LINDEF(IDLOC,X1,X2,Y1,Y2,STATUS)
C
C   USE CURSOR TO SELECT TWO POINTS
C
      INTEGER STATUS,PX,PY
      REAL UX(2),UY(2)
C
C   INITIALISE CURSOR AND ENABLE OVERLAYS
C
      CALL ARGS_CUROP ('1234','W')
      CALL ARGS_OVOP (8,'G')
C
C   IPT IS USED TO COUNT THE POINTS SELECTED
C
      IPT=0
      IDLOC = 0
  998 CONTINUE
      DO WHILE (IPT.LT.2)
          CALL ARGS_RDCUR('IMAG',ID,IB,X,Y)
C
C       IF IMAGE WAS LOCATED THEN STORE POSITION
C
          IF (IB.EQ.0) THEN
              STATUS = 1
              GOTO 999
          ELSE IF (IB.EQ.4) THEN
              STATUS = 2
              GOTO 999
          ELSE IF (ID.GT.0) THEN
              IF (IDLOC.EQ.0) THEN
                  IDLOC = ID
              ENDIF
              IF (ID.EQ.IDLOC) THEN
                  IF (IPT.EQ.0) THEN
                      CALL CROSS (ID,X,Y)
                      CALL ARGS_UTOP (ID,X,Y,PX,PY,STATUS)
                      XU = X
                      YU = Y
                      X1 = PX
                      Y1 = PY
                  ELSE IF (IB.GT.1) THEN
                          UX(1) = XU
                          UX(2) = X
                          UY(1) = YU
                          UY(2) = Y
                          CALL ARGS_CLS(8)
                          CALL ARGS_OVCL(8,.FALSE.)
                          CALL ARGS_OVOP(8,'G')
                          CALL CROSS(ID,XU,YU)
                          CALL ARGS_POLYL(ID,2,UX,UY,STATUS)
                          GO TO 998
                       ELSE
                          CALL ARGS_UTOP (ID,X,Y,PX,PY,STATUS)
                          X2 = PX
                          Y2 = PY
                  ENDIF
                  IPT = IPT + 1
                  CALL ARGS_CURC ('G')
              ENDIF
          ENDIF
      ENDDO
      STATUS = 0
C
C   REMOVE CURSOR
C
999   CALL ARGS_CURCL
      END
C
      SUBROUTINE INTERPOLATE(ID,LINDAT,NPTS,DATA,NX,NY,LINE)
      INTEGER STATUS
      REAL LINDAT(0:1023,2),LINE(0:1023),DATA(0:NX-1,0:NY-1),XVAL(2),
     :    YVAL(2)
C
C   SET COLOUR OF LINE AND PLOT IT
C
      XVAL(1) = LINDAT(0,1)
      YVAL(1) = LINDAT(0,2)
      XVAL(2) = LINDAT(NPTS-1,1)
      YVAL(2) = LINDAT(NPTS-1,2)
      CALL ARGS_POLYL (ID,2,XVAL,YVAL,STATUS)
C
C   LINDAT CONTAINS THE X,Y PAIRS OF POINTS TO BE INTERPOLATED
C
      DO I=0,NPTS-1
          IX1 = LINDAT(I,1)
          IY1 = LINDAT(I,2)
          IX2 = IX1 + 1
          IY2 = IY1 + 1
C
C   GET THE FOUR SURROUNDING POINTS IN THE DATA ARRAY
C
         VAL1 = DATA(IX1,IY1)
         VAL2 = DATA(IX2,IY1)
         VAL3 = DATA(IX1,IY2)
         VAL4 = DATA(IX2,IY2)
C
C   F & G ARE THE FRACTIONAL PIXEL DISPLACEMENTS OF THE
C   INTERPOLATION POINT
C
         F = LINDAT(I,1) - IX1
         G = LINDAT(I,2) - IY1
C
C   ON EXIT, 'LINE' WILL CONTAIN THE ARRAY OF INTERPOLATED VALUES
C   READY FOR PLOTTING. BILINEAR INTERPOLATION IS USED.
C
         LINE(I) = F*(VAL2-VAL1) + F*G*(VAL1+VAL4-VAL2-VAL3)
     1             + G*(VAL3-VAL1) + VAL1
      ENDDO
C
C
      END
C
C
      SUBROUTINE LINSET(X1,X2,Y1,Y2,LINDAT,NPTS)
C
      REAL LINDAT(0:1023,2)
C
      PARAMETER (RAD=57.29578)
C
C   CALCULATE THE X,Y POSITIONS OF POINTS AT RADII OF
C   INTEGRAL PIXEL SPACINGS FROM THE FIRST POINT
C
      THETA = ATAN2 (Y2-Y1,X2-X1)
      CT = COS(THETA)
      ST = SIN(THETA)
      SEP = SQRT ((X2-X1)**2 + (Y2-Y1)**2)
      NPTS = NINT (SEP)
C
      DO I=0,NPTS-1
        LINDAT(I,1) = I*CT + X1
        LINDAT(I,2) = I*ST + Y1
      ENDDO
      END
C
      SUBROUTINE LINPLOT(LINE,NPTS)
      REAL LINE(0:NPTS-1),AX(2),AY(2)
C
C   GET SCALING PARMS
C
      FMAX = LINE(0)
      FMIN = 0.0
      DO 100 I=0,NPTS
      IF(LINE(I).GT.FMAX)  FMAX = LINE(I)
      IF(LINE(I).LT.FMIN)  FMIN = LINE(I)
  100 CONTINUE
C
C   PREPARE FOR PLOTTING
C
      AX(1) = 0.0
      AX(2) = REAL(NPTS-1)
      AY(1) = FMIN
      AY(2) = 1.2*FMAX
      CALL JBDEV('GKS_15_0')
      CALL JBAXES(AX,2,20.,5HPIXEL,5,AY,2,15.,1H ,1)
C
C   PLOT DATA
C
      DO 200 I=0,NPTS-1
      CALL JOIN PT(REAL(I),LINE(I))
  200 CONTINUE
C
      END
C
      SUBROUTINE JBNEW(X,Y,WIDTH,HEIGHT,N)
C
C   DUPE SIMPLEPLOT INTO NOT CLEARING THE ARGS SCREEN!
C
      RETURN
      END
C
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
