C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     ********************
C                     *                  *
C                     * Program   SECTOR *
C                     *                  *
C                     ********************
C
C
C
C          CALLING SEQUENCE:-
C               SECTOR
C
C
C          FUNCTION:-
C               It may be used to define a sector of a circle by using  the
C               cursor  on the ARGS, compute and display the radial profile
C               within that sector, and optionally store the profile  as  a
C               1-D frame.
C
C
C          USE:-
C               It may be used for stellar  profiles  or  for  finding  the
C               profile in regions of a galaxy.
C
C               Note that the first point in the output array
C               corresponds to the point at the centre of the sector
C               and that if any of the pixels in the output array are
C               blank (because no image pixels were at that radius from
C               the centre) then they are LINEARLY interpolated over.
C
C
C
C         USER PARAMETERS:-
C
C         INPUT                               This   is   the   input   2-D
C                                             Starlink    image    to    be
C                                             analysed. It should be either
C                                             the   one  displayed  on  the
C                                             ARGS, or of the same size and
C                                             in some sense related to it.
C
C         OUTPUT                              This    is    an     optional
C                                             parameter.   If  present  the
C                                             profile is stored  in  a  1-D
C                                             Starlink frame of this name.
C
C
C         USE OF TRACKER-BALL BUTTONS:-
C
C         GREEN 1     This is used to define 3 points, the order  of  which
C                     is  important.  The  first  defines  the  origin. The
C                     second defines the starting vector  AND  its  length.
C                     The  third  is  only  used to define the angle of the
C                     sector. Its distance from the origin  is  irrelevant.
C                     The sector is computed by going anti-clockwise around
C                     the origin (first point) from the second point to the
C                     third. It is possible to have sectors of greater than
C                     180 degrees by appropriate choice of input order.
C
C         WHITE 2     Same as 1
C
C
C
C         WHITE 3     Same as 1
C
C         RED   4     Same as 1
C
C
C
C
C
C
C         C D Pike                 RGO                            12-JAN-82
C
C
C--------------------------------------------------------------------------



      REAL LINDAT(0:1023,2)
      REAL  LINE(0:1023)
      INTEGER*4 PIN,STATUS,AXIN(2),POUT
      CHARACTER*72 TEXT
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
C      ALLOCATE ARGS AND RESET VSR
C
      CALL SRINIT (0,.FALSE.,STATUS)
      IF (STATUS.NE.0) THEN
          CALL WRERR ('NOARGS')
          GOTO 999
      ENDIF
      CALL ARGS_VSRRST
C
C      NOW GET INPUT FRAME
C
      ITRY = 0
    1 CALL RDIMAG('INPUT',FMT_R,2,AXIN,I,PIN,STATUS)
      IF (STATUS.NE.ERR_NORMAL) THEN
         CALL CNPAR('INPUT',STATUS)
         IF(ITRY.EQ.0)  THEN
            ITRY = 1
            GO TO 1
         ENDIF
         CALL WRERR('BADIN')
      END IF
C
C
C      NOW DEFINE THE SECTOR
C
      CALL SECDEF(X1,Y1,X2,Y2,X3,Y3)
C
C    DRAW THE OUTLINE
C
      CALL ARC(X1,Y1,X2,Y2,X3,Y3)
      CALL DEFREGION(%VAL(PIN),AXIN(1),AXIN(2),X1,Y1,X2,Y2,X3,Y3,
     1               LINE,NPTS)
      CALL LINPLOT(LINE,NPTS)
      CALL WRIMAG('OUTPUT',FMT_R,NPTS,1,POUT,STATUS)
      IF (STATUS.EQ.ERR_NORMAL) THEN
          CALL ASP_COPCON(FMT_R,FMT_R,LINE,%VAL(POUT),NPTS,
     :        STATUS)
      ENDIF
C
C
C      NOW TIDY UP AND GO HOME
C
  999 CALL FRDATA(' ',STATUS)
      CALL ARGS_OVCL(8,.FALSE.)
      CALL END PLT
      CALL EXIT
      END
      SUBROUTINE DEFREGION(DATA,NX,NY,X1,Y1,X2,Y2,X3,Y3,LINE,IRAD)
C
      INCLUDE 'INTERIM(FMTPAR)'
      INCLUDE 'INTERIM(ERRPAR)'
      REAL DATA(0:NX-1,0:NY-1),LINE(0:1023)
      LOGICAL INSIDE
C
C   CALCULATE RADIUS OF VECTOR
C
      RADIUS = SQRT((X2-X1)**2 + (Y2-Y1)**2)
C
C   DEFINE EXO-REGION
C
      IXSTART = NINT(X1-RADIUS)
      IXEND   = NINT(X1+RADIUS)
      IYSTART = NINT(Y1-RADIUS)
      IYEND   = NINT(Y1+RADIUS)
C
C    CHECK BOUNDS
C
      IXSTART = MAX(0,IXSTART)
      IXEND   = MIN(NX-1,IXEND)
      IYSTART = MAX(0,IYSTART)
      IYEND   = MIN(NY-1,IYEND)
C
C  NOW A MESSY BIT TO TRY AND CUT DOWN THE SEARCH AREA
C
      CALL GETHETA(X1,Y1,X2,Y2,THETA1)
      CALL GETHETA(X1,Y1,X3,Y3,THETA2)
C
      IF(THETA1.EQ.THETA2) THEN
         THETA1 = 0.0
         THETA2 = 6.283185
         GO TO 300
      ENDIF
      DELTHETA =( THETA2-THETA1)*57.29578
C
      IF(ABS(DELTHETA).LE.90.) THEN
         IF(INSIDE(THETA1,0,90))  THEN
            IF(INSIDE(THETA2,0,90))  THEN
               IXSTART = X1
               IYSTART = Y1
            ELSE
               IF(INSIDE(THETA2,90,180)) IYSTART = Y1
            ENDIF
         ENDIF
         IF(INSIDE(THETA1,90,180))  THEN
            IF(THETA2.LE.3.141593)  THEN
               IXEND = X1
               IYSTART = Y1
            ELSE
               IF(THETA2.GT.3.141593)   IXEND = X1
            ENDIF
         ENDIF
         IF(INSIDE(THETA1,180,270))  THEN
            IF(THETA1.LE.4.712389)  THEN
               IYEND = Y1
               IXEND = X1
            ELSE
               IF(THETA2.GT.4.712389)  IYEND = Y1
            ENDIF
         ENDIF
         IF(INSIDE(THETA1,270,360))  THEN
            IF(INSIDE(THETA2,270,360)) THEN
               IXSTART = X1
               IYEND = Y1
            ELSE
               IF(THETA2.LE.1.57079)  IXSTART = X1
            ENDIF
         ENDIF
      ELSE
C
C  TEST FOR LESSER CASE OF ANGLE >90 <180
C
      IF(INSIDE(THETA1,0,180).AND.INSIDE(THETA2,0,180)) IYSTART = Y1
      IF(INSIDE(THETA1,180,360).AND.INSIDE(THETA2,180,360)) THEN
         IF(THETA2.GT.THETA1)  IYEND = Y1
      ENDIF
      IF(INSIDE(THETA1,90,270).AND.INSIDE(THETA2,90,270)) THEN
         IF(THETA2.GT.THETA1) IXEND = X1
      ENDIF
      IF(INSIDE(THETA1,270,360).AND.INSIDE(THETA2,0,90)) IXSTART = X1
      ENDIF    ! FROM THE FIRST IF
  300 CONTINUE
C
C   SET UP SECTOR
C
      CALL GETSECTOR(DATA,NX,NY,
     1               IXSTART,IXEND,IYSTART,IYEND,LINE,IRAD,
     2               X1,Y1,X2,Y2,X3,Y3)
C
      RETURN
      END
C
C
      LOGICAL FUNCTION INSIDE(X,LIM1,LIM2)
      INSIDE = .FALSE.
      FX = X*57.29578
      IF(FX.GE.LIM1.AND.FX.LE.LIM2) INSIDE = .TRUE.
      END
C
C
C
C
      SUBROUTINE GETSECTOR(DATA,NX,NY,
     1                     IXSTART,IXEND,IYSTART,IYEND,LINE,IRAD,
     2                     X1,Y1,X2,Y2,X3,Y3)
C
      REAL DATA(0:NX-1,0:NY-1),LINE(0:1023),SECTOR(0:1023,2)
C
C  CALC RADIUS
C
      RADIUS = SQRT((X2-X1)**2 + (Y2-Y1)**2)
      IRAD   = NINT(RADIUS)
C
C   SET FLAG FOR X-AXIS CROSSING
C
      ICROSS = 0
C
C  INITIALISE OUTPUT, WRITE IMAGE TO WINDOW
C
      DO 50 I=0,1023
      SECTOR(I,1) = 0.0
      SECTOR(I,2) = 0.0
   50 CONTINUE
      NPIX = 0
      DO 51 J=IYSTART,IYEND
      DO 51 I=IXSTART,IXEND
      NPIX = NPIX + 1
   51 CONTINUE
C
C   GET ANGLES OF VECTORS
C
      CALL GETHETA(X1,Y1,X2,Y2,THETA1)
      CALL GETHETA(X1,Y1,X3,Y3,THETA2)
      IF(THETA1.EQ.THETA2) THEN
         THETA1 = 0.0
         THETA2 = 6.283185
      ENDIF
C
C   TEST FOR CROSSING +VE X-AXIS. IF SO DO IN 2 BITS
C
      IF(THETA2.LT.THETA1)  THEN
          STORE = THETA2
          THETA2 = 6.283185
          ICROSS = 1
      ENDIF
C
C   SET CENTRE POINT OF SECTOR LINE IN CASE IT FALLS THRO
C   THE LATER TESTS FOR INCLUSION IN THE SECTOR!
C
      SECTOR(0,1) = DATA(NINT(X1),NINT(Y1))
      SECTOR(0,2) = 1.0
C
C   LOOP THRO EXO-REGION, TEST THETA AND RADIUS FOR INCLUSION IN SECTOR
C
C
C     SET PIXEL COUNTER
C
  200 NPIX = 0
      DO 100 J=IYSTART,IYEND
      Y = FLOAT(J)
      DO 100 I=IXSTART,IXEND
      X = FLOAT(I)
      NPIX = NPIX + 1
C
      CALL GETHETA(X1,Y1,X,Y,THETAX)
C
C  TEST IF ANGLE IN SECTOR
C
      IF(THETAX.GE.THETA1.AND.THETAX.LE.THETA2) THEN
          DIST = SQRT((X-X1)**2 + (Y-Y1)**2)
          IF(DIST.LE.RADIUS)   THEN
              IDIST = NINT(DIST)
              SECTOR(IDIST,1) = DATA(I,J) + SECTOR(IDIST,1)
              SECTOR(IDIST,2) = SECTOR(IDIST,2) + 1
          ENDIF
      ENDIF
C
  100 CONTINUE
C
C  CHECK ICROSS AND RETURN TO 100 LOOP FOR SECOND BIT IF FLAG SET
C
      IF(ICROSS.EQ.1)   THEN
         THETA1 = 0.0
         THETA2 = STORE
         ICROSS = 0
         GO TO 200
      ENDIF
C
C   TAKE MEAN OF ACCUMULATED VALUES
C
      DO 201 K=0,IRAD-1
      IF(SECTOR(K,2).GT.0.0)  THEN
         LINE(K) = SECTOR(K,1)/SECTOR(K,2)
      ENDIF
  201 CONTINUE
C
C   NOW LINEARLY INTERPOLATE OVER OUTPUT PIXELS THAT HAVE NO DATA
C
      DO 202 I=1,IRAD-1
      IF(LINE(I).NE.0) GO TO 202

      DO K=1,100
      IF(LINE(I+K).NE.0)  GO TO 204
      ENDDO

  204 IST = I-1
      IEND = I+K

      DO K=IST+1,IEND-1
      LINE(K) = LINE(IST) + (LINE(IEND)-LINE(IST))*(K-IST)/(IEND-IST)
      ENDDO

      I = IEND

  202 CONTINUE

C
C   NORMAL EXIT
C
      RETURN
      END
C
C
C
      SUBROUTINE GETHETA(X1,Y1,X,Y,ANGLE)
C
      IF(X.EQ.X1)  THEN
         IF(Y.GE.Y1)  THEN
            ANGLE = 1.57079
         ELSE
            ANGLE = 4.712389
         ENDIF
         RETURN
      ENDIF
      ANGLE = ATAN((Y-Y1)/(X-X1))
      IF(ANGLE.LT.0.0)   THEN
           IF(X.LT.X1) THEN
               ANGLE = 3.141593 + ANGLE
           ELSE
               ANGLE = 6.283185 + ANGLE
           ENDIF
      ELSE
           IF(X.LT.X1)  ANGLE = 3.141593 + ANGLE
      ENDIF
C
      RETURN
      END
C
C
C
      SUBROUTINE LINPLOT(LINE,NPTS)
      REAL LINE(NPTS)
      DIMENSION AX(2),AY(2)
C
C  SET FULL LINE AND CLEAR SCREEN
C
C
C  GET SCALING PARMS
C
      FMAX = 0.0
      FMIN = 0.0
      DO 100 I=2,NPTS
      IF(LINE(I).GT.FMAX)  FMAX = LINE(I)
C     IF(LINE(I).LT.FMIN)  FMIN = LINE(I)
  100 CONTINUE
C
C  CHECK FOR LACK OF DATA
C
      IF(FMAX.EQ.FMIN)  THEN
         WRITE(6,*) ' NO DATA OBTAINED IN SECTOR'
         WRITE(6,*) ' OR DATA IS CONSTANT VALUED'
         RETURN
      ENDIF
C
C   PREPARE FOR PLOTTING
C
      AX(1) = 1.
      AX(2) = REAL(NPTS)
      AY(1) = 0.
      AY(2) = 1.2*FMAX
      CALL JBDEV('ARGS')
      CALL JBAXES(AX,2,20.,6HRADIUS,6,AY,2,15.0,1H ,1)
C
C  PLOT DATA
C
      DO 200 I=1,NPTS
      X = REAL(I)
      Y = REAL(LINE(I))

      CALL JOIN PT(X,Y)
  200 CONTINUE
      RETURN
      END
C
C

C
C
      SUBROUTINE SECDEF(X1,Y1,X2,Y2,X3,Y3)
C
C      USE CURSOR TO SELECT TWO POINTS
C
      CALL ARGS_CUROP ('1','W')
      CALL ARGS_OVOP(8,'G')
C
C      IPT IS USED TO COUNT THE POINTS SELECTED
C
      IPT=1
 100  CALL ARGS_RDCUR('IMAG',ID,IB,X,Y)
C
C      IF IMAGE WAS LOCATED THEN STORE POSITION
C
      IF (IB.EQ.1) THEN
          IF (IPT.EQ.1) THEN
              X1 = X
              Y1 = Y
              CALL CROSS(X,Y)
          ELSE IF(IPT.EQ.2) THEN
              X2 = X
              Y2 = Y
              CALL CROSS(X,Y)
              ELSE
                  X3 = X
                  Y3 = Y
          ENDIF
          CALL ARGS_CURC ('G')
          IPT=IPT+1
      ENDIF
      IF (IPT.LE.3) GO TO 100
C
C      REMOVE CURSOR
C
      CALL ARGS_CURCL
      END
C
      SUBROUTINE CROSS(X,Y)
      REAL XVAL(2),YVAL(2)


      XVAL(1) = X-2.0
      XVAL(2) = X + 2.0
      YVAL(1) = Y
      YVAL(2) = Y

      CALL ARGS_POLYL(1,2,XVAL,YVAL,ISTAT)

      XVAL(1) = X
      XVAL(2) = X
      YVAL(1) = Y-2.0
      YVAL(2) = Y+2.0
      CALL ARGS_POLYL(1,2,XVAL,YVAL,ISTAT)

      END
      SUBROUTINE ARC(X1,Y1,X2,Y2,X3,Y3)

      REAL XPOS(360),YPOS(360)

C   CALCULATE RADIUS OF ARC

      RADIUS = SQRT((X2-X1)**2 + (Y2-Y1)**2)

C   SET RADIUS LINE TO DRAW

      XPOS(1) = X1
      YPOS(1) = Y1
      XPOS(2) = X2
      YPOS(2) = Y2
      NPTS = 2

C  FLAG FOR X-AXIS CROSSING

      ICROSS = 0

      CALL GETHETA(X1,Y1,X2,Y2,THETA1)
      CALL GETHETA(X1,Y1,X3,Y3,THETA2)

      IF(THETA2.LT.THETA1)   THEN
         STORE = THETA2
         THETA2 = 6.283185
         ICROSS = 1
      ENDIF

C   SET ANGULAR RESOLUTION

      STEP = 0.02

  200 DO I=1,360
      THETA = THETA1 + I*STEP
      IF(THETA.GT.THETA2)  GO TO 100
      NPTS = NPTS + 1
      XPOS(NPTS) = RADIUS*COS(THETA) + X1
      YPOS(NPTS) = RADIUS*SIN(THETA) + Y1
      ENDDO

C   CHECK FOR AXIS CROSSING

  100 IF(ICROSS.EQ.1)  THEN
         THETA1 = 0.0
      THETA2 = STORE
         ICROSS = 0
         GO TO 200
      ENDIF

      NPTS = NPTS + 1
      XPOS(NPTS) = X1
      YPOS(NPTS ) = Y1

      CALL ARGS_OVOP(8,'Y')
      CALL ARGS_POLYL(1,NPTS,XPOS,YPOS,ISTAT)

      END


      SUBROUTINE JBNEW(X,Y,WIDTH,HEIGHT,N)
      RETURN
      END


