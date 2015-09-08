      SUBROUTINE CRCLCR (PLANE1,COL1,PLANE2,COL2,XPOS,YPOS,
     :                   RADIUS,STATUS)
C+
C     CRCLCR.
C
C     Subroutine to return the coords (in pixels) of a
C     circular cursor drawn on the Args.
C
C  Given;
C   PLANE1 (I)  Args overlay plane to be used by the cursor.
C               Note; any graphics in this plane will be cleared.
C   COL1   (C)  Colour of the cursor.
C   PLANE2 (I)  Args overlay plane in which the accepted circle
C               is to drawn.
C   COL2   (C)  Colour of the accepted circle.
C
C  Returned;
C   XPOS   (I)  X position of the centre of the circle.
C   YPOS   (I)  Y    "     "   "    "    "   "    "   .
C   RADIUS (I)  Radius of the circle.
C   STATUS (I)  Max(input status, internale status).
C               Internal status = 0 for a successful return,
C               otherwise non-zero.
C
C  Note;
C   (1)  This routine uses the Args data-base.
C   (2)  Overlay plane 15 is used internally and any
C        graphics in it will be cleared.
C
C  Subroutines called;
C   Args:-   SRINIT, ARGS_PICWRT, ARGS_OVWRT, ARGS_OVCOL, ARGS_OVGEN,
C            ARGS_USRCUR, ARGS_NUMIM, ARGS_ATOP.
C   Fings:-  ARGS, VUPORT, WINDOL, MOVTO2, CHAHOL, DEVEND.
C   E2D:-    BLABELS, CURCIR, CIRCLE.
C
C   A C Davenhall./ROE/  25.9.82.
C
C   Loosly based on code by;
C   B. D. Kelly, J. A. Cooke and D. Tudhope.
C
C-
      INTEGER PLANE1,PLANE2,XPOS,YPOS,RADIUS,STATUS
      CHARACTER COL1*1,COL2*2
C
C
      INTEGER BPLANE
      PARAMETER (BPLANE=15)
C
      INTEGER ID,ISTATUS,INTSTT
      INTEGER RADP,JUNK
      REAL XPOSA,YPOSA,RADA
      INTEGER XIN,YIN,XOUT,YOUT,BUTT
      LOGICAL CLRFLG
C
      INTEGER ISIZE,JSIZE
      PARAMETER (ISIZE=4)
      PARAMETER (JSIZE=64)
C
      INTEGER*2 CURSOR(256)
C
C
C    Initialise the Args.
C
      CALL SRINIT (0,.FALSE.,ISTATUS)
C
C    Put up button labels.
C
      CALL BLABELS (BPLANE,COL1,'ACCEPT POINT','INCREASE SZE.',
     :              'DECREASE SZE.','ABORT')
C
      ISTATUS=0
C
C    Enable the tentative overlay plane.
C
      CALL ARGS_PICWRT
      CALL ARGS_OVWRT (PLANE1)
      CALL ARGS_OVCOL (PLANE1,COL1)
      CALL ARGS_OVGEN ('W')
C
C
C    Set up starting position to the middle of the screen.
C    Because of the peculiarities of the cursor routine the
C    coords. given below correspond to the middle of the screen.
C    True story.
C
      XIN=223
      YIN=223
      RADA=5.0E0
C
C    Set an initial (illegal) value for the button label.
C
      BUTT=5
      CLRFLG=.FALSE.
C
      DO WHILE (BUTT.NE.1.AND.BUTT.NE.4)
C
C    Increase the cursor radius if button 2 has been chosen.
C
        IF (BUTT.EQ.2)  RADA=RADA+1.0E0
C
C    Decrease the cursor radius if button 3 has been chosen.
C
        IF (BUTT.EQ.3)  RADA=RADA-1.0E0
C
C    Prevent the radius adopting illegal values.
C
        RADA=MIN(RADA,3.1E1)
        RADA=MAX(RADA,1.0E0)
C
C
        IF (CLRFLG) CALL ARGS_OVCLR (PLANE1)
        CLRFLG=.FALSE.
C
C    Display messages if the cursor radius has reached either
C    its maximum or minimum values.
C
        IF (RADA.GE.3.05E1.OR.RADA.LE.1.50E0) THEN
C
C    Setup for using Fings.
C
          CALL ARGS
          CALL ARGS_PICWRT
          CALL VUPORT (0.0E0,1.0E0,0.0E0,1.0E0)
          CALL WINDOL (0.0E0,5.11E2,0.0E0,5.11E2)
C
C    Enable the tenatative overlay plane.
C
          CALL ARGS_PICWRT
          CALL ARGS_OVWRT (PLANE1)
          CALL ARGS_OVCOL (PLANE1,COL1)
          CALL ARGS_OVGEN ('W')
C
          CALL MOVTO2 (1.40E2,4.7E2)
          IF (RADA.GE.3.05E1) THEN
            CALL CHAHOL (%REF('MAXIMUM CURSOR SIZE REACHED*.'))
          END IF
          IF (RADA.LE.1.5E0) THEN
            CALL CHAHOL (%REF('MINIMUM CURSOR SIZE REACHED*.'))
          END IF
          CLRFLG=.TRUE.
          CALL DEVEND
        END IF
C
C    Generate and display the cursor.
C
        CALL CURCIR (RADA,ISIZE,JSIZE,CURSOR)
        CALL ARGS_USRCUR (PLANE1,COL1,CURSOR,XIN,YIN,XOUT,YOUT,
     :                    BUTT)
        XIN=XOUT
        YIN=YOUT
      END DO
      CALL ARGS_OVCLR (PLANE1)
C
C    Accept the point if the user has so desired by his choice
C    of buttons.
C
      IF (BUTT.EQ.1) THEN
        XOUT=XOUT+32
        YOUT=YOUT+32
C
C    Convert from Args to pixel coords, using the latest image
C    drawn.
C
        CALL ARGS_NUMIM (ID)
        RADP=NINT(RADA)
        CALL ARGS_ATOP (ID,XOUT,YOUT,XPOS,YPOS,ISTATUS)
        CALL ARGS_ATOP (ID,XOUT+RADP,YOUT,RADIUS,JUNK,ISTATUS)
        RADIUS=RADIUS-XPOS
C
C    Set up for drawing the accepted circle, in Args coords.
C    ... Set up Fings.
C
        CALL ARGS
        CALL ARGS_PICWRT
        CALL VUPORT (0.0E0,1.0E0,0.0E0,1.0E0)
        CALL WINDOL (0.0E0,5.11E2,0.0E0,5.11E2)
C
C    Set up for using the accepted overlay plane.
C
        CALL ARGS_OVWRT (PLANE2)
        CALL ARGS_OVCOL (PLANE2,COL2)
        CALL ARGS_OVGEN ('W')
C
C    Draw the circle and stop plotting.
C
        XPOSA=FLOAT(XOUT)
        YPOSA=FLOAT(YOUT)
        CALL CIRCLE (XPOSA,YPOSA,RADA)
        CALL DEVEND
C
C    Set the internal status.
C
        INTSTT=0
      ELSE
        INTSTT=1
        XPOS=1
        YPOS=1
        RADIUS=1
      END IF
C
C    Set the return status.
C
      STATUS=MAX(STATUS,INTSTT,ISTATUS)
C
C    Clear the button labels and renable the picture planes.
C
      CALL ARGS_OVCLR (BPLANE)
      CALL ARGS_PICWRT
      END
