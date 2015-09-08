      SUBROUTINE ELLPLTS
C+
C     ELLPLTS.
C
C     Subroutine to plot a series of contours
C     extracted from a nebular image and a
C     set of contours fit to each ellipse
C     on a user selected graphics device. Both
C     the contours and the fitted ellipse
C     are held as Starlink images.
C
C  Given;
C   None.
C
C  Returned;
C   None.
C
C  Subroutines called;
C   Interfaces:- INPICR, OUTPICR, MULREP, OUTPUT, CLEARIM.
C   E2D:-        PLTFIT.
C
C  Structure:-
C   Attempt to obtain a pointer to the contour file.
C      "    "    "    "    "    "   "  ellipse  "  .
C   If pointer obtained Ok
C     Attempt to obtain pointers to a couple of work arrays
C     If pointers obtained Ok
C       Query the user for the graphics device required.
C       make the plots
C     else
C       print an error message.
C     end if
C   else
C     print an error message.
C   end if
C   tidy up images.
C
C  A C Davenhall./ROE/                               24/9/82.
C-
      INTEGER AXIS1(3),AXIS2(2),AXIS3(1)
      INTEGER CNTPTR,PARPTR,WK1PTR,WK2PTR
      INTEGER WKSTAT,IOSTAT,IMSTAT
      INTEGER DEVICE,MAXPAR,MAXPOS,MAXCNT,PARMS
      LOGICAL CLEAR
      CHARACTER REPLY*20
C
C
C    Attempt to obtain a pointer to the contour file.
C
      IOSTAT=0
      IMSTAT=0
      CALL INPICR ('CONTOURS',
     : ' Enter name of the file holding the contours;',
     :   3,AXIS1,CNTPTR,IMSTAT)
C
C    Attempt to obtain a pointer to the ellipse parameter file.
C
      CALL INPICR ('ELLIPSES',
     : ' Enter name of file holding the ellipse parameters;',
     :   2,AXIS2,PARPTR,IMSTAT)
C
C    Proceed if pointer obtained Ok.
C
      IF (IMSTAT.EQ.0) THEN
C
C    Attempt to obtain pointers to the work arrays.
C
        WKSTAT=0
        AXIS3(1)=AXIS1(2)
        CALL OUTPICR ('WORK1',
     :   ' 1st work array.',
     :     1,AXIS3,WK1PTR,WKSTAT)
        CALL OUTPICR ('WORK2',
     :   ' 2nd work array.',
     :      1,AXIS3,WK2PTR,WKSTAT)
C
C    Proceed if pointers to these arrays obtained Ok.
C
        IF (WKSTAT.EQ.0) THEN
C
C    Obtain a graphics device from the user.
C
          CALL MULREP (
     :     ' Select required graphics device;',
     :     'ARGS,T4010,VERSATEC$',REPLY,IOSTAT)
          IF (REPLY.EQ.'T4010')       DEVICE=1
          IF (REPLY.EQ.'ARGS')        DEVICE=2
          IF (REPLY.EQ.'VERSATEC')    DEVICE=3
C
C    On the basis of the device selected decide
C    whether to clear the screen or not.
C
          CLEAR=.TRUE.
          IF (DEVICE.EQ.1) CLEAR=.FALSE.
C
C    Make the plots.
C
          MAXPAR=AXIS1(1)
          MAXPOS=AXIS1(2)
          MAXCNT=AXIS1(3)
          PARMS=AXIS2(1)
          CALL PLTFIT (DEVICE,CLEAR,MAXPAR,MAXPOS,MAXCNT,
     :                 %VAL(CNTPTR),PARMS,%VAL(PARPTR),
     :                 %VAL(WK1PTR),%VAL(WK2PTR))
        ELSE
          CALL OUTPUT (
     : ' ***ERROR Unable to obtain pointers to work arrays.',
     :                 IOSTAT)
        END IF
      ELSE
        CALL OUTPUT (
     :   ' ***ERROR Unable to access files successfully.',
     :               IOSTAT)
      END IF
C
C    Tidy up images.
C
      CALL CLEARIM ('CONTOURS')
      CALL CLEARIM ('ELLIPSES')
      CALL CLEARIM ('WORK1')
      CALL CLEARIM ('WORK2')
      END
