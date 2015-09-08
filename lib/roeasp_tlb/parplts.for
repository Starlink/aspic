      SUBROUTINE PARPLTS
C+
C     PARPLTS.
C
C     Subroutine to take a file holding parameters determined
C     by fitting ellipses to a set of contours extracted from a 
C     nebular image and plot any selected parameter against
C     any other parameter.
C
C  Given;
C   None.
C
C  Returned;
C   None.
C
C  Subroutines called;
C   Interfaces:- INPICR, OUTPICR, MULREP, OUTPUT, CLEARIM.
C   E2D:-        PARPLT.
C
C  Structure:-
C   Attempt to obtain a pointer to parameter file.
C   If pointer obtained Ok.
C     Attempt to obtain pointers to work arrays.
C     If pointers obtained Ok
C       Select graphics device
C       perform plots.
C     else
C       Print message saying can't get work arrays.
C     end if
C   else
C     Print message saying can't get parameter file.
C   end if
C   tidy up images.
C
C  A C Davenhall./ROE/                                   24/9/82.
C-
      INTEGER AXIS1(2),AXIS2(1)
      INTEGER PARPTR,WK1PTR,WK2PTR
      INTEGER PRSTAT,WKSTAT,IOSTAT
      INTEGER PARMS,MAXCNT,DEVICE
      LOGICAL CLEAR
C
      CHARACTER REPLY*20
C
C
C    Attempt to obtain pointer to parameter file.
C
      IOSTAT=0
      PRSTAT=0
      CALL INPICR ('ELLIPSES',
     : ' Enter name of file holding ellipse parameters;',
     :   2,AXIS1,PARPTR,PRSTAT)
C
C    Proceed if pointer obtained Ok.
C
      IF (PRSTAT.EQ.0) THEN
C
C    Attempt to obtain pointer to work arrays.
C
        AXIS2(1)=AXIS1(2)
        WKSTAT=0
        CALL OUTPICR ('WORK1',
     :   ' 1st work array.',
     :     1,AXIS2,WK1PTR,WKSTAT)
        CALL OUTPICR ('WORK2',' 2nd work array.',
     :     1,AXIS2,WK2PTR,WKSTAT)
C
C    Proceed if these pointers obtained Ok.
C
        IF (WKSTAT.EQ.0) THEN
C
C    Select graphics device.
C
          CALL MULREP (' Select required graphics device;',
     :                 'ARGS,T4010,VERSATEC$',REPLY,IOSTAT)
          IF (REPLY.EQ.'T4010')      DEVICE=1
          IF (REPLY.EQ.'ARGS')       DEVICE=2
          IF (REPLY.EQ.'VERSATEC')   DEVICE=3
C
C    On the basis of the device selected decide whether the
C    screen is to be cleared or not.
C
          CLEAR=.TRUE.
          IF (DEVICE.EQ.1) CLEAR=.FALSE.
C
C    Perform the plots.
C
          PARMS=AXIS1(1)
          MAXCNT=AXIS1(2)
          CALL PARPLT (DEVICE,CLEAR,PARMS,MAXCNT,%VAL(PARPTR),
     :                 %VAL(WK1PTR),%VAL(WK2PTR))
        ELSE
          CALL OUTPUT (
     :     ' ***ERROR Unable to obtain work arrays successfully.',
     :       IOSTAT)
        END IF
      ELSE
        CALL OUTPUT (
     :   ' ***ERROR Unable to obtain parameter file successfully.',
     :     IOSTAT)
      END IF
C
C    Tidy up the images.
C
      CALL CLEARIM ('ELLIPSES')
      CALL CLEARIM ('WORK1')
      CALL CLEARIM ('WORK2')
      END
