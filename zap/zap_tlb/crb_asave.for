	SUBROUTINE CRB_ASAVE
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *******************
C                     *                 *
C                     * Program   ASAVE *
C                     *                 *
C                     *******************
C
C
C
C          CALLING SEQUENCE:-
C               ASAVE [XLIMITS=I1,I2] [YLIMITS=J1,J2]
C
C
C          FUNCTION:-
C               Saves the currently displayed Args image by copying it i
C               a file on disk.  By default the entire 16 bit image is r
C               from  the  Args  memory  store  into  an  integer*2   fi
C               Optionally,  any  rectangular region of the Args screen
C               be saved by specifying the X and Y limits of the region.
C
C
C          USE:-
C               Useful for storing an ARGS image  for  later  display.
C               image  can  be  redisplayed  most  simply  with the prog
C               AFLASH, which simply copies the array back  into  the  A
C               without  any scaling.  Alternatively, the program ADISP
C               be used if scaling  limits  of  0  to  255  are  specifi
C               However, ADISP will not redisplay any overlays properly.
C
C
C
C         USER PARAMETERS:-
C
C         IMAGE                               Name of starlink 2D disk f
C                                             to   be   created  by  ASA
C                                             containing the Args image.
C
C
C         NORMALLY DEFAULTED PARAMETERS:-
C
C         XLIMITS         0,511               Minimum and  maximum  colu
C                                             on the Args to be saved.
C
C         YLIMITS         0,511               Minimum and maximum  rows
C                                             the Args to be saved.
C
C
C
C         W D Pence                AAO                            13-JAN
C
C
C-----------------------------------------------------------------------
C
C     COPIES THE ARGS IMAGE AND SAVES IT AS A DISK 2D FILE.
C
      INTEGER IDIMN(2),IX(2),IY(2)
      CALL SRINIT(0,.FALSE.,ISTAT)
      IF (ISTAT .NE. 0)THEN
        CALL WRUSER('FAILED TO GET ARGS',ISTAT)
        STOP
      END IF
C
C     GET BOUNDARYS OF RECTANGULAR AREA TO BE SAVED
C
      GO TO 12
10    CALL CNPAR('XLIMITS',ISTAT)
      CALL CNPAR('YLIMITS',ISTAT)
12    CALL RDKEYI('XLIMITS',.FALSE.,2,IX,NXVALS,ISTAT)
      IF (NXVALS .LT. 2 )THEN
        CALL WRUSER('MUST INPUT MIN AND MAX LIMITS',ISTAT)
        GO TO 10
      END IF
      IF (IX(1) .GE. IX(2))THEN
        CALL WRUSER('FIRST LIMIT MUST BE LESS THAN THE SECOND',
     1       ISTAT)
        GO TO 10
      END IF
      IF (IX(1) .LT. 0 .OR. IX(2) .GT. 511)THEN
        CALL WRUSER('LIMITS MUST BE IN THE RANGE 0 TO 511',ISTAT)
        GO TO 10
      END IF
      CALL RDKEYI('YLIMITS',.FALSE.,2,IY,NYVALS,ISTAT)
      IF (NYVALS .LT. 2)THEN
        CALL WRUSER('MUST INPUT MIN AND MAX LIMITS',ISTAT)
        GO TO 10
      END IF
      IF (IY(1) .GE. IY(2))THEN
        CALL WRUSER('FIRST LIMIT MUST BE LESS THAN THE SECOND',
     1       ISTAT)
        GO TO 10
      END IF
      IF (IY(1) .LT. 0 .OR. IY(2) .GT. 511)THEN
        CALL WRUSER('LIMITS MUST BE IN THE RANGE 0 TO 511',ISTAT)
        GO TO 10
      END IF
C
      IDIMN(1)=IX(2)-IX(1)+1
      IDIMN(2)=IY(2)-IY(1)+1
      NDIMS=2
C
C     CREATE AN OUTPUT ARRAY
C
      CALL WRUSER('Name for new image file = ?',JSTAT)
      CALL WRIMAG('IMSAVE',102,IDIMN,NDIMS,IPIN,JSTAT)
C
C     LOAD THE ARGS IMAGE INTO THE ARRAY
C
      CALL SRFPR(%VAL(IPIN),IDIMN(1),IDIMN(1),IDIMN(2),IX(1),IY(1))
C
	CALL CNPAR('IMSAVE',ISTAT)
      END
C***********************************************************************
