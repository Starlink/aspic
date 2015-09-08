      SUBROUTINE ABOX
C+
C     ABOX.
C
C     Routine to draw a number of boxes around features of an
C     image displayed on the Args. Graphics will be used to 
C     draw boxes around the selected features and the coords.
C     of the selected areas will be saved as a Starlink
C     image.
C
C
C  Given;
C   None.
C
C  Returned;
C   none.
C
C  Subroutines called;
C   Interfaces:- OUTPUT, OUTPICR, CLEARIM.
C   E2D:-        IMBOXES, COPY.
C
C  Structure:-
C   Obtain coords. of boxes.
C   If more than zero boxes have been selected.
C     attempt to obtain pointer to output starlink image
C     If pointer obtained Ok.
C       copy coords. into starlink image.
C     else
C        print message saying can7t get image.
C     end if
C   else
C     print message saying no boxes selected.
C   end if
C   tidy up Starlink images.
C
C  A C Davenhall./ROE/   {Original}               16/8/82.
C  A C Davenhall./ROE/                            19/8/82.
C-
      INTEGER BOXPTR,BXSTAT,BOXSIZ(2)
C
      INTEGER MAXBOX,MAXPAR,NBOX
      PARAMETER (MAXBOX=30)
      PARAMETER (MAXPAR=4)
      INTEGER COORDS(MAXBOX,MAXPAR)
C
      INTEGER IOSTAT
      CHARACTER BUFFER*60
C
      INTEGER PLANE1,PLANE2
      PARAMETER (PLANE1=9)
      PARAMETER (PLANE2=10)
      CHARACTER COL1*1,COL2*1
      PARAMETER (COL1='W')
      PARAMETER (COL2='G')
C
C
C    Obtain coords. of boxes.
C
      CALL IMBOXS (PLANE1,COL1,PLANE2,COL2,MAXBOX,MAXPAR,NBOX,
     :             COORDS)
C
C    If any boxes at all have been selected attempt to
C    obtain a pointer to an output array to hold their
C    coords.
C
      IF (NBOX.GT.0) THEN
        WRITE (BUFFER,2000) NBOX
 2000   FORMAT(1X,'No. of boxes selected = ',I2)
        CALL OUTPUT (BUFFER,IOSTAT)
        BXSTAT=0
        BOXSIZ(1)=NBOX
        BOXSIZ(2)=4
        CALL OUTPICR ('BOXFIL',
     :     ' Enter name for file to hold coords. of boxes;',
     :       2,BOXSIZ,BOXPTR,BXSTAT)
C
C    Proceed to copy the arrays if the pointer has been
C    obtained Ok.
C
        IF (BXSTAT.EQ.0) THEN
          CALL COPY (MAXBOX,MAXPAR,COORDS,NBOX,4,%VAL(BOXPTR))
          CALL OUTPUT (' File saved successfully.',IOSTAT)
        ELSE
          CALL OUTPUT (
     :  ' ***ERROR Unable to obtain file for box coords. successfully.',
     :           IOSTAT)
        END IF
      ELSE
        CALL OUTPUT (' No boxes selected. Program terminating.',
     :               IOSTAT)
      END IF
C
C    Tidy up image.
C
      CALL CLEARIM ('BOXFIL')
      END
