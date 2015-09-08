      SUBROUTINE GBOX (XEXT,YEXT,MAXBOX,MAXPAR,NBOX,CORBOX,STATUS)
C+
C     GBOX.
C
C     Subroutine to define a set of rectangular apertures
C     for CPB's equivalent profile routine. The smallest
C     aperture is defined by the user using a cursor
C     and the largest aperture is either taken to be the entire
C     array or defined by the user with the cursor.
C
C  Given;
C   XEXT   (I) X size of the image.
C   YEXT   (I) Y  "   "   "    "  .
C   MAXBOX (I) Max. permitted no. of aperture.
C   MAXPAR (I)  " . no. of parameters for each box. Must be at
C              least 4.
C   NBOX   (I) No. of boxes required.
C
C  Returned;
C   NBOX   (I) No. of boxes generated. Usually, but not always
C              equal to the number requested, depending
C              on the space available.
C   CORBOX (IA)Coordinates of the boxes generated.
C   STATUS (I) Return status. = 0 for successful completion, 
C              otherwise non-zero.
C
C  Subroutines called;
C   Interfaces:- OUTPUT, YESNO.
C   E2D:-        RECUR, GBOX1.
C
C  Structure:-
C   Copy coords of the max box = size of the image array.
C   Put up cursor to mark the min. box to be used.
C   If return status Ok.
C     Copy coords of min box.
C     generate coords for the other boxes.
C   end if
C
C  A C Davenhall./ROE/                                   24/8/82.
C  A C Davenhall./ROE/     {Modified}                    12/9/82.
C-
      INTEGER XEXT,YEXT,NBOX,MAXBOX,MAXPAR,STATUS
      INTEGER CORBOX(MAXBOX,MAXPAR)
C
      INTEGER XBASE,YBASE,XTOP,YTOP
      INTEGER CORMIN(4),CORMAX(4),DIFF(4),INCR(4)
      INTEGER IOSTAT
      LOGICAL LARGAP
      CHARACTER REPLY*1
C
      INTEGER PLANE1,PLANE2
      PARAMETER (PLANE1=9)
      PARAMETER (PLANE2=10)
      CHARACTER*1 COL1,COL2
      PARAMETER (COL1='W')
      PARAMETER (COL2='G')
C
C
      STATUS=0
C
C    Define the largest aperture.
C
      IOSTAT=0
      CALL YESNO (
     : ' Is the whole image to be taken as the largest aperture?',
     : 'Y',REPLY,IOSTAT)
      IF (REPLY.EQ.'N') THEN
C
C    Define the coords. of the largest box with the cursor.
C
        CALL OUTPUT ( 
     :   ' Mark the box corresponding to the largest aperture.',
     :     IOSTAT)
        CALL OUTPUT (
     :   ' This should be much bigger than the visible extent',
     :     IOSTAT)
        CALL OUTPUT (
     :   ' of the galaxy.',IOSTAT)
        CALL RECUR (PLANE1,COL1,PLANE2,COL2,XBASE,YBASE,XTOP,YTOP,
     :               STATUS)
        CORMAX(1)=XBASE
        CORMAX(2)=YBASE
        CORMAX(3)=XTOP
        CORMAX(4)=YTOP
        LARGAP=.TRUE.
      ELSE
C
C    Set up the coordinates of the largest box - equal to
C    the whole array size.
C
        CORMAX(1)=1
        CORMAX(2)=1
        CORMAX(3)=XEXT
        CORMAX(4)=YEXT
        LARGAP=.FALSE.
      END IF
C
C    Obtain the coords. of the smallest box from the cursor.
C
      CALL OUTPUT (
     : ' Mark box corresponding to the smallest aperture.',IOSTAT)
      CALL OUTPUT (
     : ' This should be considerably larger then the visible ',IOSTAT)
      CALL OUTPUT (
     : ' Extent of the galaxy.',IOSTAT)
      IF (LARGAP) CALL OUTPUT (
     :   ' It also must be completely inside the largest aperture.',
     :     IOSTAT)
C
      CALL RECUR (PLANE1,COL1,PLANE2,COL2,XBASE,YBASE,XTOP,YTOP,
     :            STATUS)
      CORMIN(1)=XBASE
      CORMIN(2)=YBASE
      CORMIN(3)=XTOP
      CORMIN(4)=YTOP
C
C    Proceed to generate the boxes if the return status from
C    defining the largest and smallest apertures is Ok.
C
      IF (STATUS.EQ.0) THEN
C
C    Generate array of coords. for the boxes.
C
        CALL GBOX1 (CORMIN,CORMAX,MAXBOX,MAXPAR,NBOX,
     :              DIFF,INCR,CORBOX)
      END IF
      END
