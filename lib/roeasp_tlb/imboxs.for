      SUBROUTINE IMBOXS (PLANE1,COL1,PLANE2,COL2,MAXBOX,MAXPAR,
     :                   NBOX,COORDS)
C+
C     IMBOXS.
C
C     Subroutine to obtain the coordinates (in pixels) of a
C     number of boxes surrounding user selected features
C     on an image displayed on the Args. The boxes are
C     defined using the cursor.
C
C  Given;
C   PLANE1 (I)  Args overlay plane to be used by the tentative
C               box. note; any graphics in this plane will
C               be cleared.
C   COL1   (C)  Colour of the tentative box.
C   PLANE2 (I)  Args overlay plane to be used by the accepted
C               boxes.
C   COL2   (C)  Colour of the accepted boxes.
C   MAXBOX (I)  Max. permitted no. of boxes = size of arrays
C               to hold them (below).
C   MAXPAR (I)  Max. permitted no. of parameters for each
C               box, the other dimension of the coords.
C               array ( = at least 4).
C
C  Returned;
C   NBOX   (I)  No. of selected boxes.
c   COORDS (IA) Coords. of selected boxes.
C
C  Subroutines called;
C   Interfaces:- YESNO, OUTPUT.
C   Args:-       SRINIT.
C   E2D:-        RECUR.
C
C  Structure:-
C   Initialise the Args.
C   Do while (More boxes required and less than the permitted
C             no of boxes).
C     Attempt to obtain box
C     If box obtained Ok
C       Increment no. of boxes.
C       Add to list of boxes.
C     end if
C     Inquire if more boxes required.
C   end do
C   end
C
C  A C Davenhall./ROE/                                   19/8/82.
C-
      INTEGER MAXBOX,MAXPAR,NBOX,PLANE1,PLANE2
      INTEGER COORDS(MAXBOX,MAXPAR)
      CHARACTER COL1*(*),COL2*(*)
C
      INTEGER IFAIL,STATUS,IOSTAT
      LOGICAL MORE
      CHARACTER REPLY*1
      INTEGER XBASE,YBASE,XTOP,YTOP
C
C
      IOSTAT=0
C
C    Initialise the Args.
C
      CALL SRINIT (0,.FALSE.,IFAIL)
C
C    Obtain the boxes.
C
      MORE=.TRUE.
      NBOX=0
C
      DO WHILE (MORE)
C
C    Attempt to obtain a box.
C
        STATUS=0
        CALL RECUR (PLANE1,COL1,PLANE2,COL2,XBASE,YBASE,XTOP,YTOP,
     :              STATUS)
C
C    Accept if the box is Ok.
C
        IF (STATUS.EQ.0) THEN
          NBOX=NBOX+1
          COORDS(NBOX,1)=XBASE
          COORDS(NBOX,2)=YBASE
          COORDS(NBOX,3)=XTOP
          COORDS(NBOX,4)=YTOP
        END IF
C
C    Inquire if another box is required, unless the array 
C    is full, in which case print a message.
C 
        IF (NBOX.LT.MAXBOX) THEN
          CALL YESNO (' Is another box required?','Y',REPLY,IOSTAT)
          IF (REPLY.EQ.'N') MORE=.FALSE.
        ELSE
          CALL OUTPUT (' Max. permitted no. of boxes chosen.',
     :                   IOSTAT)
          MORE=.FALSE.
        END IF
      END DO
      END
