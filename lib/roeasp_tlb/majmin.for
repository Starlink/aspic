      SUBROUTINE MAJMIN (IMAGE,IXEXT,IYEXT,PLANE,COLOUR,
     :                   XCEN,YCEN,AXIS,NAXIS,PTS,STATUS)
C+
C     MAJMIN.
C
C     Subroutine to interactively obtain major and minor
C     axis profiles from pre-centred nebular image.
C
C  Given;
C   IMAGE   (RA)  Array containing the nebular image.
C   IXEXT   (I)   X size of image array.
C   IYEXT   (I)   Y  "   "    "     "  .
C   PLANE   (I)   Args overlay plane in which the cursor
C                 is to appear.
C   COLOUR  (C)   Colour of the Args cursor.
C   XCEN    (R)   X coord. of the centre of the image.
C   YCEN    (R)   Y   "  . "   "    "    "   "    "  .
C
C  Returned;
C   AXIS    (RA)  Array to hold the 4 extracted profiles.
C   NAXIS   (I)   Max. permitted no. of points in each
C                 profile (array size = 4*NAXIS).
C   PTS     (IA)  No. of points extracted for each profile.
C   STATUS  (I)   Return status;
C                 = 0 - Axes extracted.
C                 = 1 - Axes not extracted.
C
C  Subroutines called;
C   E2D:-        IRONX1, SECTOR.
C   Interfaces:- YESNO, OUTPUT.
C
C  Structure:-
C   Setup an initial cursor position.
C   put up cursor & obtain required values.
C   if (wish to proceed to extract axes)
C     do for each axis
C       convert the parameters returned by the cursoring
C       routine into the values required by the sector
C       extraction routine.
C       extract the axis.
C       copy the extracted axis into the return array.
C     end do
C   else
C     set status.
C   end if
C
C  A C Davenhall./ROE/                                  5/7/82.
C-
      INTEGER IXEXT,IYEXT,PLANE,STATUS
      INTEGER PTS(4)
      REAL IMAGE(IXEXT,IYEXT)
      REAL AXIS(1024,4)
      REAL XCEN,YCEN
      CHARACTER COLOUR*1
C
      REAL THETA,PHI,A,B,LEN,THETAA,RHTGLE
C
C    A right angle expressed in radians.
C
      PARAMETER (RHTGLE=1.57080E0)
C
      REAL X2,Y2,X3,Y3
      REAL AXIS1(1024)
      INTEGER STAT1,NPTS,MINEXT
      CHARACTER REPLY*1
C
C
C    Setup the initial cursor position.
C
      THETA=0.0E0
      PHI=2.0E-1
      MINEXT=MIN(IXEXT,IYEXT)
      A=FLOAT(IXEXT)/3.0E0
      B=FLOAT(IYEXT)/4.0E0
C
C    Put up the cursor with these initial values and
C    return the desired shape and orientation.
C
      CALL OUTPUT (' Position cursor over the required axes.',STAT1)
      CALL IRONX1 (IXEXT,IYEXT,PLANE,COLOUR,XCEN,YCEN,THETA,
     :             PHI,A,B)
C
C    Proceed to extract axes if the user so desires.
C
      STAT1=0
      CALL YESNO (' Proceed to extract axes?','Y',REPLY,
     :             STAT1)
      IF (REPLY.EQ.'Y'.AND.STAT1.EQ.0) THEN
        CALL OUTPUT (' Please wait. Axes being extracted.',STAT1)
        STATUS=0
C
C    Extract each axis in turn.
C
        DO I=1,4
C
C    Compute the coordinates required for each sector from
C    the values returned by the cursoring routine.
C
C    First choose the appropriate length depending on whether
C    or not a major or minor axis is been processed.
C
          IF (I.EQ.1.OR.I.EQ.3) THEN
            LEN=A
          ELSE
            LEN=B
          END IF
C
C    Now compute the orientation of the clockmost arm of this
C    axis.
C
          THETAA=THETA+(RHTGLE*FLOAT(I-1))
C
C    Finally compute the coords. of the extrema of the 2 arms,
C    which can be passed to the sector extracting 
C    routine.
C
          X2=XCEN+(LEN*COS(THETAA))
          Y2=YCEN+(LEN*SIN(THETAA))
          X3=XCEN+(LEN*COS(THETAA+PHI))
          Y3=YCEN+(LEN*SIN(THETAA+PHI))
C
C    Extract the sector.
C
          CALL SECTOR (IMAGE,IXEXT,IYEXT,XCEN,YCEN,
     :                    X2,Y2,X3,Y3,AXIS1,NPTS)
C
C    Copy the extracted sector into the return array.
C
          DO J=1,NPTS
            AXIS(J,I)=AXIS1(J)
          END DO
          PTS(I)=NPTS
        END DO
      ELSE
C
C    Decided not to extract the sectors, 
C    set return status accordingly.
C
        STATUS=1
      END IF
      END
