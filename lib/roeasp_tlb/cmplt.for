      SUBROUTINE CMPLT (POINTS,RADIUS,LOGI,PARAM,DEVICE,TITLE,TITLE1,
     :                  XAXIS,YAXIS,IWORK1,IWORK2,IWORK3,
     :                  RWORK1,RWORK2,RWORK3)
C+
C     CMPLT.
C
C     Subroutine to plot the observed profile of a disk galaxy
C     with a profile fitted to it superposed. The fitted profile
C     consists of a disk and bulge component; these are plotted
C     separately and also as a composite profile.
C
C  Given;
C   POINTS  (I)  No. of points in the observed profile.
C   RADIUS  (RA) Radii of points in the observed profile.
C   LOGI    (RA) Log intensity of points in the observed profile.
C   PARAM   (RA) Array holding parameters for fitted profile.
C   DEVICE  (I)  Graphics device to be used;
C                 = 1  - T4010.
C                 = 2  - Args.
C                 = 3  - Versatec.
C   TITLE   (C)  Title of the graph.
C   TITLE1  (C)  Second title for graph.
C   XAXIS   (C)  Label for the X axis.
C   YAXIS   (C)    "    "   "  Y  "  .
C
C  Used;
C   IWORK1  (RA) Array, size=POINTS.
C   IWORK2  (RA)   "  ,  "     "   .
C   IWORK3  (RA)   "  ,  "     "   .
C   RWORK1  (RA)   "  ,  "     "   .
C   RWORK2  (RA)   "  ,  "     "   .
C   RWORK3  (RA)   "  ,  "     "   .
C
C  Returned;
C   None.
C
C  Subroutines called;
C   Graphics:-   WINDO, GDEVI1, DRAXIS,TYPLO1, QLOT, STOPLOT.
C   Fings:-      CLICTL, BROKEN, MOVTO2, CHAESC, CHAHOL.
C
C  Functions called;
C   E2D:-  BULGE, DISK.
C
C  A C Davenhall./ROE/                                      10/7/83.
C-
      IMPLICIT NONE
C
      INTEGER POINTS,DEVICE
      REAL RADIUS(POINTS),LOGI(POINTS),IWORK1(POINTS),
     :     IWORK2(POINTS),IWORK3(POINTS),RWORK1(POINTS),
     :     RWORK2(POINTS),RWORK3(POINTS)
      REAL PARAM(10)
      CHARACTER*(*) TITLE,TITLE1,XAXIS,YAXIS
C
C    Declare functions.
C
      REAL BULGE,DISK
C
      REAL XMIN,XMAX,YMIN,YMAX,RADPT,XPOS,YPOS
      REAL WORK1,WORK2,WORK3
      REAL A(10)
      INTEGER INDEX,PTS1,PTS2,PTS3
C
C
C    Define the plotting space using the observed dataset.
C
      CALL WINDO (RADIUS,LOGI,POINTS,POINTS,XMIN,XMAX,YMIN,YMAX,
     :            .FALSE.)
C
C    Initialise graphics.
C
      CALL GDEVI1 (DEVICE,.TRUE.)
C
C    Setup the range for plotting and draw a set of axes.
C
      CALL BROKEN (0)
      CALL DRAXIS (XMIN,XMAX,YMIN,YMAX,XAXIS,YAXIS)
      CALL CLICTL (1)
C
C    Depending on the number of points in the observed profile
C    plot it as either individual symbols (diamonds) or a 
C    "histogram".
C
      IF (POINTS.LE.50) THEN
        CALL TYPLO1 (1,6)
      ELSE
        CALL TYPLO1 (3,0)
      END IF
      CALL QLOT (RADIUS,LOGI,POINTS,POINTS)
C
C    Generate a set of points corresponding to the fitted bulge 
C    and disk and to the composite profile.
C
      A(1)=1.0E1**PARAM(3)
      A(2)=PARAM(4)
C
      PTS1=0
      PTS2=0
      PTS3=0
C
      DO INDEX=1,POINTS
        RADPT=RADIUS(INDEX)
        WORK1=BULGE(RADPT,PARAM,10)
        WORK2=DISK(RADPT,A,10)
        WORK3=ALOG10(WORK2+(1.0E1**WORK1))
        WORK2=ALOG10(WORK2)
C
        IF (WORK1.GE.YMIN.AND.WORK1.LT.YMAX) THEN
          PTS1=PTS1+1
          IWORK1(PTS1)=WORK1
          RWORK1(PTS1)=RADPT
        END IF
C
        IF (WORK2.GE.YMIN.AND.WORK2.LT.YMAX) THEN
          PTS2=PTS2+1
          IWORK2(PTS2)=WORK2
          RWORK2(PTS2)=RADPT
        END IF
C
        IF (WORK3.GE.YMIN.AND.WORK3.LT.YMAX) THEN
          PTS3=PTS3+1
          IWORK3(PTS3)=WORK3
          RWORK3(PTS3)=RADPT
        END IF
      END DO
C
C    Plot the individual components as dotted lines.
C 
      CALL TYPLO1 (2,0)
      CALL BROKEN (1)
      IF (PTS1.GT.0) CALL QLOT (RWORK1,IWORK1,PTS1,PTS1)
      IF (PTS2.GT.0) CALL QLOT (RWORK2,IWORK2,PTS2,PTS2)
C
C    Plot the composite profile as a dashed line.
C
      CALL BROKEN (3)
      IF (PTS3.GT.0) CALL QLOT (RWORK3,IWORK3,PTS3,PTS3)
C
C    Put up the title.
C
      XPOS=XMIN+(2.0E-1*(XMAX-XMIN))
      YPOS=YMAX+(9.5E-2*(YMAX-YMIN))
      CALL MOVTO2 (XPOS,YPOS)
      CALL CHAESC ('$')
      CALL CHAHOL (%REF(TITLE)//'$.')
      YPOS=YMAX+(4.5E-2*(YMAX-YMIN))
      CALL MOVTO2 (XPOS,YPOS)
      CALL CHAHOL (%REF(TITLE1)//'$.')
C
C    Terminate plotting.
C
      CALL STOPLOT
      END
