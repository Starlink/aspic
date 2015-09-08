      SUBROUTINE AXIS (XMIN,XMAX,YMIN,YMAX,LABELX,LABELY)
C+
C      Subroutine to draw a set of axes stretching from
C      point (XMIN,YMIN) to (XMAX,YMAX) using FINGS.
C
C  Given;
C  XMIN  (R)  Min. X coord.
C  XMAX  (R)  Max. X   "  .
C  YMIN  (R)  Min. Y   "  .
C  YMAX  (R)  Max. Y   "  .
C  LABELX (C) X label.
C  LABELY (I0 Y label.
C
C  Returned; -.
C
C  Subroutines called;
C  XTICK, YTICK, DRABOXR,
C  MOVTO2,CHAESC,CHAHOL,CLICTL.
C
C  A C Davenhall./ROE/                      1/2/82.
C-
      REAL XMIN,XMAX,YMIN,YMAX
      CHARACTER LABELX*(*), LABELY*(*)
      REAL XPOS,YPOS
      CALL CLICTL (1)            ! Enable clipping.
      CALL XTICK (XMIN,XMAX,YMIN,YMAX)
      CALL YTICK (XMIN,XMAX,YMIN,YMAX)
      CALL DRABOXR (XMIN,YMIN,XMAX,YMAX)
      CALL CHAESC ('$')
C
C      Label X axis.
C
      XPOS=XMIN+((XMAX-XMIN)*0.4)
      YPOS=YMIN-((YMAX-YMIN)*0.09)
      CALL MOVTO2 (XPOS,YPOS)
      CALL CHAHOL (%REF(LABELX)//'$.')
C
C      Label Y axis.
C
      YPOS=(YMAX+YMIN)*0.5
      XPOS=XMIN-((XMAX-XMIN)*0.19)
      CALL MOVTO2 (XPOS,YPOS)
      CALL CHAHOL (%REF(LABELY)//'$.')
C
      CALL CLICTL (0)            ! Disable clipping.
      END
