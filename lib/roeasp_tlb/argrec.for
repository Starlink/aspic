      SUBROUTINE ARGREC (XBASE,YBASE,XTOP,YTOP,STATUS)
C+
C     ARGREC.
C
C     Subroutine to draw a rectangle defined in pixel coords.
C     on the Args, overlayed on top of the current image.
C     This is acheived by translating the pixel coords. into
C     Args coords. using the Args database. A Fings window
C     equivalent to Args coords. spanning the screen is
C     assumed (and required) to have been previously
C     set up.
C
C  Given;
C   XBASE  (I)  X coord. of lower left hand corner of box.
C   YBASE  (I)  Y   "  . "    "    "    "     "    "   " .
C   XTOP   (I)  X   "  . "  upper right "     "    "   " .
C   YTOP   (I)  Y   "  . "    "    "    "     "    "   " .
C
C  Returned;
C   STATUS (I)  Return status. = 0 for successful return, otherwise
C               non-zero.
C
C  Subroutines called;
C   Args:-     ARGS_NUMIM, ARGS_PTOA.
C   Graphics:- DRABOX.
C 
C  Structure:-
C   Open Args database.
C   Convert pixel coords. to Args coords.
C   If status return from conversion = 0
C     draw box on Args.
C   end if.
C
C  A C Davenhall./ROE/                                 19/8/82.
C-
      INTEGER XBASE,YBASE,XTOP,YTOP,STATUS
C
      INTEGER ARGIMG
      INTEGER XBASEA,YBASEA,XTOPA,YTOPA
      INTEGER STAT1,STAT2
C
C
C    Attempt to open the Args database and obtain the number
C    (identifier) of the most recently displayed image.
C
      CALL ARGS_NUMIM (ARGIMG)
C
C    Attempt to convert pixel coords. to Args coords.
C
      CALL ARGS_PTOA (ARGIMG,XBASE,YBASE,XBASEA,YBASEA,STAT1)
      CALL ARGS_PTOA (ARGIMG,XTOP,YTOP,XTOPA,YTOPA,STAT2)
      STATUS=MAX(STAT1,STAT2)
C
C    Draw the box if the conversion has been carried out Ok.
C 
      IF (STATUS.EQ.0) CALL DRABOX (XBASEA,YBASEA,XTOPA,YTOPA)
      END
