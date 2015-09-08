      SUBROUTINE CHARG1 (PLANE,COLOUR,STRING,XPOS,YPOS,SIZCHA)
C+
C     CHARG1.
C
C     Subroutine to plot a character string on the ARGS.
C
C  Given;
C   PLANE   (I)  Args overlay plane to which the string is to be
C                written
C   COLOUR  (C)  Colour in which the string is to appear.
C   STRING  (C)  Character string to be plotted.
C   XPOS    (I)  X coord of bottom L.H. corner of string position.
C   YPOS    (I)  Y   "   "    "    R.H.   "    "    "       "    .
C                (XPOS and YPOS are expressed in Args coords).
C   SIZCHA  (I)  Size of characters required.
C
C  Returned;
C   None.
C
C  Subroutines called;
C   E2DLIB:       UPPCAS.
C   ARGS:         SRINIT, ARGS_PICWRT, ARGS_OVWRT, ARGS_OVCOL, ARGS_OVGEN.
C  Fings:    ARGS, DEVSPE, VUPORT, WINDOL, CHASIZ,
C                 MOVTO2, CHAESC, CHAHOL, DEVEND.
C
C  Structure:-
C   Perform lower to upper case conversion.
C   select ARGS.
C   Setup vuport.
C   Setup window.
C   Setup character size.
C   move to the appropriate position.
C   print string.
C   teminate plotting.
C
C  A C Davenhall./ROE/                                      4/6/82.
C  A C Davenhall./ROE/  {Modified}                         28/7/82.
C-
      CHARACTER STRING*(*), COLOUR*(1)
      INTEGER XPOS,YPOS,SIZCHA,PLANE
C
C
C    Convert the character string to upper case.
C
      CALL UPPCAS (STRING)
C
C    Select the ARGS as the target for Fings.
C
      CALL SRINIT (0,.FALSE.,IFAIL)
      CALL ARGS
      CALL ARGS_PICWRT
      CALL DEVSPE (9600)
C
C    Write Fings to the selected overlay plane.
C
      CALL ARGS_OVWRT (PLANE)
      CALL ARGS_OVCOL (PLANE,COLOUR)
      CALL ARGS_OVGEN ('W')
C
C    Set viwport.
C
      CALL VUPORT (0.0E0,1.0E0,0.0E0,1.0E0)
C
C    Setup window.
C
      CALL WINDOL (0.0E0,5.11E2,0.0E0,5.11E2)
C
C    Select the character size for plotting.
C
      CALL CHASIZ (FLOAT(SIZCHA))
C
C   Move to the selected position and plot the string.
C
      CALL MOVTO2 (FLOAT(XPOS),FLOAT(YPOS))
      CALL CHAESC ('*')
      CALL CHAHOL (%REF(STRING)//'*.')
C
C    Stop plotting.
C
      CALL DEVEND
      END
