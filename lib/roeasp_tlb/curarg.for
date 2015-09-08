      SUBROUTINE CURARG (XPOS,YPOS)
C+
C     CURARG.
C
C     Subroutine to display a cross hair cursor on the Args
C     and return values in pixel coordinates.
C     This is very naughty and should only be used in exceptional
C     circumstances.
C
C  Given;
C   XARG   (I)  X starting coord. for cursor.
C   YARG   (I)  Y    "       "  .  "    "   
C
C  Returned;
C   XARG   (I)  Selected X coord.
C   YARG   (I)     "     Y   "  .
C
C  Note; All Args coords. lie in the range 0 - 511.
C
C  Subroutines called;
C   ...
C
C  A C Davenhall./ROE/                                           21/2/83.
C-
      INTEGER XPOS,YPOS
C
      INTEGER B1,B2,B3,B4,ARGSTT
C
C
C    Force the cursor to start inside the image.
C
      IF (XPOS.LT.0.OR.XPOS.GT.511.OR.YPOS.LT.0.OR.YPOS.GT.511) THEN
        XPOS=255
        YPOS=255
      END IF
C
C    Put up cursor and obtain values.
C
      CALL SRINIT (0,.FALSE.,ARGSTT)
C
      CALL ARGS_CURS ('+')
C
      CALL ARGS_CURP (0,XPOS,YPOS)
C
      CALL ARGS_CURC ('W')
C
      CALL ARGS_TBCL (0)
C
      CALL ARGS_LAMPS (1,0,0,0)
C
      CALL ARGS_TBCX (XPOS,YPOS,B1,B2,B3,B4)
C
      CALL ARGS_LAMPS (0,0,0,0)
C
      CALL ARGS_CURS ('0')
C
C    Force returned coords. o lie in range 0 - 511.
C
      XPOS=MAX(0,XPOS)
      XPOS=MIN(XPOS,511)
      YPOS=MAX(0,YPOS)
      YPOS=MIN(YPOS,511)
C
      END
