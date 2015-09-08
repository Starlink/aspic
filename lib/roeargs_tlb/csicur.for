      SUBROUTINE ARGS_CSICUR(TYPE,NPOS,RADIUS,XPOS,YPOS,CURCOL,
     :                 INDCOL,RET,RDATA)
*+
*   ARGS_CSICUR
*
*   Drives ARGS trackerball cursor, of defined shape and size.
*   The size can be altered by the trackerball buttons.
*   A set of (X,Y) coordinates and corresponding cursor sizes
*   is returned.
*
*   This routine uses plane 8 for the overlay cursor and writes
*   its indicator into plane 9.
*
*   Given         (arguments)
*   TYPE      I    = 1 for circle, 2 for square, 3 for open cross.
*   NPOS      I    maximum no. of positions to be obtained.
*   RADIUS    I    radius of circle or half-length of square.
*   XPOS      I    given X position of cursor
*   YPOS      I    given Y position of cursor
*   CURCOL    C    cursor colour
*   INDCOL    C    indicator colour
*
*   Returned      (arguments)
*   RET       I    actual no. of positions defined
*   RDATA     IA   (X,Y) positions and sizes of cursors returned.
*
*   Subroutines called :
*   args_ovwrt,args_ovcol,args_ovgen: ROEARGS
*   LOAD_UCUR,WRITE_UCUR,RUN_UCUR,READ_UCUR  : ROEARGS
*
*   D.King/RGO/1981
*   Alterations to use ROEARGS by J.A.Cooke/UOE/14Dec81
*-
      CHARACTER*1 CURCOL,INDCOL
      INTEGER TYPE,NPOS,RADIUS,XPOS,YPOS,RET
      INTEGER*2 DATA(5)
      INTEGER*2 OUTDATA(769)
      INTEGER RDATA(3,NPOS)
*
*   trap input conditions
*
      IF (RADIUS.LE.0) RADIUS=10
      IF (XPOS.LE.0.OR.XPOS.GE.511) XPOS=255
      IF (YPOS.LE.0.OR.YPOS.GE.511) YPOS=255
      IF (TYPE.LE.0.OR.TYPE.GT.3) TYPE=1
      MPOS=NPOS
      IF (NPOS.LE.0.OR.NPOS.GE.768) MPOS=1
*
*   Initialize cursor, and start cursor software running inside ARGS.
*
      DATA(1)=TYPE
      DATA(2)=MPOS
      DATA(3)=RADIUS
      DATA(4)=XPOS
      DATA(5)=YPOS
      call args_ovwrt(8)
      call args_ovcol(8,curcol)
      call args_ovcol(9,indcol)
      call args_ovgen('W')
      CALL LOAD_UCUR
      CALL WRITE_UCUR(DATA)
*
*   Operate cursor and receive reply
*
      CALL RUN_UCUR
      CALL READ_UCUR(OUTDATA)
*
*   Copy cursor returned values into output array
*
      DO J=1,MPOS
         DO I=1,3
            RDATA(I,J)=OUTDATA((J-1)*3+I)
         END DO
      ENDDO
      RET=OUTDATA(769)

      END
