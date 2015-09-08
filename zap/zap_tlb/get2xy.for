      SUBROUTINE GET2XY(XPOS,YPOS)
C
C   This subroutine gets 2 pairs of (x,y) co-ordinates from the
C   environment. It use the ARGS cursor and/or RDKEY to do this.
C
C   Parameters :-
C
C      Output   XPOS    A 2 element array which will store the lower
C                       and the higher of the 2 x values input.
C               YPOS    A similar array storing the 2 y values.
C
C
      INTEGER XY(2),XPOS(2),YPOS(2)
      INTEGER X1,X2,Y1,Y2
      CHARACTER*72 TEXT
      CALL SRINIT(0,.FALSE.,ISTAT)
C
C   IF THE ARGS IS AVAILABLE TRY THE CURSOR
C
      IF (ISTAT.EQ.0) THEN
	CALL ARGS_CUROP('1234','W')
	CALL BLABELS(9,'Y','CORNER XY','CORNER XY',
     1'CORNER XY','CORNER XY')
         CALL ASP_XYPOS(X1,Y1,IVAL,ID1)
C
C   IVAL=0 MEANS SUCCESS , ID1 IS THE IMAGE POINTED TO
C
      END IF
C
C   IF NO GOOD TRY ASKING FOR NUMBERS
C
      IF (ISTAT.NE.0.OR.IVAL.NE.0) THEN
        ISTA=2
         DO WHILE (ISTA.GT.1)
            CALL RDKEYI('XYPOS1',.FALSE.,2,XY,I,ISTA)
            CALL CNPAR('XYPOS1',IST)
         END DO
         X1=XY(1)
         Y1=XY(2)
      ELSE
         WRITE (TEXT,'(A,2I5)') 'First set of co-ordinates is ',X1,Y1
         CALL WRUSER(TEXT,ISTAT)
      END IF
C
C   NOW REPEAT FOR SECOND POINT
C
C   ONLY TRY CURSOR IF FIRST WAS OK
C
      IF (ISTAT.EQ.0.AND.IVAL.EQ.0) THEN
         CALL ASP_XYPOS(X2,Y2,IVAL,ID2)
      END IF
      IF (ISTAT.NE.0.OR.IVAL.NE.0) THEN
         ISTA=2
         DO WHILE (ISTA.GT.1)
            CALL RDKEYI('XYPOS2',.FALSE.,2,XY,I,ISTA)
            CALL CNPAR('XYPOS2',IST)
         END DO
         X2=XY(1)
         Y2=XY(2)
      ELSE
         WRITE (TEXT,'(A,2I5)') 'Second set of co-ordinates is',X2,Y2
         CALL WRUSER(TEXT,ISTAT)
      END IF
C
C   Now unscramble the bottom left hand corner and
C   the top right hand one.
C
      XPOS(1)=MIN(X1,X2)
      XPOS(2)=MAX(X1,X2)
      YPOS(1)=MIN(Y1,Y2)
      YPOS(2)=MAX(Y1,Y2)
C
C   IF THE ARGS IS AVAILABLE, DRAW A SQUARE ON IT
C
      IF (ISTAT.EQ.0) THEN
         CALL ARGS_SQUARE(XPOS,YPOS,ID2)
      END IF
C
C   FINALLY, WRITE THE VALUES BACK TO THE ENVIRONMENT
C
      WRITE (TEXT,'(A,2I5)') 'Bottom left hand corner ',
     :                        XPOS(1),YPOS(1)
      CALL WRUSER(TEXT,IST)
      WRITE (TEXT,'(A,2I5)') 'Top right hand corner ',
     :                        XPOS(2),YPOS(2)
      CALL WRUSER(TEXT,IST)
 
	CALL ARGS_CURCL
	CALL ARGS_OVCLR(9)
      END
