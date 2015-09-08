      SUBROUTINE AZOOM(IX,IY,IZX,IZY)
C
C+     THIS SUBROUTINE ZOOMS THE ARGS BY FACTORS OF IZX,IZY
C      ABOUT THE POINT IX,IY
C      ALL FOUR PARAMETERS ARE INTEGERS
C
C      THE WORK WAS DONE BY D J KING , TIDIED UP BY K F HARTLEY
C
      INTEGER*4 IX,IY,IZX,IZY
C
C      FIRST VALIDATE PARAMETERS
C
      IF (IX.LT.1.OR.IX.GT.511) RETURN
      IF (IY.LT.1.OR.IY.GT.511) RETURN
      IF (IZX.LT.1.OR.IZX.GT.15) RETURN
      IF (IZY.LT.1.OR.IZY.GT.15) RETURN
C
C      THEN SET UP THE CURRENT POINTER
C
        CALL APUT1('2401'X)
        CALL APUT1('01F8'X)
        CALL APUT1('0008'X)
        CALL APUT1('0000'X)
        CALL AFLUSH(-1)
	CALL APUT1('1700'X)
        CALL APUT1(IX)
	CALL APUT1('2700'X)
	CALL APUT1(IY)
	CALL AFLUSH(-1)
C
C      NOW DO THE ZOOM
C
        CALL APUT1('2401'X)
        CALL APUT1('01F8'X)
        CALL APUT1('0000'X)
        CALL APUT1('0000'X)
        CALL AFLUSH(-1)
	CALL APUT1('5008'X)
	CALL APUT1(((IZY-1)*256)+IZX-1)
	CALL AFLUSH(-1)
      RETURN
	END
