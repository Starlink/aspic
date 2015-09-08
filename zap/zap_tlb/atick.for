      SUBROUTINE ATICK (ID,X,Y,KN,NUM)
C
C DRAW A TICK ON ARGS AT POSITION (X+KN*2,Y) (USER UNITS)
C  (ID IS ID OF IMAGE) AND NUMBERS IT IF NUM IS +VE.
C
      INTEGER PX,PY,STATUS
      CHARACTER*72 TEXT
      REAL X,Y,UX,UY,DX,DY,XVAL(2),YVAL(2)
      KX = KN*2
      KXA = KN*3
      KY = KN
      CALL ARGS_UTOP (ID,X,Y,PX,PY,STATUS)
      CALL ARGS_PTOU (ID,PX+KX,PY+KY,UX,UY,STATUS)
      DXA = UX - X
      DY = UY - Y
      CALL ARGS_PTOU(ID,PX+KXA,PY,UX,UY,STATUS)
      DXB = UX - X
      XVAL(1) = X + DXA
      YVAL(1) = Y
      XVAL(2) = X + DXB
      YVAL(2) = Y
      CALL ARGS_POLYL (ID,2,XVAL,YVAL,STATUS)
      XVAL(1) = X + DXA
      YVAL(1) = Y
      XVAL(2) = X + DXA
      YVAL(2) = Y + DY
      CALL ARGS_POLYL (ID,2,XVAL,YVAL,STATUS)
C
C
      IF (NUM.GT.0) THEN
         DO K = 1,72
            TEXT(K:K) = ' '
         ENDDO
         WRITE(TEXT,900)NUM
  900    FORMAT(I10)
         KSHIFT = 0
         DO K = 1,9
            IF(TEXT(K:K).EQ.' ') KSHIFT = K
         ENDDO
         DO K = 1,10
            J = K + KSHIFT
            TEXT(K:K) = TEXT(J:J)
         ENDDO
         CALL ARGS_UTOP(ID,X,Y,PX,PY,STATUS)
         CALL ARGS_PTOU(ID,PX+3,PY+3,UXA,UYA,STATUS)
         KX = UXA
         KY = UYA
         CALL ARGTXT(TEXT,'B',0,KX,KY,0)
      ENDIF
C
      END
 
 
 
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
