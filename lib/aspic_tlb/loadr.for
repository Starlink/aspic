      SUBROUTINE LOADR(ARRY,NUM)
C
      IMPLICIT INTEGER (A-Z)
      REAL ARRY(3,2,NUM),RNUM(3)
      CALL WRUSER('INPUT IMAGE POINTS',STATUS)
      DO J = 1,NUM
300   CALL RDKEYR('POINT',.FALSE.,3,RNUM,AVAL,STATUS)
      IF (AVAL.LT.3) THEN
      CALL WRERR('REJECT')
      CALL CNPAR('POINT',STATUS)
      GO TO 300
      ENDIF
      ARRY(1,2,J) = RNUM(1)
      ARRY(2,2,J) = RNUM(2)
      ARRY(3,2,J) = RNUM(3)
      CALL CNPAR('POINT',STATUS)
      ENDDO
C
      CALL WRUSER('OUTPUT IMAGE POINTS',STATUS)
      DO J = 1,NUM
400   CALL RDKEYR('POINT',.FALSE.,3,RNUM,AVAL,STATUS)
      IF(AVAL.LT.3) THEN
      CALL WRERR('REJECT')
      CALL CNPAR('POINT',STATUS)
      GO TO 400
      ENDIF
      ARRY(1,1,J) = RNUM(1)
      ARRY(2,1,J) = RNUM(2)
      ARRY(3,1,J) = RNUM(3)
      CALL CNPAR('POINT',STATUS)
      ENDDO
C
      RETURN
      END
