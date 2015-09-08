      SUBROUTINE HGRAM(KDATA,KX,KY,LX,LY,KMIN,KGRAM,NUMBIN,INVAL)
      INTEGER LX(2),LY(2)
      INTEGER*2 KDATA(KX,KY)
      INTEGER KGRAM(NUMBIN)
C
      DO L = 1,NUMBIN
         KGRAM(L) = 0
      ENDDO
      DO J = LX(1),LX(2)
         DO K = LY(1),LY(2)
            IF(KDATA(J,K).NE.INVAL) THEN
               L = KDATA(J,K) - KMIN + 1
               IF(L.GE.1.AND.L.LE.NUMBIN)KGRAM(L) = KGRAM(L) + 1
            ENDIF
         ENDDO
      ENDDO
C
C
C
      END
 
 
 
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      *************
C      *           *
C      * S/R RANGE *
C      *           *
C      *************
C
C SUBROUTINE RANGE GETS MIN AND MAX VALUES IN A PART OF A 2-D ARRAY
C
C -------------------------------------------------------------
C
C
