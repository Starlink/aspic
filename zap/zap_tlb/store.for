      SUBROUTINE STORE(AIN,NX,NY,ANEW,OUT,KLEN)
C
C
C
      REAL AIN(NX,NY),ANEW(3,NY),OUT(NX,KLEN)
C
C
C
      J = 0
      DO K = 1,NY
         IF (ANEW(3,K).GT.0.5) THEN
            J = J + 1
            DO L = 1,NX
               OUT(L,J) = AIN(L,K)
            ENDDO
            DO L = 1,2
               OUT(L+5,J) = ANEW(L,K)
            ENDDO
         ENDIF
      ENDDO
C
C
C
      END
 
 
 
