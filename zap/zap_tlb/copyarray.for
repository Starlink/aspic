      SUBROUTINE COPYARRAY(IDSA,IDSB,SS,ES,SR,ER,ID1,ID2,IDA1)
      REAL*4 IDSA(ID1,ID2),IDSB(IDA1)
      INTEGER SS,ES,SR,ER
      N = 1
      DO K = SR,ER
      DO J = SS,ES
      IDSB(N) = IDSA(J,K)
      N = N + 1
      ENDDO
      ENDDO
      RETURN
      END
