      SUBROUTINE GETDAT(DA,NITEMA,LSTLA,DB,NITEMB,KPARA,KPARB,X,Y)
C
C
C
      REAL DA(NITEMA,LSTLA),DB(NITEMB,LSTLA),X(LSTLA),Y(LSTLA)
C
C
C
      DO K = 1,LSTLA
         KX = 5 + KPARA
         X(K) = DA(KX,K)
         KY = 5 + KPARB
         Y(K) = DB(KY,K)
      ENDDO
C
C
C
      END
 
 
 
