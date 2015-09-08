      SUBROUTINE COPY(INPUT,OUTPUT,N)
C
C     Copies one array to another. Should really be implemented
C     using the MOVC instruction (allows 64k to be transferred
C     at a time). It's up to you if you want to do this.
C
      IMPLICIT  INTEGER(A-Z)
      LOGICAL*1 INPUT(N),OUTPUT(N)
C
      DO 20 I=1,N
         OUTPUT(I)=INPUT(I)
   20 CONTINUE
C
      RETURN
      END
