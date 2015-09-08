      SUBROUTINE BLOCKIT(IDA,IDB,BLK,I1,I2,I3)
C
C    SMOOTHING ROUTINE
C
      REAL*4 IDA(I1,I2),IDB(I3)
	INTEGER*2 BLK
      AB = BLK * BLK
      KK = 1
      DO J = 1,I2-BLK+1,BLK
      DO K = 1,I1-BLK+1,BLK
      A= 0.0
      DO L = 0,BLK-1
      DO M = 0,BLK-1
      A = A + IDA(K+M,J+L)
      ENDDO
      ENDDO
      IDB(KK) = A/AB
      KK = KK + 1
      ENDDO
      ENDDO
      RETURN
      END
 
