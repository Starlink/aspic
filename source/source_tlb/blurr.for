*
*+ BLURR  IMAGEOUT
*
*  CREATE 5 X 5 BLURRING FUNCTION (ROUGHLY GAUSSIAN)
*

      INTEGER IDIMN(2)

      INCLUDE 'INTERIM(FMTPAR)'

      DATA IDIMN/5,5/

      CALL WRIMAG('OUT',FMT_R,IDIMN,2,IPOUT,JSTAT)

      CALL BLURR(%VAL(IPOUT))

      CALL FRDATA(' ',JSTAT)
      END
      SUBROUTINE BLURR(A)
      REAL A(5,5)

      DO J=1,5
         Y=REAL(J-3)
         YSQ=Y*Y
         DO I=1,5
            X=REAL(I-3)
            XSQ=X*X
            RSQ=XSQ+YSQ
            A(I,J)=EXP(-RSQ)
         END DO
      END DO

      S=0.0
      DO J=1,5
         DO I=1,5
            S=S+A(I,J)
         END DO
      END DO

      DO J=1,5
         DO I=1,5
            A(I,J)=A(I,J)/S
         END DO
      END DO
      END
