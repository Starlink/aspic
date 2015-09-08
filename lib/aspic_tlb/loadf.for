      SUBROUTINE LOADF(A,B,R,N1,N2,N3,N4,N5)
      REAL A(N1,N2),B(N3,N4),R(3,2,N5)
      DO I = 1,N5
      R(1,1,I) = B(1,I)
      R(2,1,I) = B(2,I)
      R(3,1,I) = B(3,I)
      R(1,2,I) = A(1,I)
      R(2,2,I) = A(2,I)
      R(3,2,I) = A(3,I)
      ENDDO
      RETURN
      END
