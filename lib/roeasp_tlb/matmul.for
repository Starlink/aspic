	SUBROUTINE MATMUL (A,B,C,N,M)
C+
C        Subroutine to multiply a matrix by a vector.
C
C        Given;
C        A  (R)  Square matrix to be multiplied.
C        B  (R)  Vector to multiply by.
C        N  (I)  Size of vector B & square matrix A.
C        M  (I)  Size of arrays B & A.
C
C        Returned;
C        C  (R)  Vector containing product of A & B.
C
C        A C Davenhall./ROE/                     Starlink; 21/1/82.
C-
	DIMENSION A(M,M),B(M),C(M)
	DATA ZERO/0.0E0/
	DO 1 I=1,N
	CC=ZERO
	DO 2 K=1,N
 2	CC=CC+(A(I,K)*B(K))
 1	C(I)=CC
	RETURN
	END
