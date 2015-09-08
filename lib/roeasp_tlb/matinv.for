	SUBROUTINE MATINV (A,N,M)
C+
C        Subroutine to invert a matrix.
C
C        Given;
C        A  (R)  Array to be inverted.
C        N  (I)  Size of matrix being inverted.
C        M  (I)  Size of array A.
C
C        Returned;
C        A  (R)  Inverted matrix.
C
C        A C Davenhall./ROE/                       Starlink;21/1/82.
C-
	DIMENSION A(M,M)
	DATA ONE,IONE/1.0E0,1/
	NN=N-IONE
	DO 1 K=1,N
	T=A(1,K)
	DO 2 J=1,NN
 2	A(J,K)=A(J+IONE,K)/T
	A(N,K)=ONE/T
	DO 3 I=1,N
	IF (I.EQ.K) GOTO 4
	T=A(1,I)
	DO 5 J=1,NN
 5	A(J,I)=A(J+IONE,I)-(A(J,K)*T)
	A(N,I)=-A(N,K)*T
 4	CONTINUE
 3	CONTINUE
 1 	CONTINUE
	RETURN
	END
