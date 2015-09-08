        SUBROUTINE DMATINV (A,N,M)
C+
C       DMATINV.
C
C       Subroutine to invert a double precision matrix.
C
C  Given;
C   A  (RA)  Matrix to be inverted (2D array).
C   N  (I)   No. of rows & cols in matrix to be inverted.
C   M  (I)   Size of each side of array A (.ge.N)
C
C  Returned;
C   A  (RA)  Inverted matrix.
C
C  A C Davenhall./UCL/                                1976.
C  A C Davenhall./St Andrews/  {Modified}             Spring 79.
C  A C Davenhall./ROE/         {   "    }             5/8/82.
C-
        INTEGER N,M
	DOUBLE PRECISION A(M,M),T,ONE
	DATA ONE,IONE/1.0D0,1/
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
