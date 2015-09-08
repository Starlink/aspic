	SUBROUTINE BUBBLE (X,M,NPTS)
C+
C	 Subroutine to perform a simple bubble sort
C	 on a single array of floating point numbers.
C
C	 X - array of real numbers to be sorted.
C	 M - size of array X.
C	 NPTS - No. of pts. to be sorted.
C
C	 Subroutines called:-
C	   OUTPUT
C
C	 A C Davenhall.  ROE.		1/10/81.
C-
	REAL X(M)
	REAL DUMMY
	INTEGER M,NPTS,ISTAT,NN,JJ
	CHARACTER BUFF*80
C
C	 Check no. of pts. is less than array size & if not
C	 set no. of pts. equal to array size and print message.
C
	IF (NPTS.GT.M) THEN
	  BUFF=' ***ERROR***; No. of pts. is larger than array size.'
	  CALL OUTPUT (BUFF,ISTAT)
	  WRITE(BUFF,2000) NPTS,M
 2000	FORMAT(10X,'No. of pts. = ',I5,2X,'Array size = ',I5)
	  CALL OUTPUT (BUFF,ISTAT)
	  BUFF='     No. of pts. set equal to array size.'
	  CALL OUTPUT (BUFF,ISTAT)
	  NPTS=M
	END IF
C
C	 Now do Bubble sort.
C
	NN=NPTS-1
	DO I=1,NN
	  DO J=1,NN
	    JJ=J+1
	    IF (X(JJ).LT.X(J)) THEN
	      DUMMY=X(J)
	      X(J)=X(JJ)
	      X(JJ)=DUMMY
	    END IF
	  END DO
	END DO
	END
