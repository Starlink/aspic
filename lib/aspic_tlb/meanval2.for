	SUBROUTINE MEANVAL2(INPUT,ISIZE,MEAN)
C
C+	ROUTINE TO CALCULATE THE MEAN VALUE OF A REAL ARRAY
C
	REAL INPUT(ISIZE),MEAN
	MEAN=0.0
	DO I=1,ISIZE
		MEAN=MEAN+INPUT(I)
	ENDDO
	MEAN=MEAN/ISIZE
	RETURN
	END