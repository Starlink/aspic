**********************************************************************
*
*
* This subroutine reads the video look up table from the ARGS
* and returns the R,G,B values in the array K(3,256)
*
*
*********************************************************************


	SUBROUTINE ASP_GETLUT(K)
	BYTE VLUT(4,256)
	INTEGER K(3,256)
*
* Read LUT from ARGS into array VLUT(4,256)
*
	CALL ASP_VLTGET(VLUT)
*
* Transfer the R,G,B information into the array K
*
	DO I=1,256
		DO J=1,3
		K(J,I)=VLUT(J,I)
*
* Treat the byte array VLUT as unsigned
*
		IF (K(J,I).LT.0) K(J,I)=K(J,I)+256
		ENDDO
	ENDDO
	RETURN
	END
