	SUBROUTINE DECODE(RAS,DECS,IHRAS,IAMAS,SRAS,IDDECS,IMDECS,SDECS
     :	,FLAG)

C+	$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C               
C	           *********************
C                  * SUBROUTINE DECODE *
C                  *********************
C
C
C
C	This program takes in the right ascension and declination
C	of a object in radians and converts from radians to hours
C	minutes seconds for right ascension and degrees minutes
C	seconds for declination.
C
C
C  	   INPUT PARAMETERS
C	   ----------------
C
C		RAS = Right ascension (in radians)
C
C              DECS = Declination (in radians)
C
C
C
C	  OUTPUT PARAMETERS
C         -----------------
C
C    	  	IHRAS = The "hours part" of the right ascension
C
C		IAMAS = The "minutes part" of the right ascension
C
C		 SRAS = The "seconds part" of the right ascension
C
C	       IDDECS = The "degrees part" of the declination
C
C	       IMDECS = The "minutes part" of the declination
C
C               SDECS = The "seconds part" of the declination
C
C
C
C    		
C	  STARLINK PARAMETERS
C         -------------------
C
C			None
C
C
C
C     	  CALLS
C         -----
C
C         		None
C
C
C	  NOTES
C         -----
C
C               	None
C
C
C
C         WRITTEN BY
C         ----------
C
C
C             			BRIAN HARRISON
C
C
C			Version 1		July 1982
C 
C-	$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


	TWOPI=6.2831853071
	FLAG=0.0
C
C        Test to see if DECS is negative, if it is then set FLAG=1.0
C        and change the sign of DECS.
C
	IF (DECS.LT.0.0) THEN
	  FLAG=1.0
	  DECS=-DECS
	ENDIF
C
C	Convert RAS and DECS to degrees
C
	RAS=RAS*360.0/TWOPI
	DECS=DECS*360.0/TWOPI
	RAS=RAS/15.0
C
C	Convert to degrees, minutes,seconds
C
	HRAS=INT(RAS)
	TEMP=RAS-HRAS
	TEMP=TEMP*60.0
	AMAS=INT(TEMP)
	TEMP=TEMP-AMAS
	SRAS=TEMP*60.0
	DDECS=INT(DECS)
	TEMP=DECS-DDECS
	TEMP=TEMP*60.0
	AMDECS=INT(TEMP)
	TEMP=TEMP-AMDECS
	SDECS=TEMP*60.0
C
C 	Make the degrees minutes seconds into integers 
C	
	IHRAS=IFIX(HRAS)
	IAMAS=IFIX(AMAS)
	IDDECS=IFIX(DDECS)
	IMDECS=IFIX(AMDECS)
C
C	Return to the main program
C
	END
