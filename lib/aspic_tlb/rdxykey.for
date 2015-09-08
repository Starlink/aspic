        SUBROUTINE RDXYKEY(RAS,DECS,IOERR)

C
C
C  PURPOSE
C
C		To obtain a set of RA and DEC from the terminal
C		as character strings and convert them to radians.
C
C
C  METHOD
C
C		Obtain RA, DEC from the keyboard if a null entry
C		was made then go back to the main program and set
C		IOERR = -1. If an invalid entry was made then
C		ask user to re enter the RA and DEC again.
C
C  
C		The RA and DEC are read in as character strings
C		in to STARLINK parameters.
C
C
C		This subroutine uses subroutines from the
C		CHART package (RDRA,RDDEC) and this should
C		be consulted to find out the allowed format.
C
C  ARGUMENTS
C
C
C		RAS   
C			The single precision value of RA (in
C			radians )
C
C
C		DECS
C			The single precision value of DEC (in
C			radians )
C
C		IOERR
C			This is used as a flag to indicate if
C			a null entry was made.
C
C
C  CALLS
C	      	THIS PACKAGE:
C				LBGONE,XYPRGG
C
C		STARLINK:
C				RDKEYC, CNPAR, CTOR ,WERR, CTOI
C
C  WRITTEN BY
C
C	B Harrison	RGO		Version 1      July 1982
C
C



	CHARACTER*40 RAIN,DECIN
	DOUBLE PRECISION RA,DEC
C         
C      CONVERSION FACTORS USED THROUGHOUT.       
C     RDSA = RADS. PER SEC. OF ARC      
C     RDST = RADS. PER SEC. OF TIME     
C     RDDG = RADS. PER DEGREE 
C         
      DOUBLE PRECISION TWOPI,HALFPI,RDSA,RDST,RDDG
      COMMON/CONVF/TWOPI,HALFPI,RDSA,RDST,RDDG    
      DATA HALFPI,TWOPI,RDSA,RDST,RDDG/
     1 0.1570796326794897E+1,0.6283185307179586E+1,
     2 0.4848136811095360E-5,0.7272205216643039E-4,
     3 0.1745329251994329E-1/
2	CONTINUE

C
C	RAIN = Character string containing RA
C	DECIN= Character string containing DEC
C
C	Set RAIN and DECIN to equal ' '
C

	RAIN=' '
	DECIN=' '

C
C	Read in charcter string RAIN from the terminal.
C

 	CALL RDKEYC('RA',.FALSE.,1,RAIN,NVAL,IEND1)

        IF (IEND1.NE.0) THEN
          GOTO 5
        END IF

C
C	Read in character string DECIN from the terminal.
C

        CALL RDKEYC('DEC',.FALSE.,1,DECIN,NVAL,IEND2)


        IF (IEND2.NE.0) THEN
          GOTO 5
        END IF

C
C	Convert RAIN, DECIN to radians using RDRA and RDDEC and 
C	store the answers in RA and DEC.
C

        CALL RDRA(RAIN,1,40,RA,IERR1)
	CALL RDDEC(DECIN,1,40,DEC,IERR2)
        CALL CNPAR('RA',ISTAT)
        CALL CNPAR('DEC',ISTAT)

C
C	Go to the end of the subroutine.
C

        GOTO 1
5	CONTINUE

C
C	Test to see if an incorrect entry was made and disply 
C	error message.
C

        IOERR=MAX(IERR1,IERR2)
	IEND=MAX(IEND1,IEND2)
        IF (IOERR.EQ.0.AND.IEND.EQ.0) THEN
   	  GOTO 1
        END IF
        IF (IEND.NE.0) IOERR=-1
        IF (IOERR.GT.0) THEN
          CALL WRERR('WHAT')

C
C	Go back and ask for another entry.
C

          GOTO 2
        END IF
        IF(IOERR.LT.0) THEN
          GOTO 1
        ENDIF
	GOTO 2
1	CONTINUE

C
C 	Convert RA and DEC to single precision and go back to the
C	main program.
C

        RAS=RA
        DECS=DEC
	END
