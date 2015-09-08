	SUBROUTINE HABSCLB
C+
C	 Subroutine to list commands available for absolute
C	 calibration using multiaperture photometry.
C
C	 Subroutine called; OUTPUT.
C
C	 A C Davenhall./ROE/				18/12/81.
C-
	INTEGER ISTAT
	CALL OUTPUT ('    ',ISTAT)
	CALL OUTPUT (' Photoelectric Calibration - Commands Available.',
     :			ISTAT)
	CALL OUTPUT ('    ',ISTAT)
	CALL OUTPUT (' CENTRE (C)     - Locate centre.',ISTAT)
	CALL OUTPUT (' INFILE (INF)   - Get file of measures.',ISTAT)
	CALL OUTPUT (' DISPLAY (D)    - Display apertures.',ISTAT)
	CALL OUTPUT (' INTEGRATE (I)  - Integrate apertures.',ISTAT)
	CALL OUTPUT (' RESULTS (R)    - Display results.',ISTAT)
	CALL OUTPUT (' HELP (H)       - List commands available.',ISTAT)
	CALL OUTPUT (' EXIT (E)       - Terminate this process.',ISTAT)
	CALL OUTPUT ('    ',ISTAT)
	END
