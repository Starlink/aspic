      PROGRAM CALHELP

C+
C    PROGRAM CALHELP
C	short help program for CALIBRATION suite of programs
C
C  D. Tudhope/ROE/Nov 1982
C
C-

      CHARACTER*1 C

      CALL WRUSER('               INTENSITY CALIBRATION HELP',I)
      CALL WRUSER(' ',I)
      CALL WRUSER('Calibration suite of programs to do intensity',I)
      CALL WRUSER('conversion via a look up table.',I)
      CALL WRUSER(' ',I)
      CALL WRUSER('Uses ARGS to display stepwedge in GETLEV',I)
      CALL WRUSER('Images of observed values are converted via a',I)
      CALL WRUSER('LOOKUP table to approximate intensities.',I)
      CALL WRUSER('There are 3 ordered stages to the conversion :',I)
      CALL WRUSER(' ',I)
      CALL WRUSER('1. Produce a LEVELS array, containing a few known',I)
      CALL WRUSER('   intensity levels, either by typing in values',I)
      CALL WRUSER('   or by using cursor on a STEPWEDGE image',I)
      CALL WRUSER('   of a series of photographic calibration spots.',I)
      CALL WRUSER(' ',I)
      CALL WRUSER('2. From this LEVELS array, produce a LOOKUP',I)
      CALL WRUSER('   table by using one of the FIT programs.',I)
      CALL WRUSER(' ',I)
      CALL WRUSER('3. Perform the calibration using the LOOKUP',I)
      CALL WRUSER('   table.',I)
      CALL WRUSER(' ',I)
C*  a screen full
      CALL READC('REPLY','hit RETURN to continue',' ',' ','~',C,I)
C*  new screen full
      CALL WRUSER('	commands',I)
      CALL WRUSER('CALHELP -- THIS HELP INFO',I)
      CALL WRUSER('1.	get known levels',I)
      CALL WRUSER('SETLEV  -- TYPE IN NEW LEVELS FROM KEYBOARD',I)
      CALL WRUSER('EDITLEV -- EDIT EXISTING LEVELS',I)
      CALL WRUSER('GETLEV  -- GET LEVELS BY CURSOR ON STEPWEDGE',I)
      CALL WRUSER('LISTLEV -- LIST LEVELS ON LINEPRINTER',I)
      CALL WRUSER('2.	get lookup table by choice of fits',I)
      CALL WRUSER('COSFIT  -- COSMOS-STYLE BAKER DENSITY FIT',I)
      CALL WRUSER('POLFIT  -- POLYNOMIAL FIT',I)
      CALL WRUSER('SPLFIT  -- SPLINE FIT',I)
      CALL WRUSER('SLAFIT  -- SLALOM FIT',I)
      CALL WRUSER('3.	perform calibration using lookup table',I)
      CALL WRUSER('CALIB   -- INPUT IMAGE --> OUTPUT IMAGE',I)
      CALL WRUSER(' ',I)
      CALL WRUSER('For more info, see ROE LUN 16.1',I)
      END
