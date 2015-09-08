      PROGRAM ENTERTAB

C+
C    ENTERTAB
C
C	general 2-d table program
C	reads a 2-d table of reals from keyboard
C	user specifies number of cols; no. of rows must be less than MAXTABS
C	calls readtabt to actually read table
C
C    Given (.KB)
C	table values
C	NCOLS (I)		number of columns in table
C
C    Returned (program parameter)
C	TABLE (RA)	array of .kb input
C
C    Work (program parameters)
C	WORK (RA)
C
C    D. Tudhope/ROE/April 1983
C-

C*  maximum size of TABLE array
      INTEGER MAXTABS
      PARAMETER (MAXTABS=50)
C*  number of columns in TABLE (must be <MAXKEY in READROW)
      INTEGER NCOLS
      INTEGER NTABS,NPTAB,NAXTAB(2),ISTATUS,NPWK,NAXWK(2)

      ISTATUS=0
      CALL READI('NCOLS','Enter number of columns in table',
     :           2,1,32,NCOLS,ISTATUS)
      NAXWK(1)=MAXTABS
      NAXWK(2)=NCOLS
      CALL OUTPICR('WORK',' ',2,NAXWK,NPWK,ISTATUS)
      IF (ISTATUS.NE.0) THEN
        CALL WRERR('MISTAKE')
      ELSE
        CALL WRUSER('Enter values in table as reals,',ISTATUS)
        CALL WRUSER('separated by commas',ISTATUS)
        CALL WRUSER('terminating with "END".',ISTATUS)

        CALL READTABT(MAXTABS,NCOLS,%VAL(NPWK),NTABS)
        IF (NTABS.GE.MAXTABS) THEN
          CALL WRERR('TOOMANY')
        ELSE
          IF (NTABS.EQ.0) THEN
            CALL WRERR('TOOFEW')
          ELSE
C*  get a table of exact size to output
            NAXTAB(1)=NTABS
            NAXTAB(2)=NCOLS
            CALL OUTPICR('TABLE','ENTER NAME OF TABLE FILE',
     :                    2,NAXTAB,NPTAB,ISTATUS)
            IF (ISTATUS.NE.0) THEN
              CALL WRERR('MISWRITE')
            ELSE
             CALL COPY(MAXTABS,NCOLS,%VAL(NPWK),NTABS,NCOLS,%VAL(NPTAB))
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      END
