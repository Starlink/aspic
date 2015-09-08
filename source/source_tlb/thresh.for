C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     ********************
C                     *                  *
C                     * Program   THRESH *
C                     *                  *
C                     ********************
C
C
C
C          CALLING SEQUENCE:-
C               THRESH
C
C
C          FUNCTION:-
C               To define a threshold value using the ARGS trackerball  and
C               see the effect on the ARGS monitor.
C
C
C          USE:-
C               It can be used to define a threshold  value  for  an  image
C               which  can then be used by the program AHARDCOPY to produce
C               a hardcopy of the data displayed on the ARGS monitor.  When
C               using  the  program  AHARDCOPY   after  this   program  set
C               THRESH=THRESH_OFFSET  on the command line.
C
C
C
C
C         USER PARAMETERS:-
C
C         OFFSET                              This gives the offset from  0
C                                             of  the  threshold  value. If
C                                             the  offset  is  a   positive
C                                             value  then  data  below  the
C                                             threshold value  will  appear
C                                             black   and  data  above  the
C                                             threshold value  will  appear
C                                             white.   If   the  offset  is
C                                             negative  then  the  converse
C                                             will occur.
C
C
C
C         USE OF TRACKER-BALL BUTTONS:-
C
C         GREEN 1     Inverts the current state of the LUT. The  offset  of
C                     the threshold value is not changed.
C
C         WHITE 2     Not used.
C
C         WHITE 3     Not used.
C
C         RED   4     Exits from the program and sets the parameter OFFSET.
C
C
C
C
C
C
C
C
C
C         DJK                      RGO                            13-APR-83
C
C
C--------------------------------------------------------------------------




	INTEGER STAT,DIM(2),OFFSET
	INTEGER*2 RDATA(2)



*  ALLOCATE ARGS

	CALL SRINIT(0,.FALSE.,JSTAT)
	IF (JSTAT.NE.0) THEN
		CALL WRERR('NOARGS')
	ELSE
		CALL LOAD_THRESH
		CALL RUN_THRESH
		CALL READ_THRESH(RDATA)
		OFFSET=RDATA(1)
		IF (RDATA(2).EQ.1) OFFSET=-OFFSET
		CALL WRKEYI('OFFSET',OFFSET,1,ISTAT)
	ENDIF

	END
