C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     ********************
C                     *                  *
C                     * Program   ACLEAR *
C                     *                  *
C                     ********************
C
C
C
C          CALLING SEQUENCE:-
C               ACLEAR
C
C
C          FUNCTION:-
C               It writes zeros into the ARGS picture memory.
C
C
C          USE:-
C               It may be used to clear the ARGS, leaving the LUT and  zoom
C               factors unchanged.
C
C
C         W F Lupton               RGO                             6-JAN-82
C
C
C--------------------------------------------------------------------------



	PROGRAM ACLEAR
	INTEGER ISTAT
C
	CALL SRINIT(0,.FALSE.,ISTAT)
	IF (ISTAT.EQ.0) THEN
		CALL SRBLOC(0,0,511,511,0)
C 	CALL ARGS_CLRIM(ISTAT)
	ENDIF
	CALL EXIT
	END
