C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     ********************
C                     *                  *
C                     * Program LUTSTORE *
C                     *                  *
C                     ********************
C
C
C
C          CALLING SEQUENCE:-
C                  LUTSTORE [...]
C
C
C          FUNCTION:-
C               The program will read the current video look up table  from
C               the ARGS and store it in a BDF file.
C
C
C          USE:-
C               It can be used after a  look  up  table  has  been  created
C               and/or modified by other ASPIC programs, such as LUTROT, to
C               store the current look up table for future use.  The stored
C		look up table can  be reloaded into the ARGS with the ASPIC
C		program LUTREAD.
C
C
C
C         USER PARAMETERS:-
C
C         LUT                     Name of file in which the LUT
C                                 will be stored.
C
C
C
C
C         D.J.King                 RGO                            20-Jan-82
C
C
C--------------------------------------------------------------------------


	INTEGER DIM(2)
      INCLUDE 'INTERIM(FMTPAR)'

*
*  Allocate ARGS
*
	CALL SRINIT(0,.FALSE.,JSTAT)
	IF (JSTAT.NE.0) THEN
		CALL WRERR('NOARGS')
	ELSE
*
* Set up output BDF file
*
		DIM(1)=3
		DIM(2)=256
		CALL WRIMAG('LUT',FMT_SL,DIM,2,IPNT,JSTAT)
		CALL ASP_GETLUT(%VAL(IPNT))
		CALL FRDATA('LUT',JSTAT)
		CALL CNPAR('LUT',JSTAT)
	ENDIF

	END

