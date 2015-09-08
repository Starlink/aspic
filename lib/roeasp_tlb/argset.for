      SUBROUTINE ARGSET(NTYPE)
C+
C     LOCAL VARIABLES
C
C     SETS ARGS AS TARGET FOR FINGS GRAPHICS.
C     IF NTYPE=1, THE ARGS IS RESET
C
C	GIVEN: 	(ARGUMENT)   NTYPE
C
C	CALLS FINGS: ARGS, DEVSPE, VUPORT, WINDOL, PICCLE
C
C	B.D KELLY/ROE/1981
C-
      CALL ARGS
      CALL ARGS_PICWRT
      CALL DEVSPE(9600)
      CALL VUPORT(0.0,1.0,0.0,1.0)
      CALL WINDOL(0.0,511.0,0.0,511.0)
      CALL CLICTL(1)
      IF(NTYPE.EQ.1) CALL PICCLE
      RETURN
      END
C
C
C
C
C
C********************************************************************
C********************************************************************
