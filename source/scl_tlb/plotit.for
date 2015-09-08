!+	PROCEDURE PLOTIT
!
!	DSCL PROCEDURE BASED ON RGO COMMAND PROCEDURE VERQUICK.
!	IT PLOTS RASTERISED VERSATEC FILES ('DIT.DAT' FILES) PRODUCED
!	BY ASPIC PROGRAM VERGREY.
!
!	PARAMETERS ARE OPTIONAL, BUT TWO ARE ACCEPTED (IN EITHER ORDER) :-
!	1	ANYTHING BEGINNING "DE"	- DELETE FILE AFTER PLOTTING
!	2	ANYTHING BEGINNING 0-9	- ASSUMED TO BE VERSION NO OF FILE
!
!	THE DEFAULT ACTION IS TO PLOT THE HIGHEST VERSION IN THE CURRENT
!	DIRECTORY AND NOT TO ERASE IT AFTERWARDS.
!
!	NOTE THAT THIS IS A SIMPLE-MINDED PROCEDURE AND THE USER MUST WAIT
!	WHILE IT EXECUTES. IF A MESSAGE "ASSIGN" IS DISPLAYED, IT INDICATES
!	THAT SOMEBODY ELSE IS USING THE VERSATEC (TRY AGAIN LATER). A MESSAGE
!	"CHANNEL <16N>" IS A GOOD SIGN!
!
!	WFL RGO SEP 1981
!
	VERSION=0
	DEL:=FALSE
	NPARAM=0
NEXT:	NPARAM=NPARAM+1
	PARAM:="'"P'NPARAM'"'"
	PARAM:='PARAM'
	IF PARAM.EQS."" THEN GOTO END
	FIRST_CHAR:='F$EXTRACT(0,1,PARAM)'
	FIRST_TWO:='F$EXTRACT(0,2,PARAM)'
	IF FIRST_CHAR.GES."0".AND.FIRST_CHAR.LES."9" THEN VERSION=PARAM
	IF FIRST_TWO.EQS."DE" THEN DEL:=TRUE
	GOTO NEXT
END:	ASSIGN/USER_MODE LVA0: FOR001
	ASSIGN/USER_MODE DIT.DAT;'VERSION' FOR008
	RUN DSCL_EXEDIR:PLOTITIM
	IF DEL THEN DELETE DIT.DAT;'VERSION'
	EXIT
