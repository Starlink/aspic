$!
$! Copies the latest version of a .COM file from the current directory
$! to a pre-set list of software developement systems
$!----------------------------------------------------------------------
$!
$	SYSTEMS="EDRSX,USER2,WORK"
$!
$	ON WARNING THEN GOTO LBL2
$!
$	IF P1.EQS."" THEN INQUIRE P1 "Upgrade which utility?"
$	IF P1.EQS."" THEN EXIT
$!
$	DEF_DIR=F$PARSE("")-".;"
$	FILE=F$PARSE(P1,".COM")-DEF_DIR
$	FILE=FILE-F$PARSE(FILE,,,"VERSION")
$	IF F$SEARCH(FILE).NES."" THEN GOTO LBL0
$	   WRITE SYS$OUTPUT " *** FILE NOT FOUND : ",FILE
$	   EXIT
$LBL0:
$	IF F$TYPE(BYPASS).NES."" THEN BYPASS
$	N=-1
$LBL1:
$	N=N+1
$	SYS=F$ELEMENT(N,",",SYSTEMS)
$	IF SYS.EQS."," THEN GOTO LBL2
$	PROCS_DIR=F$PARSE(F$TRNLNM("''SYS'"))-".;"-"]"+".PROCS]"
$	IF F$PARSE("''PROCS_DIR'").EQS."" THEN GOTO LBL1
$	COPY 'FILE' 'PROCS_DIR'*.*
$	PU 'PROCS_DIR''FILE'
$	WRITE SYS$OUTPUT " ",FILE," copied to system ",SYS
$	GOTO LBL1
$LBL2:
$	IF F$TYPE(DEFPRIV).NES."" THEN DEFPRIV
$	EXIT
