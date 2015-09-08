$!
$! A service routine for PROCLIB. This procedure saves the previous
$! version of the text file specified by P1 into the library and then
$! deletes all current versions from disk.
$!---------------------------------------------------------------------
$!
$	IF P1.EQS."" THEN EXIT
$	ON WARNING THEN EXIT
$	FILE=F$SEARCH("''P1';-1")
$	IF FILE.EQS."" THEN GOTO LBL1
$	VERSION=F$PARSE(FILE,,,"VERSION")
$	LIBRARY 'LIB' 'FILE'
$	WRITE SYS$OUTPUT " File ",P1,VERSION," stored in library ",LIB
$LBL1:
$	DEL 'P1';*
$	EXIT
