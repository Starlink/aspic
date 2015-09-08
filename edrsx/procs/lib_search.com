$!
$! A service routine for PROCLIB. This procedure prints the file
$! specified by P1 and then deletes it.
$!---------------------------------------------------------------------
$!
$	ON WARNING THEN EXIT
$	IF P1.EQS."" THEN GOTO LBL3
$	IF F$TYPE(STRING).NES."" THEN GOTO LBL0
$	   INQUIRE TEMP "Search for?"
$	   STRING==TEMP
$LBL0:
$	DEFINE SYS$ERROR TEMP.LIS
$	DEFINE SYS$OUTPUT TEMP.LIS
$	ON WARNING THEN GOTO LBL2
$	SEARCH/NOOUTPUT/MATCH=OR 'P1' 'STRING'
$	SEV=$SEVERITY
$LBL2:
$	DEASSIGN SYS$ERROR
$	DEASSIGN SYS$OUTPUT
$	ON WARNING THEN EXIT
$	DEL TEMP.LIS;0
$	IF SEV.EQS.3 THEN DEL 'P1';0
$	IF SEV.EQS.1 THEN WRITE SYS$OUTPUT P1," contains string ",STRING
$	EXIT
$LBL3:
$	DELETE/SYM/GLO STRING
$	EXIT
