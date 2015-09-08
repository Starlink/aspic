$!
$! A service routine for PROCLIB. This procedure prints the file
$! specified by P1 and then deletes it.
$!---------------------------------------------------------------------
$!
$	IF P1.EQS."" THEN EXIT
$	PRINT/HEADER/DELETE 'P1'
$	WRITE SYS$OUTPUT " File ",P1," submitted to printer"
$	EXIT
