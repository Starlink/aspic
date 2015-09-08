$!
$! A service routine for PROCLIB. This procedure leaves the text file
$! specified by P1 on disk.
$!---------------------------------------------------------------------
$!
$	IF P1.EQS."" THEN EXIT
$	WRITE SYS$OUTPUT " File ",P1," extracted"
$	EXIT
