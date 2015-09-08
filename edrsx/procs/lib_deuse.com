$!
$! A service routine for PROCLIB. This procedure removes "USED BY"
$! header fields from the file specified by P1 and then stores it back 
$! in the original library
$!---------------------------------------------------------------------
$!
$	IF P1.EQS."" THEN EXIT
$	ON WARNING THEN EXIT
$	ZAPUSED/NOEC/NOOPT 'P1' 'P1'
$	LIBRARY 'LIB' 'P1'
$	WRITE SYS$OUTPUT " Module ",MODULE," de-used and stored in library ",LIB
$	DEL 'P1';*
$	EXIT
