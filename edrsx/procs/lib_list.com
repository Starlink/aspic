$!
$! A service routine for PROCLIB. This procedure displays the PURPOSE
$! field of the header of the file specified by P1.
$!---------------------------------------------------------------------
$!
$	IF P1.EQS."" THEN EXIT
$	WRITE SYS$OUTPUT "*-------------------------------------------------------------------"
$	WRITE SYS$OUTPUT "* Module ",MODULE," :"
$	WRITE SYS$OUTPUT "* "
$	OPEN/READ FILE 'P1'
$	ON WARNING THEN GOTO LBL3
$LBL1:
$	PURPOSE="NO"
$LBL2:
$	READ/END=LBL3 FILE RECORD
$	IF PURPOSE.EQS."YES" THEN WRITE SYS$OUTPUT RECORD
$	IF F$LENGTH(RECORD).EQ.1 THEN GOTO LBL1
$	IF F$LOCATE("-",RECORD).EQ.1 THEN GOTO LBL3
$	IF F$LOCATE("PURPOSE",RECORD).EQ.1 THEN PURPOSE="YES"
$	GOTO LBL2
$LBL3:
$	WRITE SYS$OUTPUT " "
$	CLOSE FILE
$	EXIT
