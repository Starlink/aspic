$!
$!++++++++++++++++++++++++++++++++++++++++++++++++++++++++
$! Initializes the program being developed 
$!--------------------------------------------------------
$!
$	ON WARNING THEN GOTO LBL4
$	IF F$TYPE(DEVSYS_SYS).EQS."" THEN DEVSYS
$	IF F$TYPE(DEVSYS_SYS).EQS."" THEN GOTO LBL4
$!
$! Find which program is to be developed
$!
$	IF P1.EQS."" THEN INQUIRE P1 "Develope which program?"
$	IF P1.EQS."" THEN GOTO LBL4
$!
$! Check that both object and text libraries exist for this program
$!
$	SLIB=F$SEARCH(DEVSYS_SOURCE+P1+".TLB")
$	OLIB=F$SEARCH(DEVSYS_OBJECT+P1+".OLB")
$	IF SLIB.NES."".AND.OLIB.NES."" THEN GOTO LBL3
$	IF SLIB.EQS."".AND.OLIB.EQS."" THEN GOTO LBL1
$	WRITE SYS$OUTPUT " *** PROGRAM ",P1," HAS A MISSING LIBRARY!"
$	GOTO LBL4
$LBL1:
$	WRITE SYS$OUTPUT " *** PROGRAM ",P1," HAS NO LIBRARIES!"
$LBL2:
$	INQUIRE CRE_LIB "Create libraries? (Y,N)"
$	IF CRE_LIB.EQS."N" THEN GOTO LBL4
$	IF CRE_LIB.NES."Y" THEN GOTO LBL2
$	LIB/CRE/TEXT 'DEVSYS_SOURCE''P1'.TLB
$	LIB/CRE      'DEVSYS_OBJECT''P1'.OLB
$LBL3:
$	DEVSYS_PROG==P1
$	WRITE SYS$OUTPUT " Set up to develope ",P1
$	EXIT
$LBL4:
$	WRITE SYS$OUTPUT " "
$	WRITE SYS$OUTPUT " *** DEVELOPEMENT PROGRAM UNCHANGED"
$	EXIT
