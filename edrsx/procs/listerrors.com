$! 
$! Lists all Interim ERROR parameters accessed by a program, to
$! SYS$OUTPUT.
$!-----------------------------------------------------------------
$!
$	ON WARNING THEN EXIT
$!
$! Determine the directory containing the software being developed
$!
$	IF F$TYPE(DEVSYS_SYS).EQS."" THEN DEVSYS
$	IF F$TYPE(DEVSYS_SYS).EQS."" THEN EXIT
$	WRITE SYS$OUTPUT DEVSYS_SYS," development"
$!
$! Find which program is to be processed
$!
$	IF P1.EQS."" THEN INQUIRE P1 "List errors for which program?"
$	P1=F$PARSE(P1,,,"NAME")
$	IF P1.EQS."" THEN EXIT
$!
$! Extract TOP routine for this program and see what subroutine it
$! calls
$!
$	LIBRARY/EXTRACT='P1'/OUTPUT='P1'.TOP SOURCE:TOPSOURCE.TLB
$	IF F$TRNLNM("ABCD").NES."" THEN CLOSE ABCD
$	OPEN/READ ABCD 'P1'.TOP
$	READ ABCD REC
$	CLOSE ABCD
$	DELETE 'P1'.TOP;*
$	REC=F$EDIT(REC,"UPCASE")
$	MOD=F$EXTRACT(F$LOCATE("CALL_",REC)+5,100,REC)
$!
$! Initialize the list of routines called in the program tree to null
$!
$	MLIST==""
$!
$! Set the current subroutine depth to 0, and set the library in
$! which the next subroutine is stored to the program library
$!
$	DEPTH=0
$	LIB="SOURCE:''P1'.TLB"
$!
$! Call LISTER to list all the errors in the next subroutine and to
$! call LISTER recursively for each subroutine called by the current
$! subroutine
$!
$	CALL LISTER 'MOD' 'LIB' 'DEPTH' 'P2'
$!
$! List all routines in program tree
$!
$	N=0
$	WRITE SYS$OUTPUT " "
$	WRITE SYS$OUTPUT " Routines called in ",P1," tree:"
$LBL1:
$	MOD=F$ELEMENT(N,",",MLIST)
$	IF MOD.EQS."," THEN GOTO LBL2
$	WRITE SYS$OUTPUT "      ",MOD
$	N=N+1
$	GOTO LBL1
$LBL2:
$	EXIT
$!
$!
$!
$LISTER:
$	SUBROUTINE
$!
$!----------------------------------------------------------------------
$!SUBROUTINE LISTER:
$!			PARAMETERS:  P1 - Module name
$!				     P2 - Library containing module
$!				     P3 - Module depth
$!
$!  Extracts the specified module from its text library and lists all 
$!  occurances of the string "WRERR(" within the module. It then calls
$!  LISTER for each of the modules listed in the SUBROUTINE CALLS header
$!  field (but only for EDRSX or EDRS modules). Thus by calling LISTER
$!  once a list of all ERROR parameters for the entire program call
$!  tree is produced on SYS$OUTPUT.
$!----------------------------------------------------------------------
$!
$	ON WARNING THEN EXIT
$!
$! If this module has already been done, return immediately.
$!
$	IF MLIST-P1.NES.MLIST THEN EXIT
$	WRITE SYS$OUTPUT " "
$	WRITE SYS$OUTPUT " Searching module: ",P1," (depth ",P3," )"
$	IF P1.EQS.P4 THEN SET VERIFY
$!
$! Extract the module from its library and search for "WRERR("
$!
$	LIBRARY/EXTRACT='P1'/OUTPUT='P1'.FOR 'P2'
$	SEARCH 'P1'.FOR "WRERR("
$!
$! Increment module depth and generate logical name for this module
$!
$	DEPTH=P3+1
$	FILE="ABCD"+F$STRING(DEPTH)
$!
$! Open the module and read through to the start of the SUBROUTINES
$! CALLED field (if EDRSX module), or the CALLS field (if EDRS module).
$!
$	HEADER="NO"
$	PACK="NONE"
$	IF F$TRNLNM(FILE).NES."" THEN CLOSE 'FILE'
$	OPEN/READ 'FILE' 'P1'.FOR
$LBL1:
$	READ/END=LBL6 'FILE' REC
$	IF HEADER.EQS."NO".AND.F$EXTRACT(0,1,REC).EQS."*" THEN HEADER="YES"
$	IF F$LENGTH(REC).EQ.1 THEN GOTO LBL1
$	IF F$LOCATE("-",REC).EQ.1 THEN GOTO LBL6
$	IF F$LOCATE("SUBROUTINES CALLED",REC).EQ.1 THEN PACK="EDRSX"
$	IF F$LOCATE("CALLS",REC).EQ.1 THEN PACK="EDRS"
$	IF PACK.EQS."NONE" THEN GOTO LBL1
$!
$! Read the next record which should be a library specification, and see
$! which library it is (only UTILITIES, EDRS and EDRSX program libraries
$! are of interest). If it is not a valid library, loop until one is
$! found.
$!
$LBL2:
$	GOSUB READ_REC
$	IF STATUS.EQS."END" THEN GOTO LBL5
$	IF LIBSP.EQS."NONE".OR.LIBSP.EQS."" THEN GOTO LBL2
$!
$! Find a record containing a list of modules
$!
$LBL3:
$	NEXTLIB="''LIBSP'"
$	GOSUB READ_REC
$	IF STATUS.EQS."END" THEN GOTO LBL5
$	IF LIBSP.EQS."" THEN GOTO LBL2
$	IF LIBSP.NES."NONE" THEN GOTO LBL3
$!
$! Trim the list by removing the comment character and any spaces and
$! tabs
$!
$	REC=F$EDIT(F$EXTRACT(1,100,REC),"COLLAPSE")
$!
$! Loop through each module in list and call LISTER for each
$!
$	R=0
$LBL4:
$	MOD=F$ELEMENT(R,",",REC)
$	IF MOD.EQS."," THEN GOTO LBL2
$	IF MOD.NES."" THEN CALL LISTER 'MOD' 'NEXTLIB' 'DEPTH' 'P4'
$	R=R+1
$	GOTO LBL4
$!
$! Tidy up
$!
$LBL5:
$	CLOSE 'FILE'
$	DEL 'P1'.FOR;0
$	MLIST==MLIST+","+P1
$	EXIT
$!
$! Arrive here if no SUBROUTINES CALLED or CALLS header was found
$!
$LBL6:
$	WRITE SYS$OUTPUT " "
$	WRITE SYS$OUTPUT " *** NO ""SUBROUTINES CALLED"" HEADER IN ",P1
$	WRITE SYS$OUTPUT " "
$	GOTO LBL5
$!
$! Arrive here if a DCL warning occurs
$!
$LBL7:
$	WRITE SYS$OUTPUT " "
$	WRITE SYS$OUTPUT " *** DCL ERROR. CHECK IT !!!!!"
$	WRITE SYS$OUTPUT " "
$	GOTO LBL5
$	
$!
$READ_REC:
$!---------------------------------------------------------------------
$! LOCAL SUBROUTINE READ_REC:
$!
$!  Reads a record from the current file. Checks to see if the end of
$!  the current header has been reached, in which case STATUS is return
$!  containing "END". See if the record contains a library spec,
$!  if it doesn't return LIBSP containing "NONE", if it does return
$!  LIBSP holding the full library file name if the library is an EDRSX
$!  library or the EDRS library, or null if it is any other library.
$!----------------------------------------------------------------------
$!
$	STATUS="OK"
$	READ/END=READ_REC3 'FILE' REC
$	IF F$LENGTH(REC).LE.1 THEN GOTO READ_REC
$	CHAR1=F$EXTRACT(1,1,REC)
$	IF CHAR1.NES." ".AND. CHAR1.NES."	" THEN GOTO READ_REC3
$	IF F$EXTRACT(0,1,REC).NES."*" THEN GOTO READ_REC3
$	IF REC-":".NES.REC THEN GOTO READ_REC1
$	   LIBSP="NONE"
$	   RETURN
$READ_REC1:
$	IF REC-"THIS PACKAGE (".EQS.REC THEN GOTO READ_REC2
$	   OPENBR=F$LOCATE("(",REC)+1
$	   TLIB=F$EXTRACT(OPENBR,F$LOCATE(".",REC)-OPENBR,REC)
$	   LIBSP="SOURCE:''TLIB'.TLB"
$	   IF TLIB.EQS."UTILITIES" THEN LIBSP="USOURCE:UTILITIES.TLB"
$	   RETURN
$READ_REC2:
$	LIBSP=""
$	IF REC-"EDRS:".EQS.REC.AND.REC-"THIS PACKAGE:".EQS.REC THEN RETURN
$	LIBSP="STARDISK:[STARLINK.PACK.ASPIC.EDRS]EDRSLIB.TLB"
$	RETURN
$READ_REC3:
$	STATUS="END"
$	RETURN
