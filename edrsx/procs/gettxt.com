$!
$!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
$! Gets a text file from its library onto the disk
$!-------------------------------------------------------------
$	ON WARNING THEN EXIT
$!
$! Get module name and file type
$!
$	IF P1.EQS."" THEN INQUIRE P1  "Get which text module?"
$	IF P1.EQS."" THEN EXIT
$	F_NAME=F$PARSE(P1,,,"NAME")
$	F_TYPE=F$PARSE(P1,".TMP",,"TYPE")
$	FILE=F_NAME+F_TYPE
$!
$! If user specified a library, make sure it has a valid file type
$!
$	IF P2.EQS."" THEN GOTO LBL2
$	   LIB=P2
$	   LIB_TYPE=F$PARSE(LIB,"._$_",,"TYPE")
$	   LIB_NAME=F$PARSE(LIB,"._$_")-";"-LIB_TYPE
$	   IF LIB_TYPE.NES."._$_" THEN GOTO LBL3
$	      LIB_TYPE=".TLB"
$	      IF F_TYPE.EQS.".HLP" THEN LIB_TYPE=".HLB"
$	      LIB=LIB_NAME+LIB_TYPE
$	      GOTO LBL3
$LBL2:
$!
$! Otherwise set the library to be one of the standard libraries.
$! Check that development system and program are defined.
$!
$	IF F$TYPE(DEVSYS_SYS).EQS."" THEN DEVSYS
$	IF F$TYPE(DEVSYS_SYS).EQS."" THEN EXIT
$	WRITE SYS$OUTPUT DEVSYS_SYS," development"
$	IF F$TYPE(DEVSYS_PROG).EQS."" THEN DEVPROG
$	IF F$TYPE(DEVSYS_PROG).EQS."" THEN EXIT
$	LIB=DEVSYS_SOURCE+DEVSYS_PROG+".TLB"
$	IF F_TYPE.EQS.".HLP" THEN LIB=DEVSYS_TOP+DEVSYS_SYS+"HELP.HLB"
$	IF F_TYPE.EQS.".TOP" THEN LIB=DEVSYS_SOURCE+"TOPSOURCE.TLB"
$LBL3:
$!
$! Check library exists
$!
$	DEF_DIR=F$PARSE(" ")-".;"
$	LIB=LIB-DEF_DIR
$	FULL_LIB=F$SEARCH(LIB)
$	IF FULL_LIB.NES."" THEN GOTO LBL4
$	   WRITE SYS$OUTPUT " *** LIBRARY NOT FOUND: ",LIB
$	   EXIT
$LBL4:
$!
$! Attempt to extract the source
$!
$	ON WARNING THEN GOTO LBL5
$	SET MESSAGE/NOTEXT/NOFAC/NOID/NOSEV
$	LIBRARY/EXTRACT='F_NAME'/OUTPUT='FILE' 'LIB'
$	GOTO LBL7
$!
$! If module was not found try the utilities library
$!
$LBL5:
$	ON WARNING THEN GOTO LBL6
$	LIB=F$SEARCH("USOURCE:UTILITIES.TLB")
$	LIBRARY/EXTRACT='F_NAME'/OUTPUT='FILE' 'LIB'
$	GOTO LBL7
$LBL6:
$!
$! Arrive here if module was not found in any library
$!
$	SET MESSAGE/TEXT/FAC/ID/SEV
$	WRITE SYS$OUTPUT " *** MODULE NOT FOUND: ",P1
$	EXIT
$LBL7:
$!
$! Arrive here if module was found. Rename the source if necessary from
$! code header
$!
$	SET MESSAGE/TEXT/FAC/ID/SEV
$	IF F_TYPE.NES.".TMP" THEN GOTO LBL11
$	   OPEN/READ TEXT 'FILE'
$LBL8:
$	   READ/END_OF_FILE=LBL9 TEXT RECORD
$	   IF F$LENGTH("''RECORD'").EQ.1 THEN GOTO LBL8
$	      IF F$LOCATE("-",RECORD).EQ.1 THEN GOTO LBL9
$	         IF F$LOCATE("SOURCE",RECORD).NE.1 THEN GOTO LBL8
$	            READ TEXT RECORD
$	            TMP=F$EDIT(RECORD,"COMPRESS,TRIM")
$	            FILE=F$ELEMENT(1," ",TMP)
$	            RENAME 'F_NAME'.TMP 'FILE'
$	            GOTO LBL10
$LBL9:
$	   WRITE SYS$OUTPUT " *** TEXT FILE HAS NO SOURCE FIELD IN HEADER"
$	   CLOSE TEXT
$	   EXIT
$LBL10:
$	   CLOSE TEXT
$LBL11:
$	WRITE SYS$OUTPUT " File ",FILE," extracted from library ",LIB
$	EXIT
