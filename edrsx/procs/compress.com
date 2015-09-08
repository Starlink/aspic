$!
$! Compresses the library specified by p1. If non is specified
$! then all libraries involved with current program are compressed
$! If p1 has no file type then p1 is taken as the name of a program
$! whose libraries are to be compressed.
$!----------------------------------------------------------------
$	ON WARNING THEN EXIT
$!
$! Check that a development system is defined
$!
$	IF F$TYPE(DEVSYS_SYS).EQS."" THEN DEVSYS
$	IF F$TYPE(DEVSYS_SYS).EQS."" THEN EXIT
$	WRITE SYS$OUTPUT DEVSYS_SYS," development"
$!
$! Remember default directory and disk
$!
$	CDIR=F$PARSE(" ",,,"DIRECTORY")
$	CDISK=F$PARSE(" ",,,"DEVICE")
$!
$! If user gave a parameter which included a file type, compress only
$! the specified library
$!
$	PROG=P1
$	LOOP="FALSE"
$	IF PROG-"*".NES.PROG THEN LOOP="TRUE"
$	IF PROG-"%".NES.PROG THEN LOOP="TRUE"
$	IF P1.EQS."" THEN GOTO LBL1
$	   IF F$PARSE(P1,,,"TYPE").EQS."." THEN GOTO LBL2
$	      GOSUB COMPRESS
$	      EXIT
$!
$! If no parameter was given use all libraries associated with current
$! prog
$!
$LBL1:
$	IF F$TYPE(DEVSYS_PROG).EQS."" THEN DEVPROG
$	IF F$TYPE(DEVSYS_PROG).EQS."" THEN EXIT
$	PROG=DEVSYS_PROG
$!
$! If a program was specified, compress its libraries
$!
$LBL2:
$	WRITE SYS$OUTPUT " Compressing all libraries associated with ",PROG
$	WRITE SYS$OUTPUT " "
$	P1="OBJECT:"+PROG+".OLB"
$	GOSUB COMPRESS
$	P1="SOURCE:"+PROG+".TLB"
$	GOSUB COMPRESS
$	P1="TOP:"+DEVSYS_SYS+"HELP.HLB"
$	GOSUB COMPRESS
$	P1="SOURCE:TOPSOURCE.TLB"
$	GOSUB COMPRESS
$	P1="SOURCE:UTILITIES.TLB"
$	IF F$TRNLNM("SOURCE").EQS.F$TRNLNM("USOURCE") THEN GOSUB COMPRESS
$	P1="OBJECT:UTILITIES.OLB"
$	IF F$TRNLNM("OBJECT").EQS.F$TRNLNM("UOBJECT") THEN GOSUB COMPRESS
$	EXIT
$!--------------------------------------------------------------
$! LOCAL SUBROUTINE COMPRESS
$!
$!  Compress the library specified by P1
$!
$COMPRESS:
$	
$	FILE=F$SEARCH("''P1'")-CDIR-CDISK
$	IF FILE.EQS."" THEN RETURN
$	   NAME=FILE-F$PARSE(FILE,,,"VERSION")
$	   LIB/COMPRESS/OUTPUT='NAME' 'FILE'
$	   PU 'NAME'
$	   SET FILE/TRUNCATE 'NAME'
$	   WRITE SYS$OUTPUT " ",NAME," compressed"
$	   LAST_FILE=FILE
$	   IF LOOP.EQS."TRUE" THEN GOTO COMPRESS
$	   RETURN
