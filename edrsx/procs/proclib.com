$!
$! Processes all files in a text or help library. The process performed
$! is selected by parameter P2 which must be a recognised key-word. For
$! each recgonised key-word there must exist a utility routine called
$! LIB_'keyword' in the development system PROCS directory which does
$! the required processing on any given disk file.
$!
$! PARAMETERS:
$!	   P1  Name of library to be processed (default type is .TLB)
$!	   P2  Process key-word (see above)
$!	   P3  File type for disk files produced (default type is .TXT)
$!	   P4  If "Y" then the processing of each file must be confirmed
$!	       by the user. If "N" files are processed automatically.
$!	   P5  If "Y" then files are deleted after being processed
$!	   P6  A qualifier for the LIBRARY command which lists the files
$!	       in the library. Can be used to select files by date, etc.
$!----------------------------------------------------------------------
$!
$	ON WARNING THEN EXIT
$	KEY_WORDS=" PRINT DETAB DEUSE LIST GET SAVE SEARCH "
$	DEFAULT_DIR=F$PARSE(" ")-".;"
$!
$! Get all parameter values
$!
$	IF P1.EQS."" THEN INQUIRE P1 "Process which library?"
$	IF P1.EQS."" THEN EXIT
$	LIB=F$SEARCH(F$PARSE(P1,".TLB"))-DEFAULT_DIR
$	IF LIB.NES."" THEN GOTO LBL1
$	   WRITE SYS$OUTPUT " *** NO SUCH LIBRARY: ",P1
$	   EXIT
$!
$LBL1:
$	IF P2.EQS."" THEN INQUIRE P2 "Process key word? (Default=PRINT)"
$	IF P2.EQS."" THEN P2="PRINT"
$	TEMP=KEY_WORDS-P2
$	IF TEMP-"  ".NES.TEMP THEN GOTO LBL2
$	   WRITE SYS$OUTPUT " *** NO SUCH KEY WORD: ",P2
$	   WRITE SYS$OUTPUT " Valid key words are",KEY_WORDS
$	   P2=""
$	   GOTO LBL1
$!
$LBL2:
$	IF P3.EQS."" THEN INQUIRE P3 "File type? (Default=.TXT)"
$	IF P3.EQS."" THEN P3=".TXT"
$	F_TYPE=P3-"."
$!
$LBL3:
$	IF P4.EQS."Y".OR.P4.EQS."N" THEN GOTO LBL4
$	   INQUIRE P4 "Confirm files? (Y/N)"
$	   GOTO LBL3
$!
$LBL4:
$	IF P5.EQS."Y".OR.P5.EQS."N" THEN GOTO LBL41
$	   INQUIRE P5 "Delete files after use? (Y/N)"
$	   GOTO LBL4
$!
$LBL41:
$	IF P6.EQS."" THEN INQUIRE P6 "LIBRARY qualifier?"
$	IF F$LOCATE("/",P6).EQS.0 THEN P6=F$EXTRACT(1,255,P6)
$	IF P6.NES."" THEN P6="/"+P6
$!
$! Do any process specific operations
$!
$	IF P2.EQS."PRINT" THEN P5="N"
$!
$! Get a list of all files to be processed
$!
$	LIBRARY'P6'/LIST=TMP.LIS 'LIB'
$!
$! Read through the list file until the file pointer is past the header
$!
$	NDONE=0
$	ON WARNING THEN GOTO LBL9
$	OPEN/READ LIB_LIST TMP.LIS
$LBL5:
$	READ/END=LBL9 LIB_LIST RECORD
$	IF RECORD.NES."" THEN GOTO LBL5
$!
$! Now read in the name of each module to be processed in turn
$!
$LBL6:
$	READ/END=LBL9 LIB_LIST RECORD
$	MODULE=F$ELEMENT(0," ",RECORD)
$	IF P4.EQS."N" THEN GOTO LBL8
$LBL7:
$	   INQUIRE ANS "Process module ''MODULE' ? (Y/N/Q)"
$	   IF ANS.EQS."N" THEN GOTO LBL6
$	   IF ANS.EQS."Q" THEN GOTO LBL9
$	   IF ANS.NES."Y" THEN GOTO LBL7
$LBL8:
$!
$! Extract the module from the library into a file with the given type
$!
$	FILE=F$PARSE(MODULE,,,"NAME")+F$PARSE(MODULE,"."+F_TYPE,,"TYPE")
$	LIBRARY/EXTRACT='MODULE'/OUTPUT='FILE' 'LIB'
$!
$! Process this file and go round for next file
$!
$	LIB_'P2' 'FILE'
$	IF P5.EQS."Y".AND.F$SEARCH(FILE).NES."" THEN DEL 'FILE';0
$	NDONE=NDONE+1
$	GOTO LBL6
$LBL9:
$!
$! All files done. Tidy up.
$!
$	LIB_'P2'
$	CLOSE LIB_LIST
$	DEL TMP.LIS;0
$	WRITE SYS$OUTPUT " ",NDONE," files processed"
$	EXIT
