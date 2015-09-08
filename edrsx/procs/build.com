$!
$!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
$! Builds a .EXE file for a specified program or list of programs 
$!-----------------------------------------------------------------------
$	ON WARNING THEN EXIT
$!
$! List valid option keywords
$!
$	OPTIONS="/DEBUG/ALL/PCA"
$!
$! Determine the directory containing the software being developed
$!
$	IF F$TYPE(DEVSYS_SYS).EQS."" THEN DEVSYS
$	IF F$TYPE(DEVSYS_SYS).EQS."" THEN EXIT
$	WRITE SYS$OUTPUT DEVSYS_SYS," development"
$!
$! See if any option keywords have been specified
$!
$	GOSUB OPTIONS 
$!
$! If all objects are to be re-built, find all .EXE files in
$! DEVSYS TOP directory and build them
$!
$	IF ALL.EQS."NO" THEN GOTO LBL1
$LBL0:
$	   PROG=F$PARSE(F$SEARCH("TOP:*.EXE",1),,,"NAME")
$	   IF PROG.EQS."" THEN EXIT
$	      GOSUB BUILD
$	      GOTO LBL0
$LBL1:
$!
$! If user didn't specify an object, use current development program
$!
$	IF P1.NES."" THEN GOTO LBL2
$	   IF F$TYPE(DEVSYS_PROG).EQS."" THEN DEVPROG
$	   IF F$TYPE(DEVSYS_PROG).EQS."" THEN EXIT
$	   PROG=DEVSYS_PROG
$	   GOSUB BUILD
$	   EXIT
$LBL2:
$!
$! See if there is a .LIS file corresponding to parameter P1
$!
$	OPEN/READ/ERROR=LBL5 LIST_FILE 'P1'.LIS
$LBL3:
$	   READ/END=LBL4 LIST_FILE FILE
$	   PROG=F$PARSE(FILE,,,"NAME")
$	   GOSUB BUILD
$	   GOTO LBL3
$LBL4:
$	   EXIT
$LBL5:
$!
$! See if there is a program corresponding to parameter P1
$!
$	PROG=F$PARSE(P1,,,"NAME")
$	GOSUB BUILD
$	DEL/SYM/GLO OK
$	EXIT
$!-----------------------------------------------------------------------
$!   LOCAL SUBROUTINE OPTIONS
$!				
$! Sets up symbols for any recognized option keywords, either YES if
$! selected, or NO if not selected. Also cancel any parameters used in
$! specifing these options.
$!
$OPTIONS:
$	N=0
$	OPT_LIST=""
$	! Loop through all valid option keywords
$LBL6:
$	N=N+1
$	OPT=F$ELEMENT(N,"/",OPTIONS)
$	IF OPT.EQS."/" THEN RETURN
$	! See if any of the subroutine parameters have the value of the
$	! current option
$	'OPT'="NO"
$	M=0
$LBL7:
$	M=M+1
$	IF M.EQS.9.OR.P'M'.EQS."" THEN GOTO LBL6
$	IF P'M'.NES.OPT THEN GOTO LBL7
$	! If so, flag option and delete parameter from list
$	'OPT'="YES"
$	OPT_LIST=OPT_LIST+" "+OPT
$	WRITE SYS$OUTPUT " ",OPT," selected"
$	L=M-1
$LBL8:
$	L=L+1
$	P'L'=P'F$STRING(L+1)'
$	IF L.LT.7.AND.P'L'.NES."" THEN GOTO LBL8
$	GOTO LBL6
$!-------------------------------------------------------------------
$!
$! LOCAL SUBROUTINE BUILD
$!
$!  Execute the command procedure which will build an object
$!
$BUILD:
$	IF F$TRNLNM(PROG).NES."" THEN DEASSIGN 'PROG'
$	BLD_FILE=F$SEARCH("BUILD:''PROG'.BLD",2)
$	IF BLD_FILE.EQS."" THEN GOSUB LBL10
$	IF BLD_FILE.EQS."" THEN RETURN
$	WRITE SYS$OUTPUT " Building ",PROG
$	@'BLD_FILE' 'OPT_LIST'
$	IF OK.EQS."NO" THEN GOTO LBL9
$	WRITE SYS$OUTPUT PROG," built"
$	RETURN
$LBL9:
$	WRITE SYS$OUTPUT " *** UNSUCCESSFUL BUILD OF ",PROG
$	RETURN
$LBL10:
$	WRITE SYS$OUTPUT " *** BUILD FILE NOT FOUND: ",DEVSYS_BUILD,PROG,".BLD"
$	WRITE SYS$OUTPUT " A standard linking procedure will be tried"
$	BLD_FILE=F$SEARCH("BUILD:TEMPLATE.BLD",3)
$	IF BLD_FILE.EQS."" THEN WRITE SYS$OUTPUT " *** FILE NOT FOUND: ",DEVSYS_BUILD,"TEMPLATE.BLD"
$	RETURN
