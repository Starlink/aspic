$!
$!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
$!  Submits a DSCL command to batch to run in the same environment
$!  as exists when the job is submitted. Elements of the environment
$!  preserved are: default directory, global symbols, process logical
$!  names.
$!	Also, data can be given to the batch job as one would at the
$!  terminal. If P2 or P3 has the value "DATA", then the user is
$!  prompted for lines of data until a CONTROL_Z is pressed.
$!
$!PARAMETERS:
$!	p1:  DSCL command
$!	p2:  A string to be used as a qualifier for the DCL submit
$!	     command (without a leading / ). Can also take value "DATA".
$!	p3:  If it has the value "DATA" then user is prompted for lines
$!	     of data until CONTROL-Z is pressed.
$!------------------------------------------------------------------
$!
$!  Interpret parameter values supplied by user
$!
$	NO_COM="FALSE"
$	IF P1.EQS."" THEN NO_COM="TRUE"
$	IF NO_COM.EQS."TRUE" THEN INQUIRE P1 "DSCL command?"
$	IF P1.EQS."" THEN EXIT
$	IF P2.EQS."DATA" THEN P3="DATA"
$	IF P2.EQS."DATA" THEN P2=""
$	IF NO_COM.EQS."TRUE" THEN INQUIRE P2 "SUBMIT qualifier?"
$	IF NO_COM.EQS."TRUE" THEN INQUIRE P3 "Data? (Y or <ret>)"
$	IF P3.EQS."Y" THEN P3="DATA"
$	P1=F$EDIT(P1,"TRIM")
$!
$!  Open the .COM file which will be submitted to batch
$!
$LBL0:
$	INQUIRE JOB_NAME "Batch queue job name?"
$	IF F$TRNLNM(JOB_NAME).EQS."" THEN GOTO LBL1
$	WRITE SYS$OUTPUT " *** That name is already being used for something"
$	GOTO LBL0
$LBL1:
$	ON ERROR THEN GOTO CLOSE_JOB
$	ON CONTROL_Y THEN GOTO CLOSE_JOB
$	OPEN/WRITE JOBFILE JOB_'JOB_NAME'.COM
$!
$!  First batch command will be to go to the current directory
$!
$	WRITE JOBFILE "$SET DEFAULT ",F$ENVIRONMENT("DEFAULT")
$!
$!  Now set the DCL prompt to null, in order that SHO SYM output can
$!  be used directly to define symbols
$!
$	WRITE JOBFILE "$SET PROMPT="""""
$!
$!  Now list all global symbols to the job file (the output form SHO SYM
$!  is in a suitable format to use as a symbol definition command)
$!
$	VERIFY=F$VERIFY(0)
$	DEFINE SYS$OUTPUT JOBFILE
$	SHO SYM/GLO *
$	DEASSIGN SYS$OUTPUT
$	IF VERIFY.EQ.1 THEN SET VERI
$!
$!  Set the DCL prompt back to $
$!
$	WRITE JOBFILE "SET PROMPT=""$"""
$!
$!  List all currently define process logical names in a text file.
$!
$	SHO LOG/PROC/OUTPUT=LOGNAMES.LIS
$!
$!  Loop through all records in the log names file, extracting the log 
$!  name and equivalence string and setting up a DEFINE command in the
$!  job file to define that logical name within the batch job. (NB All
$!  log names  SYS$..., and 'TT' are not appropriate to batch jobs)
$!
$	ON CONTROL_Y THEN GOTO CLOSE_LOG
$	ON ERROR THEN GOTO CLOSE_LOG
$	OPEN/READ LOGNAMES LOGNAMES.LIS
$GET_LOG:
$	READ/END=END_LOG LOGNAMES RECORD
$	EQUIV=F$EDIT(F$ELEMENT(1,"=",RECORD),"TRIM")
$	IF EQUIV.EQS."=" THEN GOTO GET_LOG
$	NAME=F$EDIT(F$ELEMENT(0,"=",RECORD),"TRIM")
$	IF F$LOCATE("SYS$",NAME).EQ.1 THEN GOTO GET_LOG
$	IF F$LOCATE("TT",NAME).EQ.1 THEN GOTO GET_LOG
$	WRITE JOBFILE "$DEFINE ",NAME," ",EQUIV
$	GOTO GET_LOG
$END_LOG:
$	CLOSE LOGNAMES
$	ON ERROR THEN GOTO CLOSE_JOB
$	ON CONTROL_Y THEN GOTO CLOSE_JOB
$	DELETE LOGNAMES.LIS;0
$!
$!  Execute the given command procedure using DSCLCOMM
$!
$	WRITE JOBFILE "$DSCLCOMM ",P1
$!
$!  If data is to be included, get data lines from user
$!
$	IF P3.NES."DATA" THEN GOTO END_DATA
$	WRITE SYS$OUTPUT "  Enter lines of data and terminate with CONTROL-Z:"
$GET_DATA:
$	READ/END=END_DATA/PROMPT="Data: " SYS$COMMAND DATA
$	WRITE JOBFILE DATA
$	GOTO GET_DATA
$END_DATA:
$!
$!  Close the .COM file and purge it
$!
$	CLOSE JOBFILE
$	PURGE JOB_'JOB_NAME'.COM
$!
$!  Submit it to batch and then exit
$!
$	COMMAND="SUBMIT/NOTI/NAME=''JOB_NAME'/LOG=''JOB_NAME'"
$	IF F$LOCATE("/",P2).EQ.0 THEN P2="''F$EXTRACT(1,100,P2)'"
$	IF P2.NES."" THEN 'COMMAND'/'P2' JOB_'JOB_NAME'
$	IF P2.EQS."" THEN 'COMMAND' JOB_'JOB_NAME'
$	EXIT
$!
$! Arrive here if an error or control-Y occurs while files are open
$!
$CLOSE_LOG:
$	CLOSE LOGNAMES
$CLOSE_JOB:
$	CLOSE JOBFILE
$	EXIT
