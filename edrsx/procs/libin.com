$!
$!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
$! Takes a source file and stores it  and its derivatives (eg OBJ files)
$! in the correct libraries.
$!-----------------------------------------------------------------------
$!
$! List valid text file types and compile options
$!
$	VALID_TYPES=".FOR.TOP.INC.MAR.HLP"
$	OPTIONS="/DEBUG/PCA/ANALYSE/CHECK"
$!
$! Determine the directory containing the software being developed
$!
$	IF F$TYPE(DEVSYS_SYS).EQS."" THEN DEVSYS
$	IF F$TYPE(DEVSYS_SYS).EQS."" THEN EXIT
$	WRITE SYS$OUTPUT DEVSYS_SYS," development"
$!
$! See if any compilation options are required (DEBUG for instance)
$!
$	GOSUB COMPILE_OPTIONS 
$!
$! Get module name
$!
$	IF P1.EQS."" THEN INQUIRE P1 "Source file?"
$	IF P1.EQS."" THEN EXIT
$	DEFAULT_DIR=F$PARSE("")-".;"
$	F_TYPE=F$PARSE(P1,"._$_",,"TYPE")
$	F_NAME=F$PARSE(P1,"._$_")-";"-F_TYPE-DEFAULT_DIR
$	FILE=F_NAME+F_TYPE
$!
$! See if a module with specified name and a valid file type exists
$!
$	IF F$SEARCH(FILE).NES."" THEN GOTO LBL4
$	N=1
$LBL2:
$	F_TYPE="."+F$ELEMENT(N,".",VALID_TYPES)
$	IF F_TYPE.EQS.".." THEN GOTO LBL3
$	FILE=F_NAME+F_TYPE
$	IF F$SEARCH(FILE).NES."" THEN GOTO LBL4
$	N=N+1
$	GOTO LBL2
$!
$! If no suitable text file was found then exit
$!
$LBL3:
$	WRITE SYS$OUTPUT " *** NO SUITABLE TEXT FILE FOUND CALLED ",F_NAME
$	EXIT
$LBL4:
$!
$! Find which libraries modules are to be stored in.
$! If user specified a library, make sure it has a valid file type
$!
$	IF P2.EQS."" THEN GOTO LBL5
$	   SLIB=P2
$	   LIB_TYPE=F$PARSE(SLIB,"._$_",,"TYPE")
$	   LIB_NAME=F$PARSE(SLIB,"._$_")-";"-LIB_TYPE
$	   OLIB=LIB_NAME+".OLB"
$	   IF LIB_TYPE.NES."._$_" THEN GOTO LBL9
$	      LIB_TYPE=".TLB"
$	      IF F_TYPE.EQS.".HLP" THEN LIB_TYPE=".HLB"
$	      SLIB=LIB_NAME+LIB_TYPE
$	      OLIB=LIB_NAME+".OLB"
$	      GOTO LBL9
$LBL5:
$!
$! If a standard library exists for the current file type, use it.
$!
$	SLIB=""
$	OLIB=""
$	IF F_TYPE.EQS.".HLP" THEN SLIB=DEVSYS_TOP+DEVSYS_SYS+"HELP.HLB"
$	IF F_TYPE.EQS.".TOP" THEN SLIB=DEVSYS_SOURCE+"TOPSOURCE.TLB"
$	IF SLIB.NES."" THEN GOTO LBL9
$!
$! If user gave no name and no standard library name exists, get 
$! library name from code header
$!
$	OPEN/READ TEXT 'FILE'
$LBL6:
$	READ/END_OF_FILE=LBL7 TEXT RECORD
$LBL6A:
$	IF RECORD-"""".EQS.RECORD THEN GOTO LBL6B
$  	RECORD=RECORD-""""
$	GOTO LBL6A
$LBL6B:
$	IF F$LENGTH("''RECORD'").EQ.1 THEN GOTO LBL6
$	IF F$LOCATE("-","''RECORD'").EQ.1 THEN GOTO LBL7
$	IF F$LOCATE("SOURCE","''RECORD'").NE.1 THEN GOTO LBL6
$	READ TEXT RECORD
$	TMP=F$EDIT("''RECORD'","COMPRESS,TRIM")
$	TMP2=F$ELEMENT(3," ",TMP)
$	SLIB_TYPE=F$PARSE(TMP2,".TLB",,"TYPE")
$	SLIB_NAME=F$PARSE(TMP2,,,"NAME")
$	SLIB=F$PARSE(TMP2,"SOURCE:.TLB")-";"
$	IF SLIB_NAME.EQS."UTILITIES" THEN SLIB=F$SEARCH("USOURCE:UTILITIES.TLB")
$	OLIB=F$PARSE("OBJECT:"+SLIB_NAME+".OLB")-";"
$	IF SLIB_NAME.EQS."UTILITIES" THEN OLIB=F$SEARCH("UOBJECT:UTILITIES.OLB")
$	IF SLIB_NAME.NES."" THEN GOTO LBL8
$LBL7:
$	! If no SOURCE record was found in the header give message and end
$	WRITE SYS$OUTPUT " *** TEXT FILE HAS NO SOURCE FILED IN HEADER"
$	CLOSE TEXT
$	EXIT
$LBL8:
$	CLOSE TEXT
$LBL9:
$!
$! Produce a compile qualifier
$!
$	QUAL="LIST"
$!
$! If DEBUG is selected, source file version 1 will be left on disk
$!
$	IF DEBUG.EQS."NO".AND.PCA.EQS."NO" THEN GOTO LBL10
$	PU 'FILE'
$	RENAME 'FILE';0 'FILE';1
$	QUAL=QUAL+"/DEBUG"
$	IF F_TYPE.EQS.".FOR" THEN QUAL=QUAL+"/NOOPT"
$!
$! If ANALYSE is selected, set SCA library and qualifier
$!
$LBL10:
$	IF ANALYSE.EQS."NO" THEN GOTO LBL11
$	INQUIRE SCA_LIB "SCA library?"
$	IF SCA_LIB.EQS."" THEN GOTO LBL11
$	SCA SET LIBRARY 'SCA_LIB'
$	QUAL=QUAL+"/ANAL"
$LBL11:
$!
$! If CHECK selected, compile with CHECK=BOUNDS option
$!
$	IF CHECK.EQS."YES" THEN QUAL=QUAL+"/CHECK=BOUNDS"
$!
$! If appropriate, compile the source file with the qualifier determined above
$!
$	IF F_TYPE.EQS.".FOR" THEN DEFINE FORT$LIBRARY 'SLIB'
$	IF F_TYPE.EQS.".FOR" THEN FORTRAN/'QUAL' 'FILE'
$	IF F_TYPE.EQS.".MAR" THEN MACRO/'QUAL' 'FILE'
$!
$! Store files in appropriate libraries
$!
$LBL12:
$	ON WARNING THEN GOTO EXIT
$	NAME=F$PARSE(FILE,,,"NAME")
$	! Store .ANA file if produced
$	IF ANALYSE.EQS."YES" THEN SCA LOAD 'NAME'
$	! Store source
$!	IF F_TYPE.NES.".HLP" THEN ZAPTAB/NOOPT/NOEC 'FILE'
$	LIBRARY 'SLIB' 'FILE'
$	WRITE SYS$OUTPUT " File ",FILE," added to library ",SLIB
$	! Store object file if one exists
$	IF F_TYPE.NES.".FOR".AND.F_TYPE.NES.".MAR" THEN GOTO LBL13
$	LIBRARY 'OLIB' 'NAME'.OBJ
$	WRITE SYS$OUTPUT " File ",NAME,".OBJ added to library ",OLIB
$LBL13:
$!
$! Delete listing files, object files and analysis files
$!
$	IF DEBUG.EQS."NO".AND.PCA.EQS."NO" THEN DELETE 'FILE';*
$	IF F$SEARCH(NAME+".ANA").NES."" THEN DELETE 'NAME'.ANA;*
$	IF F$SEARCH(NAME+".OBJ").NES."" THEN DELETE 'NAME'.OBJ;*
$	IF F$SEARCH(NAME+".LIS").NES."" THEN DELETE 'NAME'.LIS;*
$	EXIT
$!-----------------------------------------------------------------------
$!   LOCAL SUBROUTINE COMPILE_OPTIONS
$!				
$! Sets up symbols for any recognized compile options, either YES if
$! selected, or NO if not selected. Also cancel any parameters used in
$! specifing these options.
$!
$COMPILE_OPTIONS:
$	N=0
$	! Loop through all valid compiler options
$LBL15:
$	N=N+1
$	OPT=F$ELEMENT(N,"/",OPTIONS)
$	IF OPT.EQS."/" THEN RETURN
$	! See if any of the subroutine parameters have the value of the
$	! current option
$	'OPT'="NO"
$	M=0
$LBL16:
$	M=M+1
$	IF M.EQS.9.OR.P'M'.EQS."" THEN GOTO LBL15
$	IF P'M'.NES.OPT THEN GOTO LBL16
$	! If so, flag option and delete parameter from list
$	'OPT'="YES"
$	WRITE SYS$OUTPUT " ",OPT," selected"
$	L=M-1
$LBL17:
$	L=L+1
$	P'L'=P'F$STRING(L+1)'
$	IF L.LT.7.AND.P'L'.NES."" THEN GOTO LBL17
$	GOTO LBL15
