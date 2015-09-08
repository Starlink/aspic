$!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
$! Initialises logical names, symbols and directory for development of
$! software contained in a specified directory (or "system")
$!
$! The system must have the following directory structure:
$!
$! "TOP"     [x.y.FRED]         Contains all files needed by a user
$! "SOURCE"  [x.y.FRED.SOURCE]  Contains text libraries.
$!		                Each .EXE file in TOP directory must
$!				have a .TLB text library in the this
$!				directory, containing source code of
$!				routines used only in the .EXE file.
$! "OBJECT"  [x.y.FRED.OBJECT]  Contains object libraries. Each .EXE
$!				file in TOP directory must have a .OLB
$!				library im this directory, containing
$!				object modules from the corresponding
$!				text library.
$! "BUILD"   [x.y.FRED.BUILD]   Contains DCL command procedures to build
$!				each of the .EXE files in TOP directory.
$!				Each .EXE must have a file with the same
$!				name but file type .BLD containing the
$!				link command, etc.
$! "PROCS"   [x.y.FRED.PROCS]   Contains all procedures used for the
$!				development and re-linking of the system
$!
$! The TOP directory must be pointed to by a logical name. This logical
$! name becomes the name by which the system is known.
$!-------------------------------------------------------------------------
$	ON WARNING THEN GOTO LBL3
$	DEFINE="DEFINE/NOLOG"
$!
$! Determine which software system is to be developed
$!
$	IF P1.EQS."" THEN INQUIRE P1 "Develope which system?"
$	IF P1.EQS."" THEN EXIT
$!
$! Translate the logical name ( given as the system name) to find the
$! TOP directory. Then construct other directory names, checking that
$! the directories exist. 
$!
$	CREATE_DIR="FALSE"
$	IF F$TYPE(DEVSYS).NES."" THEN SOURCE_DIR==F$PARSE(DEVSYS-"@",,,"DEVICE")+F$PARSE(DEVSYS-"@",,,"DIRECTORY")-".PROCS]"
$
$	DEVSYS_TOP==F$TRNLNM(P1)
$	IF DEVSYS_TOP.EQS."" THEN GOTO LBL2
$	DEVSYS_TOP=F$PARSE(DEVSYS_TOP)-".;"
$	DEFINE TOP 'DEVSYS_TOP'
$	DIRN=DEVSYS_TOP
$	GOSUB CHECK_DIRN
$
$	DEVSYS_SOURCE==DEVSYS_TOP-"]"+".SOURCE]"
$	DEFINE SOURCE 'DEVSYS_SOURCE'
$	DIRN=DEVSYS_SOURCE
$	GOSUB CHECK_DIRN
$
$	DEVSYS_OBJECT==DEVSYS_TOP-"]"+".OBJECT]"
$	DEFINE OBJECT 'DEVSYS_OBJECT'
$	DIRN=DEVSYS_OBJECT
$	GOSUB CHECK_DIRN
$
$	DEVSYS_PROCS==DEVSYS_TOP-"]"+".PROCS]"
$	DEFINE PROCS 'DEVSYS_PROCS'
$	DIRN=DEVSYS_PROCS
$	GOSUB CHECK_DIRN
$
$	DEVSYS_BUILD==DEVSYS_TOP-"]"+".BUILD]"
$	DEFINE BUILD 'DEVSYS_BUILD'
$	DIRN=DEVSYS_BUILD
$	GOSUB CHECK_DIRN
$!
$! If no utility procedures exist, use the ones in the source system
$!
$	IF F$SEARCH("PROCS:*.COM").NES."" THEN GOTO LBL0
$	   DEVSYS_PROCS==SOURCE_DIR+".PROCS]"
$	   DEFINE PROCS 'DEVSYS_PROCS'
$LBL0:
$!
$! If no utility libraries exits, use the ones in the source system
$!
$	DEFINE USOURCE 'DEVSYS_SOURCE'
$	IF F$SEARCH("USOURCE:UTILITIES.TLB").EQS."" THEN DEFINE USOURCE 'SOURCE_DIR'.SOURCE]
$	DEFINE UOBJECT 'F$STRING(F$TRNLNM("USOURCE")-".SOURCE]"+".OBJECT]")'
$	UTIL_TLB=F$SEARCH("USOURCE:UTILITIES.TLB")
$	UTIL_OLB=F$SEARCH("UOBJECT:UTILITIES.OLB")
$	DEFINE UTILITIES USOURCE:UTILITIES.TLB
$!
$! If the directories had to be created then create standard libraries.
$!
$	IF CREATE_DIR.EQS."FALSE" THEN GOTO LBL1
$	   LIBRARY/CREATE/HELP 'DEVSYS_TOP''P1'HELP.HLB
$	   LIBRARY/CREATE/TEXT 'DEVSYS_SOURCE'TOPSOURCE.TLB
$	   COPY 'SOURCE_DIR'.SOURCE]TEMPLATE.FOR SOURCE:TEMPLATE.FOR
$	   COPY 'SOURCE_DIR'.SOURCE]TEMPLATE.CON SOURCE:TEMPLATE.CON
$	   COPY 'SOURCE_DIR'.BUILD]TEMPLATE.BLD BUILD:TEMPLATE.BLD
$	   COPY 'SOURCE_DIR'.SOURCE]TEMPLATE.HLP SOURCE:TEMPLATE.HLP
$          IF UTIL_TLB.NES."".AND.UTIL_OLB.NES."" THEN GOTO LBL1
$	      WRITE SYS$OUTPUT "*** EMPTY UTILITY LIBRARIES BEING CREATED"
$	      LIBRARY/CREATE/TEXT 'DEVSYS_SOURCE'UTILITIES.TLB
$	      LIBRARY/CREATE 'DEVSYS_OBJECT'UTILITIES.OLB
$	      DEFINE USOURCE 'DEVSYS_SOURCE'
$	      DEFINE UOBJECT 'DEVSYS_OBJECT'
$LBL1:
$!
$! Define symbols to execute common utility procedures in PROCS directory
$!
$	UTILITY=F$PARSE(F$SEARCH(DEVSYS_PROCS+"*.COM"),,,"NAME")
$	IF UTILITY.NES."" THEN 'UTILITY'=="@"+DEVSYS_PROCS+UTILITY
$	IF UTILITY.NES."" THEN GOTO LBL1
$!
$! Perform the system start up procedure if it exists
$!
$	IF F$SEARCH(DEVSYS_PROCS+"SYSINIT.COM").NES."" THEN SYSINIT
$	WRITE SYS$OUTPUT " Names set up for developement of ",P1
$!
$! Set up the symbol DEVSYS_SYS to be used by other developement 
$! procedures to determine if a valid system is defined.
$!
$	DEVSYS_SYS==P1
$	IF P2.EQS."NOPROG" THEN EXIT
$	ON WARNING THEN EXIT
$!
$! Set up symbol pointing to program being developed
$!
$	IF F$TYPE(DEVPROG).NES."" THEN DEVPROG
$!
$! Set directory to TOP
$!
$	SET DEF TOP
$	IF F$TYPE(BANNER).NES."" THEN BANNER
$	WRITE SYS$OUTPUT " Current directory set to ",DEVSYS_TOP
$	EXIT
$!
$! Error messages
$!
$LBL2:
$	WRITE SYS$OUTPUT " *** NO SUCH SYSTEM: ",P1
$LBL3:
$	WRITE SYS$OUTPUT " *** DEVELOPEMENT SYSTEM UNDEFINED"
$	IF F$TYPE(DEVSYS_SYS).NES."" THEN DEL/SYM/GLO DEVSYS_SYS
$	IF F$TYPE(DEVSYS_TOP).NES."" THEN DEL/SYM/GLO DEVSYS_TOP
$	IF F$TYPE(DEVSYS_SOURCE).NES."" THEN DEL/SYM/GLO DEVSYS_SOURCE
$	IF F$TYPE(DEVSYS_OBJECT).NES."" THEN DEL/SYM/GLO DEVSYS_OBJECT
$	IF F$TYPE(DEVSYS_PROCS).NES."" THEN DEL/SYM/GLO DEVSYS_PROCS
$	IF F$TYPE(DEVSYS_BUILD).NES."" THEN DEL/SYM/GLO DEVSYS_BUILD
$	EXIT
$
$!---------------------------------------------------------------------
$!  LOCAL SUBROUTINE CHECK_DIRN  
$!
$! Checks that a directory exists using F$PARSE. If it doesn't exist, 
$! the symbol CREATE_DIR is checked, if it is TRUE then the directory 
$! is created, if not then the user is asked if the directory should 
$! be created, and is created if required.
$!
$CHECK_DIRN:
$	IF F$PARSE(DIRN).NES."" THEN RETURN
$	IF CREATE_DIR.EQS."TRUE" THEN GOTO LBL5
$	WRITE SYS$OUTPUT " *** NOT ALL DIRECTORIES EXIST FOR SYSTEM ",P1
$LBL4:
$	INQUIRE ANS "Create directories? (Y/N)" 
$	IF ANS.EQS."N" THEN GOTO LBL3
$	IF ANS.NES."Y" THEN GOTO LBL4
$LBL5:
$	CREATE_DIR="TRUE"
$	CRE/DIR 'DIRN'
$	RETURN
