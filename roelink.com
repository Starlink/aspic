$!=============================================================================
$! 	Link command file for linking software using the distributed
$!	ASPIC libraries.
$!=============================================================================
$!
$!  Set up error control
$!
$  SET NOON
$  SET MESSAGE/NOFACILITY/NOSEVERITY/NOIDENT/NOTEXT
$  ON CONTROL_Y THEN GOTO CLEAN_UP
$!
$!  Set up logical names to the library directories
$!
$  DEFINE LIBS ASPLIB:[ASPIC.LIB]
$!
$!  Check the module to be linked was specified
$!
$  IF P1 .EQS. "" THEN INQUIRE P1 "Main program object file ?"
$!
$!  Check if all libraries are to be used in the link
$!
$  IF "''P2'" .EQS. "" THEN GOTO LINK_ALL
$!
$!  Set up the link list for this program
$!
$  LIB = ""
$  P2LENGTH = F$LENGTH(P2)
$  IF F$LOCATE("ROEASP",P2) .NE. P2LENGTH THEN LIB = "''LIB',LIBS:ROEASP/LIB"
$  IF F$LOCATE("ASPFACE",P2) .NE. P2LENGTH THEN LIB = "''LIB',LIBS:ASPFACE/LIB"
$  IF F$LOCATE("FINGS",P2) .NE. P2LENGTH THEN LIB = "''LIB',LIBDIR:FINGS/LIB"
$  IF F$LOCATE("ROEARGS",P2) .NE. P2LENGTH THEN LIB = "''LIB',LIBS:ROEARGS/LIB"
$  IF F$LOCATE("ASPIC",P2) .NE. P2LENGTH THEN LIB = "''LIB',LIBS:ASPIC/LIB"
$  IF F$LOCATE("NAG",P2) .NE. P2LENGTH THEN LIB = "''LIB',NAG_LIB/LIB"
$  IF F$LOCATE("INTERIM",P2) .NE. P2LENGTH THEN LIB = "''LIB',INTERIM/OPT"
$  IF F$LOCATE("SGS",P2) .NE. P2LENGTH THEN LIB = "''LIB',LIBDIR:SGS/LIB"
$  IF F$LOCATE("HIGR",P2) .NE. P2LENGTH THEN -
   LIB = "''LIB',LIBDIR:HIGR/LIB/INC=(HIGR_DRDATA,HIGR_CNDATA,HIGR_H3DATA)"
$  IF F$LOCATE("GKS",P2) .NE. P2LENGTH THEN LIB = "''LIB',GKSOPT/OPT"
$  IF F$LOCATE("ARGS",P2) .NE. P2LENGTH .AND. -
      F$LOCATE("GKS",P2) .EQ. P2LENGTH THEN LIB = "''LIB',ARGSOPT/OPT"
$NO_ARGS:
$  GOTO PERFORM_LINK
$!
$!  Set up a link lsit including all libraries
$!
$LINK_ALL:
$!
$  LIB = ""
$  LIB = "''LIB',LIBS:ROEASP/LIB"
$  LIB = "''LIB',LIBS:ASPFACE/LIB"
$  LIB = "''LIB',LIBDIR:FINGS/LIB"
$  LIB = "''LIB',LIBS:ROEARGS/LIB"
$  LIB = "''LIB',LIBS:ASPIC/LIB"
$  LIB = "''LIB',NAG_LIB/LIB"
$  LIB = "''LIB',INTERIM/OPT"
$  LIB = "''LIB',LIBDIR:SGS/LIB"
$  LIB = "''LIB',LIBDIR:HIGR/LIB/INC=(HIGR_DRDATA,HIGR_CNDATA,HIGR_H3DATA)"
$  LIB = "''LIB',GKSOPT/OPT"
$  P2LENGTH=0
$!
$!  Link the program
$!
$PERFORM_LINK:
$  SET ON
$  SET MESSAGE/FACILITY/SEVERITY/IDENT/TEXT
$  IF F$LOCATE("FINGS",P2) .EQ. P2LENGTH .AND. -
        F$LOCATE("ASPIC",P2) .EQ. P2LENGTH THEN GOTO NO_CARGS1
$  LINK/NOMAP  'P1''LIB',SYS$INPUT:/OPT
PSECT=CARGS1,NOSHR
$  GOTO CLEAN_UP
$NO_CARGS1:
$  LINK/NOMAP  'P1''LIB'
$CLEAN_UP:
$  DEASSIGN LIBS
$  SET ON
$  SET MESSAGE/FACILITY/SEVERITY/IDENT/TEXT
$  EXIT
