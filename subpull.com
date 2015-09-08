$ !
$ !	Pull a Fortran subroutine into the ASPIC library. the command
$ !	procedure copies the routine, compiles it, updates the object
$ !	library and updates the help library.
$ !
$	DIRY:='F$DIRECTORY()'
$	ON WARNING THEN GOTO EXIT
$	@ASPDIR:DY LIB
$	IF P1.EQS."" THEN INQUIRE P1 "Filename (no filetype)"
$	@ASPDIR:STRIPFILE "''P1'" FILE
$	COPY 'P1'.FOR *
$	FORTRAN 'FILE'
$	ON WARNING THEN GOTO TIDY
$	LIBRARY ASPLIB 'FILE'
$	DSCL PROGDOC SOURCE='FILE' 'P2' 'P3' 'P4' LIBRARY=SUBLIB.HLB
$ TIDY:
$	PURGE 'FILE'.*
$	DELETE 'FILE'.OBJ.0
$ EXIT:
$	SET DEFAULTS 'DIRY'
$	EXIT
