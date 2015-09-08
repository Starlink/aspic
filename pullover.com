$ !
$ !      DCL command procedure PULLOVER
$ !
$ !	Transfer an ASPIC program or procedure to the local directory.
$ !      An entry is created under the class "LOCAL" in the ASPIC
$ !      HELP library.
$ !
$ !      It allows up to three parameters:-
$ !
$ !      P1 	is the program name (with NO file type)
$ !      P2	if present this is the filetype (no dot).
$ !      P3	if present this should be a D for DELETE entry
$ !
$ !             If no file type is given a default of .FOR is assumed.
$ !
$ !   Written by W F Lupton years ago.
$ !   Modified by K F Hartley at RGO on 15 November 1982
$ !
$	DIRY := 'F$DIRECTORY()'
$	ON WARNING THEN GOTO TIDY
$	ON CONTROL_Y THEN GOTO TIDY
$	IF P1.EQS."" THEN INQUIRE P1 "Filename (no filetype)"
$	IF P2.NES."" THEN P2 := .'P2'
$ !
$ !   Remove any disk name from the parameter.
$ !
$	@ASPDIR:STRIPFILE "''P1'" FILE
$	SUBDIR:==LSTARDISK:[STARLOCAL.PACK.ASPIC]
$ !
$ !   If the third parameter was present then act accordingly
$ !
$      SET UIC [277,1]
$      IF P3.EQS."" THEN GOTO CARRY
$         DELETE 'SUBDIR':'FILE'.*.*
$         GOTO DOC
$ CARRY:
$ !
$ !   Copy the files into the local ASPIC directory and set default
$ !   to that directory
$ !   and tidy up.
$ !
$	COPY 'P1'.* 'SUBDIR'*
$	SET DEFAULTS 'SUBDIR'
$	SET NOON
$	DELETE 'FILE'.OBJ;*
$	SET ON
$	PURGE 'FILE'.*
$ !
$ !   Now back to the main ASPIC directory
$ !   and handle the documentation.
$ !
$ DOC:
$       SET DEFAULTS ASPDIR
$	SET UIC [277,277]
$	DSCL PROGDOC 'FILE''P2' 'P3' 
$ TIDY:
$	SET NOON
$	DELETE DSCL_USERCOM:;*
$	SET ON
$	SET DEFAULTS 'DIRY'
