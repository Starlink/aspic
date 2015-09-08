!+
!
!	Procedure PROGDOC
!
!		This is a version of the ASPIC program PROGDOC
!		which is used in conjunction with PULLOVER to
!		put documentation of ASPIC programs etc. into
!		the correct place.
!
!		In this case it puts a copy of the documentation
!		of a routine which is already in ASPDIRL into
!		the new HELP library.
!
!		Because of the changes in the structure of the
!		HELP library this is a bit more complicated than
!		it was before, so this procedure basically runs
!		three programs:-
!			MYPD	which extracts the C+ documentation
!				from the source file, and writes it
!				into a .DOC file.
!			DEPD	which removes an entry from the .HLP
!				file.
!			INPD	which puts the new documentation into
!				the .HLP file in the correct
!				alphabetical position.
!		When all this is done then the whole module (.HLP) is
!		inserted into the actual library (TEST.HLB).
!
!	It is a DSCL procedure and can be run in two forms:-
!		PROGDOC [progname[.typ]]
!	or
!		PROGDOC progname[.typ] D
!
!	If no filetype is specified a type .FOR is assumed.
!	The second form, when the name of the program is compulsory
!	deletes an entry from the HELP library.
!
!	N.B. The source MUST be in ASPDIRL (at the moment).
!
!	Written by K F Hartley at RGO on 29 September 1982
!



$ IF P1.EQS."" THEN INQUIRE P1 "Source file?"
$ FTYP:="    "
$ L:='F$LOCATE(".",P1)
$ IF L.EQ.'F$LENGTH(P1) THEN FTYP:=".FOR"
$ PROG:='F$EXTRACT(0,L,P1)
$ !
$ !   Find out if the local help module is available.
$ !   If not, make it available.
$ !
$   OPEN/ERROR=GETIT TEMP LOCAL.HLP
$   CLOSE TEMP
$   GOTO RETRY
$ GETIT:
$   LIBRARY/HELP/EXTRACT=LOCAL/OUTPUT=LOCAL PROGLIB
$ RETRY:
!
!   The deletion is done even when not needed, but has no effect
!   if no such entry exists.
!
$      DEPD LOCAL 'PROG'
$      IF P2.NES."" THEN GOTO SKIP
$      FILE:=ASPDIRL:'P1''FTYP'
$      MYPD 'FILE'
$      INPD LOCAL 'PROG'
$      DELETE 'PROG'.DOC;*
$ SKIP:
!
!   This is where the library is actually updated.
!
$       LIBRARY/HELP ASPDIR:PROGLIB LOCAL.HLP
$	PURGE LOCAL.HLP
$ EXIT
