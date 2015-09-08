$ !
$ !      Procedure PERLINK
$ !
$ !      Links applications making the period finding package
$ !
$ !      N.B. Use the GKS-based version of SIMPLEPLOT and NAG
$ !
$	IF P1.EQS."" THEN INQUIRE P1 "What"
$		@ASPDIR:SUBDIR LIB LIBDIR
$	LINK/notrace 'P1',-
		per:perlib/lib,-
		NAG/LIB,-
		ASPLIB:[ASPIC.LIB]RGOLIB/LIB,-
		INTERIM/OPT,-
		GKSOPT/OPT
