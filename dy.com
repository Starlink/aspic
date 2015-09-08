$ !
$ !	Set defaults to ASPDIR.'P1'.'P2' (as it were)
$ !
$	IF P2.NES."" THEN P1:='P1'.'P2'
$	@ASPDIR:SUBDIR "''P1'" SUBDIR
$	SET DEFAULTS 'SUBDIR'
