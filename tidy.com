$ !
$ !	Purge all files matching 'P1' and then rename
$ !	all to version number 1. Default is entire default directory.
$ !
$	RSQUARE = 'F$LOCATE("]",P1)' + 1
$	LENGTH = 'F$LENGTH(P1)'
$	IF RSQUARE.GT.LENGTH THEN RSQUARE = 0
$	NAME := 'F$EXTRACT(RSQUARE,100,P1)'
$	IF NAME.EQS."" THEN NAME := *
$	IF 'F$LOCATE(".",NAME)'.EQ.'F$LENGTH(NAME)' THEN NAME := 'NAME'.*
$	FULLNAME := 'F$EXTRACT(0,RSQUARE,P1)''NAME'
$	PURGE 'FULLNAME'
$	RENAME 'FULLNAME';0 'NAME';1
