$ !
$ !	Strip filename from 'P1' and put into global symbol
$ !	'P2'. ie remove node/disk/diry/type and version no.
$ !
$	FILE := 'P1'
$	END_DIR = 'F$LOCATE("]",FILE) + 1
$	LENGTH = 'F$LENGTH(FILE)
$	IF END_DIR.LE.LENGTH THEN FILE := 'F$EXTRACT(END_DIR,100,FILE)
$	COLON = 'F$LOCATE(":",FILE) + 1
$	LENGTH = 'F$LENGTH(FILE)
$	IF COLON.LE.LENGTH THEN FILE := 'F$EXTRACT(COLON,100,FILE)
$	DOT = 'F$LOCATE(".",FILE)
$	LENGTH = 'F$LENGTH(FILE)
$	IF DOT.LT.LENGTH THEN FILE := 'F$EXTRACT(0,DOT,FILE)
$	'P2' :== 'FILE'
