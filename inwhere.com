$ !
$ !      Command procedure INWHERE
$ !
$ !      This creates a new entry for the help library accessed
$ !      by the ASPIOC command WHERE.
$ !
$ !      It requires two parameters:-
$ !         the name of the entry
$ !         the class in which it is to be put
$ !
$ !      eg INWHERE FRED LOCAL
$ !         INWHERE ALBERT ARGS
$ !
$ !      Note that because a library is being updated
$ !      each entry will overwrite a previous entry with the same name
$ !      so this procedure may be used both within PULLOVER
$ !      (when the class will be LOCAL) and within COPYDOC, when it
$ !      will be the final destination.
$ !
$ !      Written by K F Hartley at RGO on 21-3-83
$ !
$ IF P1.EQS."" THEN INQUIRE P1 "Which entry?"
$ IF P2.EQS."" THEN INQUIRE P2 "Which class?"
$ CREATE 'P1'.HLP
$ OPEN/WRITE FILE 'P1'.HLP
$ REC="1 "'P1'
$ WRITE FILE REC
$ REC="	HELP    "'P2'"   "'P1'
$ WRITE FILE REC
$ CLOSE FILE
$ PURGE 'P1'.HLP
$ LIBRARY/HELP ASPDIR:WHLIB 'P1'.HLP
$ EXIT
