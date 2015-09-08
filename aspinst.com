$ !
$ !	Install ASPIC/DSCL programs:-
$ !		DSCLCOMM	shared and header resident
$ !
$	RUN SYS$SYSTEM:INSTALL
DSCLDIR:DSCLCOMM/SHARED/HEADER
