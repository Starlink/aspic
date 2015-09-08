$ !
$ !      N.B.   Whenever DSCL is executed a null value
$ !             is given to HELP_PAR - otherwise an error
$ !             occurs on the GOTO SECOND line.
$ !             because the first time HELP is executed
$ !             HELP_PAR has no value.
$ !+
$ !      An alternative HELP procedure for ASPIC
$ !
$ !      To run it simply type HELP
$ !      It remembers the first parameter you entered
$ !      until it is cleared by typing HELP ^
$ !
$ !      Written by K F Hartley at RGO on 7 Sept 1982
$ !
$ 
$
$
$ !
$ !
$ !   First set up a "foreign" DCL symbol (to handle parameter strings)
$ !
$ TEMPHELP:==$STARDISK:[STARLINK.PACK.ASPIC.DSCL]ASPHELP.EXE
$ !
$ !   Then test for a "clear" command.
$ !
$ IF P1.EQS."^" THEN GOTO CLEAR
$ !
$ !   If the parameter has been set to an * set it back to null.
$ !
$ IF HELP_PAR.EQS."*" THEN HELP_PAR:==""
$ !
$ !   Test for an existing value for HELP_PAR
$ !
$ IF HELP_PAR.NES."" THEN GOTO SECOND
$ !
$ !   If no value exists then execute help with the given parameters.
$ !   and store the first parameter.
$ !
$      TEMPHELP 'P1' 'P2' 'P3' 'P4'
$      HELP_PAR:=='P1'
$       EXIT
$ SECOND:
$ !
$ !   Otherwise use the remembered parameter and tack on the others.
$ !
        TEMPHELP 'HELP_PAR' 'P1' 'P2' 'P3' 'P4'
$      EXIT
$ CLEAR:
$ !
$ !   Ignore all parameters and clear the memory.
$ !
$      TEMPHELP
$      HELP_PAR:==""
$      EXIT
$ !
$ !   NOTE (1)  It uses a version of the ASPIC program HELP
$ !             so that it searches a new library which has
$ !             been structured for this application.
$ !
$ !        (2)  Somewhere an initial value for HELP_PAR must be
$ !             set up - hence the instruction to use AHELP ^
