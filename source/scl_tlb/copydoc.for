$ !
$ !   DSCL procedure COPYDOC
$ !
$ !   This procedure copies documentation from one part of the
$ !   ASPIC HELP library (under the heading CLASS)
$ !   into another part under its correct class.
$ !
$ !   It should be used as part of the process of moving software
$ !   from the LOCAL ASPIC directory (as done by PULLOVER/PROGDOC)
$ !   into the standard ASPIC directory for release.
$ !
$ !   Written by K F Hartley at RGO on 15 November 1982
$ !
$ !   First find out if the LOCAL help file is available;
$ !   if not extract it from the HELP library.
$ !
$ OPEN/ERROR=FIND LOCHLP LOCAL.HLP
$ CLOSE LOCHLP
$ GOTO CONTINUE
$ FIND:
$    LIB/EXTRACT=LOCAL/HELP/OUTPUT=LOCAL.HLP PROGLIB
$ CONTINUE:
$ !
$ !   First extract the information from the file LOCAL.HLP
$ !   into a set of .EXT files and create a list of the files
$ !   in LOCAL.LIS
$ !
$    LOCDOC
$    OPEN LIST LOCAL.LIS
$ !
$ !   Now read LOCAL.LIS one line at a time and decide where
$ !   to put that information.
$ !
$ LABEL:
$   READ/END_OF_FILE=END LIST RECORD
$   WRITE SYS$OUTPUT "Which class for "'RECORD'
$   INQUIRE CLASS
$   NAMFILE:='F$EXTRACT(0,8,CLASS)'
$   IF CLASS.EQS."" THEN GOTO LABEL
$ !
$ !   The information is to be written into a user-selected
$ !   file of type .HLP.
$ !   If it does not exist it extracts it from the HELP library.
$ !
$   OPEN/ERROR=GETIT OUTFILE 'NAMFILE'.HLP
$   CLOSE OUTFILE
$   GOTO FOUND
$ GETIT:
$   LIBRARY/EXTRACT='CLASS'/OUTPUT='NAMFILE'/HELP ASPDIR:PROGLIB
$ FOUND:
$ !
$ !   Extract the name of the file (stripping off the .EXT)
$ !   and copy it into a file of type .DOC
$ !
$   L='F$LOCATE(".",RECORD)'
$   NAME="'F$EXTRACT(0,L,RECORD)'"
$   COPY 'RECORD' 'NAME'.DOC
$ !
$ !   Delete any existing entry with the same name.
$ !   (No problem arises if no such entry is found)
$ !
$   DEPD 'NAMFILE' 'NAME'
$ !
$ !   Now insert the new entry in its correct alphabetical position.
$ !
$   INPD 'NAMFILE' 'NAME'
$ !
$ !   Now tidy up for this iteration.
$ !   and return for the next file name.
$ !
$   DELETE 'NAME'.DOC;0
$   PURGE 'NAMFILE'.HLP
$   GOTO LABEL
$ END:
$ !
$ !   Now tidy up the garbage created during this process.
$ !
$    DELETE *.EXT;*
$    PURGE *.HLP
$    DELETE LOCAL.LIS;*
$ !
$ !   and insert the new entries into the help library
$ !
$    LIBRARY/HELP PROGLIB *.HLP
$    EXIT
