$ !+
$ !      Procedure FILEDOC
$ !
$ !      This allows the user to create a permanent copy of
$ !      ASPIC documentation in his own directory.
$ !
$ !      Type      FILEDOC
$ !      and a classified list of all current ASPIC programs
$ !      is created in your directory.
$ !
$ !       Type      FILEDOC *...
$ !       and a complete list of ALL the ASPIC documentation
$ !       will be stored in your directory (more than 1400 blocks!)
$ !
$ !      Type      FILEDOC par1 [par2 [par3 [par4]]]
$ !      and the information requested will be written to a
$ !      file with a suitable name. Note that the full list
$ !      of parameters must be entered - it has no memory.
$ !
$ !      Type       FILEDOC * program
$ !      if you cannot remember which class a particular program
$ !      is to be stored.
$ !
$ !      Written by K F Hartley at RGO on 9 September 1982
$ !
$
$ !
$ !   Note the two options depending on whether there
$ !   were any parameters.
$ !
$ IF P1.NES."" THEN GOTO EXTR
$    HELP/OUTPUT=INFO.LIS/LIBRARY=ASPDIR:PROGLIB *
$    write sys$output "Information written to INFO.LIS"
$    GOTO END
$ EXTR:
$   A:='p1'
$   B:='F$EXTRACT(0,1,A)'
$   IF B.EQS."*" THEN A:="FULL"
$   IF p2.nes."" then A:='p2'
$      HELP/OUTPUT='A'.DOC/LIBRARY=ASPDIR:PROGLIB 'P1' 'P2' 'P3' 'P4'
$      write sys$output "Information written to "'A'".DOC"
$ END:
$      EXIT
