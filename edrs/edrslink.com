$!
$! PURPOSE:
$!       Links an EDRS program.
$!
$! USE:
$!       Each EDRS program has a corresponding module in the text 
$! library SOURCE.TLB. To produce a new .EXE file, this module should 
$! be extracted and compiled to produce a corresponding .OBJ file.
$! This procedure should then be run to link the .OBJ file with
$! the required libraries. The logical name EDRS should be defined
$! pointing to the directory containing the object libraries EDRSLIB,
$! EDRS_AGI_INFACE, EDRS_AGS_INTER, EDRS_AGI_MODULE.
$!
$! NOTES:
$!      EDRS currently uses an old pre-release version of the AGI 
$! graphics database stored in the EDRS directory. Linking a graphics
$! program will produce lots and lots of warning messages from the 
$! linker. This should not effect the resulting .EXE file.
$!
$!---------------------------------------------------------------------
$!
$   IF P1.EQS."" THEN INQUIRE P1 "Link which EDRS program?"
$   LOG_NAM=F$TRNLNM(P1)
$   IF LOG_NAM.NES."" THEN DEASSIGN 'P1'
$!
$   INQUIRE GRAPHICS "Can program produce any graphics? (Y/N)"
$   IF GRAPHICS.NE."Y" THEN GOTO NOGRAPHICS
$!
$!
$   WRITE SYS$OUTPUT "Linking ",P1," with graphics included"
$   IF F$TRNLNM("ADAM_LIB").EQS."" THEN GOSUB GO_ADAM
$   LINK/NOMAP/NOTRACE 'P1', -
       EDRS:EDRSLIB/LIB/INC=(WRERR,STLERR,STL_GETPPI,AGCHAX,AGCHIL,WRUSER), -
       ASPLIB:ASPIC/LIB, -
       ASPLIB:ROEARGS/LIB, -
       INTERIM/LIB, -
       NAG_LIB/LIB, -
       ARGSOPT/OPT, -
       LIBDIR:TAPEIO/LIB, -
       EDRS:EDRS_AGI_INFACE/LIB,-
       EDRS:EDRS_AGS_INTER/LIB,-
       EDRS:EDRS_AGI_MODULE/LIB,-
       ADAM_EXE:ADAMSHRLIB7/OPT, -
       NCAR_DIR:AGPWRITX, -
       NCAR_DIR:NCARLIB/LIB, -
       GKS_DIR:GKSLIB/LIB, -
       @SGS_DIR:SGSLINK
$   IF LOG_NAM.NES."" THEN DEFINE 'P1' 'LOG_NAM'
$   EXIT
$!
$!
$NOGRAPHICS:
$   WRITE SYS$OUTPUT "Linking ",P1," without graphics libraries"
$   LINK/NOMAP/NOTRACE 'P1', -
       EDRS:EDRSLIB/LIB/INC=(WRERR,STLERR,STL_GETPPI,WRUSER), -
       ASPLIB:ASPIC/LIB, -
       INTERIM/LIB, -
       NAG_LIB/LIB, -
       LIBDIR:TAPEIO/LIB
$   IF LOG_NAM.NES."" THEN DEFINE 'P1' 'LOG_NAM'
$   EXIT
$!
$!
$GO_ADAM:
$   ADAMSTART
$   ADAMDEV
$   RETURN
