$!
$!  Generates a low res image, and uses it as the starting image for
$!  an historic noise loop. Upto 6 historic runs are performed, and
$!  then a final classic (non-automatic) run is made using the results 
$!  of the noise loop.
$!
$!  P1 is the name of a con file which must contain explicit values
$!  for the following parameters:
$!  BOXSIZE, RA_HRS (etc), DEC_DEG (etc), CRDDF1 - n
$!
$!  The following files are generated:
$!     'p1'_LO     Lo-res image
$!     'p1'_CO	   Coverage image
$!     'p1'_HIi    ith historic image (i=1 upto 6)
$!     'p1'_NON    Non-automatic classic image
$!     'p1'_AN	   Analysis file
$!     'p1'_VAR	   File holding variances during noise loop
$!-----------------------------------------------------------------
$!
$  IF P1.EQS."" THEN GOTO NO_CON
$  IF F$SEARCH("''P1'.CON").EQS."" THEN GOTO NO_CON
$  IF F$LENGTH(P1) .GT. 8 THEN GOTO LONG_NAME
$!
$  DEF 'P1' EDRSX:MEMCRDD
$!
$!    Ensure a low resolution image is available
$!
$  IF F$SEARCH("''P1'_LO.BDF").EQS."" 
$     THEN
$        RUNSTAR 'P1'/FUNCTION=LORES/LORES='P1'_LO/COVER='P1'_CO
$     ELSE
$        WRITE SYS$OUTPUT " "
$        WRITE SYS$OUTPUT ">>> ",P1,"_LO.BDF already exists, and will be used.
$        WRITE SYS$OUTPUT " "
$     ENDIF
$!
$!    Do a historic noise loop, to generate an image to define the noise
$!
$  IF F$SEARCH("''P1'_HI1.BDF").EQS."" 
$     THEN
$        RUNSTAR 'P1'/NITER=12/STOPFILE='P1'.STOP/SKYIN='P1'_LO/HIRES='P1'_HI1/VARIN=/VAROUT='P1'_VAR/ANALOUT='P1'_AN/STOPFILE='P1'.STOP/METHOD=HIST
$     ELSE
$        WRITE SYS$OUTPUT " "
$        WRITE SYS$OUTPUT ">>> ",P1,"_HI1.BDF already exists, and will be used.
$        WRITE SYS$OUTPUT " "
$     ENDIF
$  COUNT=1
$LBL1:
$  IF F$SEARCH("''P1'.STOP").NES."" THEN DELETE 'P1'.STOP;*
$  WRITE SYS$OUTPUT "NOISE_OK: ",F$TRNLNM("''P1'_NOISE_OK")
$  IF F$TRNLNM("''P1'_NOISE_OK").EQS."YES" THEN GOTO LBL2
$  IN="''P1'_HI''COUNT'"
$  COUNT=COUNT+1
$  OUT="''P1'_HI''COUNT'"
$  IF F$SEARCH("''OUT'.BDF").EQS."" 
$     THEN
$        RUNSTAR 'P1'/NITER=12/STOPFILE='P1'.STOP/SKYIN='IN'/HIRES='OUT'/VARIN='P1'_VAR/VAROUT='P1'_VAR/ANALOUT='P1'_AN/STOPFILE='P1'.STOP/METHOD=HIST
$     ELSE
$        WRITE SYS$OUTPUT " "
$        WRITE SYS$OUTPUT ">>> ",OUT,".BDF already exists, and will be used.
$        WRITE SYS$OUTPUT " "
$        DEF/NOLOG 'P1'_NOISE_OK NO
$     ENDIF
$  IF COUNT.LT.6 THEN GOTO LBL1
$lbl2:
$!
$!
$!    Produce the final result, a classic high resolution image.
$!
$  IF F$SEARCH("''P1'.STOP").NES."" THEN DELETE 'P1'.STOP;*
$  RUNSTAR 'P1'/METHOD=NON/NITER=100/SKYIN='OUT'/HIRES='P1'_NON/VARIN='P1'_VAR/ANALOUT='P1'_AN/STOPFILE='P1'.STOP
$  EXIT
$!
$NO_CON:
$  WRITE SYS$OUTPUT "*** Connection file not found: ",p1,".CON"
$  EXIT
$!
$LONG_NAME:
$  WRITE SYS$OUTPUT "*** Job name (",P1,") should have 8 or less characters"
$  EXIT
