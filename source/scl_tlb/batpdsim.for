!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!       !!!!!!!!!!!!!!!!
!       !              !
!       ! BATPDSIM.SCL !
!       !              !
!       !!!!!!!!!!!!!!!!
!
!
!
!  This executes PDSCOR as a Batch job.
!
!
!  It takes the .BDF files from a MULTI scan on the PDS and
!  turns them into linear images.
!
!  This takes the Reference scans and interpolates to get the
!  'clear' level under each line of the Main scan.
!  The Main scan and Clear levels are then made into linear density
!  scans by applying the PDS correction table.
!  The Clear scan is then subtracted from the Main scan to give
!  a density scan with zero at zero density.
!  Optionally, this scan can be corrected for a saturation
!  (eg electronographic) law to give a linear intensity scan.
!  The scan is then multiplied by a constant to give approx
!  the same values as the origional PDS scan.
!
!  For a description see PDSCOR.
!
!
!
! USES PROGRAMS :-
!      PDSCOR
!
!
! WRITTEN BY:-
!     A.J.PENNY                                            83-6-4
!--------------------------------------------------------------------
!
!
!
!
INQUIRE P1 "PDS MAIN IMAGE HAS NAME (INCLUDE DISK PREFIX) ? "
WRITE SYS$OUTPUT " "
WRITE SYS$OUTPUT "THE CLEAR IMAGE IS ASSUMED TO HAVE THAT FOLLOWED BY Z"
WRITE SYS$OUTPUT " "
!
INQUIRE P2 "PDS CORRECTION LUT FILE (INCLUDE DISK PREFIX) ?"
!
!
!
INQUIRE P3 "TYPE Y FOR ALLOWING FOR FILM SATURATION, N FOR NOT"
IF P3 .EQS. "N" THEN GOTO 3A
INQUIRE P4 "FILM SATURATION DENSITY?"
3A:
!
!
!
INQUIRE P5 "OUTPUT FILE (INCLUDE DISK PREFIX) ? "
!
!
!
INQUIRE P7 "TYPE Y IF WILL NEED TO MOUNT A DISK, N IF NOT"
IF P7 .EQS. "N" THEN GOTO 7A
INQUIRE P10 "PHYSICAL NAME OF DRIVE (eg DRC2:) ?"
INQUIRE P11 "LOGICAL NAME OF DISK (eg USER3) ?"
7A:
!
!
!
INQUIRE P8 "BATCH OR FASTBATCH (B/F) ?"
INQUIRE P9 "NAME TO BE GIVEN TO FILE EXECUTING BATCH JOB ?"
!
!
!
$ OPEN/WRITE OUTPUT_FILE 'P9'.COM
WRITE OUTPUT_FILE "$ SET NOVERIFY"
IF P7 .EQS. "N" THEN GOTO 7B
WRITE OUTPUT_FILE "$ MOUNT/SHARE ''P10' ''P11'"
7B:
WRITE OUTPUT_FILE "$ DSCL"
WRITE OUTPUT_FILE "PDSIM"
WRITE OUTPUT_FILE P1
WRITE OUTPUT_FILE P5
WRITE OUTPUT_FILE P2
WRITE OUTPUT_FILE P3
IF P3 .EQS. "Y" THEN WRITE OUTPUT_FILE P4
WRITE OUTPUT_FILE "STOP"
WRITE OUTPUT_FILE "$ EXIT"
$ CLOSE OUTPUT_FILE
$ IF P8 .EQS. "B" THEN SUBMIT 'P9'
$ IF P8 .EQS. "F" THEN SUBMIT/QUEUE=FASTBATCH 'P9'
!
!
!
!
!
