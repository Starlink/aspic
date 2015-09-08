!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!
!
!
!                    !!!!!!!!!!!!!!!!!!!!!
!                    !                   !
!                    ! BATLORMUL.SCL     !
!                    !                   !
!                    !!!!!!!!!!!!!!!!!!!!!
!
!
!  THIS PROCEEDURE SETS UP AND RUNS LORMUL AS A BATCH JOB
!
!
!
!
!    THE PROGRAM PROMPTS FOR COMMANDS IN THE SAME MANNER AS "LORMUL"
! AND THEN CREATES A COMMAND FILE WHICH WILL RUN EITHER IN BATCH OR 
! FASTBATCH.THE PROGRAM APPEARS TO TAKE ABOUT 10 SECONDS OF CPU TIME 
! PER ITERATION WITH PROFILE RADII OF ABOUT 5 OR 6."LORMUL" ITSELF 
! PERFORMS A MAXIMUM OF 20 ITERATIONS PER STAR.
!
!
!
!    T A JONES AND A J PENNY                       29-AUG-82
!----------------------------------------------------------------
!
!
!
!
!
!
WRITE SYS$OUTPUT "RUNNING LORMUL AS A BATCH JOB"
WRITE SYS$OUTPUT " "
INQUIRE P1 " IMAGE ?"           ! MUST USE FULL (NOT ABBREVIATED) FILE NAME
INQUIRE P2 "XYLIST ?"
INQUIRE P3 "OUTPUT FILE ?"
INQUIRE P4 "OUTPUT FILE TITLE ?"
INQUIRE P13 "FILE CONTAINING PREVOIUS ABORTED MEASURES ?"
INQUIRE P5 "RX ?"
INQUIRE P6 "RY ?"
INQUIRE P7 "P ?"
INQUIRE P8 "PRX ?"
INQUIRE P9 "PRY ?"
INQUIRE P10 "BATCH OR FASTBATCH (B/F)"
INQUIRE P11 "NAME TO BE GIVEN TO FILE EXECUTING BATCH JOB"
INQUIRE P12 "IF USING USER3 TYPE 3, IF USER4 TYPE 4, IF NEITHER 0"
!
!
!
OPEN/WRITE OUTPUT_FILE 'P11'.COM
WRITE OUTPUT_FILE "$ SET NOVERIFY"
IF P12 .EQS. "3" THEN WRITE OUTPUT_FILE "$ MOUNT/SHARE DRC2: USER3"
IF P12 .EQS. "4" THEN WRITE OUTPUT_FILE "$ MOUNT/SHARE DRC2: USER4"
WRITE OUTPUT_FILE "$ LDSCL"
WRITE OUTPUT_FILE "LORMUL"
WRITE OUTPUT_FILE P1
WRITE OUTPUT_FILE P2
WRITE OUTPUT_FILE P3
WRITE OUTPUT_FILE P4
WRITE OUTPUT_FILE P13
WRITE OUTPUT_FILE P5
WRITE OUTPUT_FILE P6
WRITE OUTPUT_FILE P7
WRITE OUTPUT_FILE P8
WRITE OUTPUT_FILE P9
WRITE OUTPUT_FILE "STOP"
WRITE OUTPUT_FILE "$ EXIT"
CLOSE OUTPUT_FILE
IF P10 .EQS. "B" THEN SUBMIT 'P11'.COM
IF P10 .EQS. "F" THEN SUBMIT/QUEUE=FASTBATCH 'P11'.COM
