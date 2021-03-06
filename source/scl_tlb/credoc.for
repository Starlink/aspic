!+
!
!      PROCEDURE   *** CREDOC ***
!
!      IT ALLOWS USERS TO GENERATE A .GRF FILE
!      FULL OF DOCUMENTATION ABOUT A PROGRAM OR PROCEDURE
!
!      THIS IS THEN PASSED THROUGH GEROFF AND THE RESULTING
!      FILE PRINTED.
! 
!      IT HAS 2 OPTIONAL PARAMETERS:-
!
!            P1     THE NAME OF THE PROGRAM; IF ABSENT IT IS PROMPTED
!            P2     IF ABSENT IT IS SET TO "PROG" ; IT DEFINES
!                   WHAT IS TO BE PRINTED AT THE HEAD
!                   OF THE DOCUMENTATION
!
!      WRITTEN BY K F HARTLEY AT RGO ON 15/12/81
!
      IF P1.EQS."" THEN INQUIRE P1 "PROGRAM NAME?"
      IF P2.EQS."" THEN P2:=PROG
      DOCIT 'P2' 'P1'
!
!   DOCIT SENDS ITS OUTPUT TO 'P1'.GRF
!
      GEROFF 'P1'.GRF 'P1'.DOC
!
!   THE GEROFF OUTPUT IS NOW PRINTED
!
      PRINT/NOFLAG/NOFEED 'P1'.DOC
      EXIT
