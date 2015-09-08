! 
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!
!
!
!                     ********************** 
!                     *                    * 
!                     * Procedure   REALFR * 
!                     *                    * 
!                     ********************** 
!
!
!
!          CALLING SEQUENCE: 
!               REALFR [frame] 
!
!
!          FUNCTION: 
!               It extracts the REAL incarnation from a Starlink bulk  data 
!               frame,  copies  it into a dummy frame, deletes the original 
!               and renames the dummy to the original name. 
!
!
!          USE: 
!               It may be used to delete all but the REAL incarnation of  a 
!               frame, as a way of saving space when using ASPIC. Note that 
!               space is required for the dummy frame; errors can lose your 
!               original data frame. 
!
!
!         USER PARAMETERS: 
!
!         FRAME                               The name of the original (and 
!                                             final) .BDF frame. 
!
!
!         KFH                      RGO                             8-MAR-82 
!
!



IF P1.EQS."" THEN INQUIRE P1 "FRAME?"
INCARN R 'P1' DUMMY
DELETE 'P1'.BDF;*
RENAME DUMMY.BDF 'P1'.BDF
