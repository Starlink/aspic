      subroutine memcb1(ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Sets up the internal structures required by MEMCRDD and 
*	MEMSYS3, and then calls subroutines to activate MEMSYS3.
*
*SOURCE
*       MEMCB1.FOR in MEMCRDD.TLB
*
*ARGUMENTS       
*   INPUTS:
*	ierr	integer		Inherited status: 0 - "OK so far"
*   OUTPUTS:
*       ierr    integer         Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*	/B0_COM/,/ZZ_COM/
*   WRITE:
*	/ME_COM/
*		ME_mj		No. of pixels in an image
*		ME_out		Logical unit no. for MEMSYS3 diagnostics
*	/B1_COM/,
*		B1_it		Next iteration of MEM3 to perform
*
*SUBROUTINES CALLED
*       THIS PACKAGE (MEMCRDD.TLB):
*              memcb5,memcb6,memcb7,memcb8,memcc0,memcc1,memcc2,memcd3,
*	       memcd5,memcf4,memcf5,memce2,memce5
*
*VAX SPECIFICS
*       implicit none
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 26/1/90
*-------------------------------------------------------------------
      implicit none

*
* INCLUDE GLOBAL PARAMETER DECLARATIONS
*
      include '(PR_DEC)'

*
* INCLUDE COMMON BLOCKS HOLDING...

* ... MEMSYS3 DATA
      include '(ME_COM)'

* ... OUTPUT FRAME DESCRIPTORS
      include '(B0_COM)'

* ... OUTPUT VALUES FROM THIS ROUTINE
      include '(B1_COM)'

* ... USER SUPPLIED PARAMETER VALUES
      include '(ZZ_COM)'

      include '(B6_COM)'
*
* DECLARE ARGUMENTS
*
      integer	ierr

*
* DECLARE LOCAL VARIABLES
*
      integer	iter	! Iteration no. to start MEMSYS3 loop at
      integer	memrun	! Set to zero to initiate a new MEMSYS3 run
      integer	method	! MEMSYS3 method no. (see MEMSYS3 manual)

*
* IF INHERITED STATUS INDICATES AN ERROR, EXIT IMMEDIATELY
*
      if(ierr.ne.0) goto 999

*
* SET UP LOGICAL UNIT NO. FOR MEMSYS3 DIAGNOSTICS AND THE NO. OF PIXELS
* IN AN IMAGE
*
      ME_out=PR_uni
      ME_mj=B0_nps*B0_nls

*
* IDENTIFY USABLE SAMPLES, AND STORE DATA RELATING TO SUCH SAMPLES AT
* THE LOW END OF THE MAIN STORAGE ARRAY ME_ST. THIS DATA IS STORED IN
* THE ORDER IN WHICH THE SAMPLES WERE READ FROM THE INPUT CRDD FILES.
* SOME OTHER RELATED VALUES ARE CALCULATED AND STORED IN /B5_COM/.
*
      call memcb5(ierr)

*
* SET UP POINTERS TO THE START OF ALL INTERNAL MEM FILE, AND ALSO TO 
* WORK SPACE REQUIRED BY MEMCRDD
*
      call memcb6(ierr)

*
* REORDER THE DATA STORED IN ME_ST BY ROUTINE MEMCB5, BY GROUP NUMBER.
* THIS RESULTS IN SAMPLES BELONGING TO THE SAME SAMPLE GROUP BEING 
* STORED NEXT TO EACH OTHER. 
*
      call memcb7(ierr)

*
* SET UP THE FOURIER TRANSFORM OF EACH SAMPLE GROUPS PSF
*
      call memcc1(ierr)

*
* CREATE THE NOISE MODEL AND STORE ACCURACIES IN FILE 22 (THIS OPERATION
* MUST BE PERFORMED before THE BACKGROUND IS SUBTRACTED FROM THE DATA)
*
      call memcb8(ierr)

*
* SUBTRACT A BACKGROUND FROM THE DATA AND STORE THE BACKGROUND IMAGE IN 
* AN INTERNAL FILE POINTER TO BY B6_BAC
*
      call memcd3(ierr)

*
* REMOVE DATA WHICH IS MORE NEGATIVE THAN THE NOISE SINCE SUCH DATA CAN
* NEVER BE FITTED BY THE MEM ALGORITHM. TELL THE USER HOW MUCH DATA IS
* REMOVED FOR THIS REASON.
*
      call memcf4(ierr)

*
* SET UP THE DEFAULT MODEL FOR THE ENTROPY. 
*
      call memcc0(ierr)

*
* IF A COMBINATION OF CLASSIC AND HISTORIC MEMSYS3 IS TO BE USED, CALL 
* MEMCD5 TO CALL MEMSYS3 IN CLASSIC MODE EVERY FEW ITERATIONS. ENTER 
* MEMSYS3 WITH THE ITERATION NO. AT 1.
*
      if(ZZ_mth.eq.'COMBINATION') then
         B1_it=1
         call memcd5(B1_it,.true.,ierr)

*
* IF COMBINATION MODE IS NOT BEING USED, SET UP MEMSYS3 METHOD NUMBER
*
      else
         if(ZZ_mth.eq.'CLASSIC') then
            method=12
         else if(ZZ_mth.eq.'NONAUTO') then
            method=11
         else if(ZZ_mth.eq.'MCM') then
            method=-1
         else
            method=10
         endif

*
* CALL MEMCC2 TO INITIATE MEMSYS3. EACH CALL TO MEM3 WITHIN
* MEMCC2 USES THE SAME METHOD (SET ABOVE).
*
         memrun=0
         B1_it=1
         call memcc2(B1_it,method,memrun,ierr)
      endif

*
* IF REQUIRED, CREATE AN IMAGE WHICH CAN BE USED AS A "BETTER" MODEL 
* FOR THE NEXT RUN OF MEMCRDD
*
      call memce2(ierr)

*
* ADD THE BACKGROUND IMAGE BACK ON 
*
      call memcf5(ierr)

*
* FINISH
*
  999 continue

      end
