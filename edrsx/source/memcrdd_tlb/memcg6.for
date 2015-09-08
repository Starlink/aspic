      subroutine memcg6(ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Sets up the internal structures required by MEMCRDD and 
*	to generate blurred CRDD.
*
*SOURCE
*       MEMCG6.FOR in MEMCRDD.TLB
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
*
*SUBROUTINES CALLED
*       THIS PACKAGE (MEMCRDD.TLB):
*              memcb5,memcb6,memcb7,memcc1,memcd3,memcf7
*
*VAX SPECIFICS
*       implicit none
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 1/11/89
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

* ... POINTERS TO INTERNAL FILES
      include '(B6_COM)'

* ... USER SUPPLIED PARAMETER VALUES
      include '(ZZ_COM)'

*
* DECLARE ARGUMENTS
*
      integer	ierr

*
* DELCARE LOCAL VARIABLES
*
      integer	ipvar
      integer	istat
      integer	iw1(500)
      integer	offset
      real	rw1(500)
      real	rw2(500)
      real	rw3(500)
      real	rw4(500)

*
* IF INHERITED STATUS INDICATES AN ERROR, EXIT IMMEDIATELY
*
      if(ierr.ne.0) goto 999

*
* SET UP NO. OF PIXELS IN AN IMAGE
*
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
      call memcg7(ierr)

*
* REORDER THE DATA STORED IN ME_ST BY ROUTINE MEMCB5, BY GROUP NUMBER.
* THIS RESULTS IN SAMPLES BELONGING TO THE SAME SAMPLE GROUP BEING 
* STORED NEXT TO EACH OTHER. 
*
      call memcb7(ierr)
      if(ierr.ne.0) goto 999

*
* COPY THE INPUT CRDD TO FILE 22
*
      do offset=0,ME_mk-1
         ME_st(ME_kb(22)+offset)=ME_st(ME_kb(21)+offset)
      enddo

*
* FILTER THE CRDD
*
      call memcf7(ME_st(ME_kb(21)),ME_st(B6_wk1),
     :            ME_st(B6_wk2),ME_st(B6_wk3),ME_st(B6_wk4),ierr)

*
* PLOT THE DATA RESIDUALS AGAINST SMOOTHED DATA VALUE, AND CALCULATE
* ESTIMATES OF THE ERROR BASED ON THE SPREAD OF THE ORIGINAL CRDD 
* VALUES FROM THE FILTERED CRDD
*
      do offset=0,ME_mk-1
         ME_st(ME_kb(22)+offset)=ME_st(ME_kb(22)+offset)
     :                           -ME_st(ME_kb(21)+offset)
      enddo

      call memch0(ME_st(ME_kb(21)),ME_st(ME_kb(22)),ME_mk,PR_rin,50,
     :            iw1,rw1,rw2,rw3,rw4,ierr)

*
* WRITE THE NOISE VALUE TO DISK
*
      call gt2diw('VAROUT',204,.true.,ME_mk,1,ipvar,istat)

      if(istat.eq.0) then

         do offset=0,ME_mk-1
            ME_st(ME_kb(22)+offset)=1.0/ME_st(ME_kb(22)+offset)
         enddo

         call wrvar(%val(ipvar),ME_mk,1)
         call frdata('VAROUT',istat)
      endif
      call cnpar('VAROUT',istat)

*
* FINISH
*
  999 continue

      end
