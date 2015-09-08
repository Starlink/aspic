      subroutine memcb7(ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Reorders the data written by routine MEMCB5 at the low end of
*	the array ME_st, so that samples are grouped by group number. 
* 	The reordered data is stored in the correct internal files for
*	use by MEMSYS3.
*
*SOURCE
*       MEMCB7.FOR in MEMCRDD.TLB
*
*ARGUMENTS       
*   INPUT:
*	ierr	integer		Inherited status: 0 - "OK so far"
*   OUTPUTS:
*       ierr    integer         Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*	/A8_COM/,/B5_COM/,/B6_COM/,/ME_COM/,/ZZ_COM/
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              wruser
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 4/10/89
*-------------------------------------------------------------------
      implicit none

*
* INCLUDE GLOBAL PARAMETER DECLARATIONS
*
      include '(PR_DEC)'

*
* INCLUDE IRAS MISSION PARAMETERS
*
      include 'UTILITIES(IR_PAR)'

*
* INCLUDE COMMON BLOCKS HOLDING...

* ... SCAN AND DETECTOR GROUP INFO
      include '(A8_COM)'

* ... SAMPLE GROUP BOUNDARIES
      include '(B5_COM)'

* ... POINTERS TO INTERNAL FILES
      include '(B6_COM)'

* ... MEMSYS3 INFORMATION
      include '(ME_COM)'

* ... USER SUPPLIED PARAMETER VALUES
      include '(ZZ_COM)'

*
* DECLARE ARGUMENTS
*
      integer	ierr

*
* DECLARE LOCAL VARIABLES
*
      integer	group	! Sample group number
      integer	istat	! Temporary status value
      integer	grpoff(PR_grp) ! Offset into a data set of the next 
			! free sample in each group
      real	omega	! Solid angle of detector
      integer	point	! Pointer to next value from the low end of ME_st
      integer	samp	! Sample count
      real	flux	! Samples flux in Jy
      integer	sdc	! An Sample,Detector,Crdd file identifier
      real	var	! Variance of sample value
      real	x	! Pixel no. at sample centre
      real	y	! Line no. at sample centre

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* IF REQURIED, TELL THE USER WHATS GOING ON
*
      if(ZZ_ilv.ge.4) then
         call wruser(' ',istat)
         call wruser('  Reordering data...',istat)
      endif

*
* INITIALISE GROUP OFFSETS TO START OF EACH GROUP
*
      do group=1,A8_ngp
         grpoff(group)=B5_fgs(group)-1
      enddo

*
* INITIALISE POINTER INTO LOW END OF ME_ST
*
      point=1

*
* LOOP ROUND ALL DATA SAMPLES
*
      do samp=1,ME_mk

*
* READ DATA OUT OF THE LOW END OF ME_ST
*
         x=ME_st(point)
         y=ME_st(point+1)
         flux=ME_st(point+2)
         var=ME_st(point+3)
         group=ME_sti(point+4)
         sdc=ME_sti(point+5)
         omega=ME_st(point+6)

*
* STORE THE DATA IN THE DETINATION FILES, AT THE NEXT FREE LOCATION
* IN THE SAMPLES GROUP
*
         ME_st(B6_x+grpoff(group))=x
         ME_st(B6_y+grpoff(group))=y
         ME_st(ME_kb(21)+grpoff(group))=flux
         ME_st(ME_kb(22)+grpoff(group))=var
         ME_sti(B6_sdc+grpoff(group))=sdc
         ME_st(B6_sol+grpoff(group))=omega

*
* UPDATE THE POINTERS TO THE NEXT FREE LOCATION IN EACH GROUP, AND TO
* THE NEXT VALUE FROM THE LOW END OF ME_ST
*
         grpoff(group)=grpoff(group)+1
         point=point+7

      enddo
            
*
* FINISH
*
  999 continue

      end
