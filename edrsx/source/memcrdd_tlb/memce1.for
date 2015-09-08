      subroutine memce1(ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Add Gaussian errors to the coordinates of each data sample
*	centre held in array ME_st.
*
*SOURCE
*       MEMCE1.FOR in MEMCRDD.TLB
*
*METHOD
*	Cross scan and in scan offsets are selected at random
*	from a Gaussian distribution with mean of 0 and a standard 
*	deviation specified by the user. These offsets
*	are then resolved into pixel offsets parallel to both 
*	image axes, and the offsets added to the stored coordinate 
*	values. If the stored value lies off the edge of the image, then
*	a different random vector is used. If 5 attempts at this still
*       gives a position off the edge of the image, then the coordinates
*	are left unaltered.
*       
*ARGUMENTS       
*   INPUT:
*	ierr	integer		Inherited status: 0 - "OK so far"
*   OUTPUTS:
*       ierr    integer         Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*	/A7_COM/,/A8_COM/,/B0_COM/,/B5_COM/,/B6_COM/,/ME_COM/,/ZZ_COM/
*   WRITE:
*	/ME_COM/,
*		  Sample centre coordinate data files in ME_st
*
*SUBROUTINES CALLED
*       NAG (single precision):
*		g05cce,g05dde
*              
*VAX SPECIFICS
*       implicit none
*       do while
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 7/11/89
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

* ... SIZE OF BLANK MARGIN REQUIRED ROUND IMAGE
      include '(A7_COM)'

* ... GROUP DIVISION INFO
      include '(A8_COM)'

* ... FITS DESCRIPTORS OF SKY IMAGE
      include '(B0_COM)'

* ... START AND END OF EACH GROUP
      include '(B5_COM)'

* ... POINTERS TO INTERNAL FILES HELD WITHIN ME_ST
      include '(B6_COM)'

* ... MEMSYS3 COMMON BLOCKS
      include '(ME_COM)'

* ... PARAMETER VALUES
      include '(ZZ_COM)'

*
* DECLARE ARGUMENTS
*
      integer	ierr

*
* DECLARE LOCAL PARAMETERS
*
      real	pi
      parameter (pi=3.1415927)

*
* DECLARE LOCAL VARIABLES
*
      real	cosfac	! COSINE factor for resolving in/cross onto x/y
      logical	done	! TRUE if the current sample centre has been 
			! succesfully modified
      real	g05dde	! Gaussian random value
      integer	group	! Current sample group
      real	inscan	! In scan offset in arcmins
      integer	istat	! Temporary status value
      real	margin	! Blank margin width in units of image pixels
      integer	nbad	! No. of failed attempts to modify the current 
			! sample centre
      real	rval	! Dummy real argument
      integer	samp	! Offset to current sample
      integer	sgroup	! Curent groups scan group number
      real	sigma	! Pointing error in units of image pixels
      real	sinfac	! SINE factor for resolving in/cross onto x/y
      real	xnew	! Candidate value for replacing current samples
			! X value
      real	xscan	! Cross scan offset in arcmins
      real	ynew	! Candidate value for replacing current samples
			! Y value

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* IF POINTING ERROR IS ZERO, LEAVE IMMEDIATELY
*
      if(ZZ_ins.le.0.and.ZZ_xs.le.0) goto 999

*
* INITIALISE THE NAG RANDOM NUMBER GENERATORS TO A NON-REPEATABLE VALUE
*
      call g05cce

*
* SCALE BLANK MARGIN SIZE FROM ARCMINS INTO IMAGE PIXELS
*
      margin=A7_mar/ZZ_psz

*
* LOOP ROUND EACH GROUP
*
      do group=1,A8_ngp
         if(B5_lgs(group).eq.0) goto 10

*
* CALCULATE SCAN GROUP NUMBER FOR THIS SAMPLE GROUP
*
         sgroup=mod(group-1,A8_nsg)+1

*
* SAVE SCAN ANGLE FUNCTIONS
*
         sinfac=sind(B0_fit(7)-A8_ang(sgroup))/ZZ_psz 
         cosfac=cosd(B0_fit(7)-A8_ang(sgroup))/ZZ_psz

*
* LOOP ROUND FOR EVERY SAMPLE IN THE CURRENT GROUP
*
         do samp=B5_fgs(group),B5_lgs(group)

*
* SET NO. OF BAD ATTEMPTS TO ZERO AND FLAG THAT THIS SAMPLE IS NOT YET 
* DONE
*
            done=.false.
            nbad=0

*
* LOOP ROUND UNTIL EITHER A GOOD OFFSET IS FOUND FOR THIS SAMPLE, OR
* 5 ATTEMPTS HAVE BEEN DONE. 
*
            do while(.not.done.and.nbad.le.5)

*
* CALCULATE THE RANDOM OFFSET VECTORS
*
               inscan=g05dde(0.0,ZZ_ins)
               xscan=g05dde(0.0,ZZ_xs)

*
* RESOLVE IT INTO CARTESIAN COORDINATES AND ADD ON TO THE ORIGINAL 
* COORDINATES OF THE SAMPLE CENTRE
*
               xnew=ME_st(B6_x+samp+1)+inscan*sinfac-xscan*cosfac
               ynew=ME_st(B6_y+samp+1)+inscan*cosfac+xscan*sinfac

*
* CHECK THAT THESE NEW COORDS ARE INSIDE THE USABLE AREA OF THE IMAGE.
* IF THEY ARN'T, INCREMENT THE NO. OF BAD ATTEMPTS AND GO ROUND FOR THE
* NEXT ATTEMPT.
*
               if(xnew.lt.margin.or.xnew.gt.B0_nps-margin.or.
     :            ynew.lt.margin.or.ynew.gt.B0_nls-margin) then
                  nbad=nbad+1

               else
*
* IF THE NEW COORDS ARE WITHIN THE USABLE PART OF THE IMAGE, THEN SAVE
* THE NEW COORDS AND FLAG THAT THIS SAMPLE IS DONE
*
                  ME_st(B6_x+samp+1)=xnew
                  ME_st(B6_y+samp+1)=ynew
                  done=.true.

               endif
*
* DO NEXT ATTEMPT AT THIS SAMPLE
*
            enddo

*
* DO NEXT SAMPLE IN THIS GROUP
*
         enddo

*
* DO NEXT GROUP
*
  10    continue
      enddo

*
* FINISH
*
  999 continue

      end
