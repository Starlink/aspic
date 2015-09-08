      subroutine memcf4(ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Remove data which is more negative than the noise.
*
*SOURCE
*       MEMCF4.FOR in MEMCRDD.TLB
*
*METHOD
*	MEM cannot produce negative image pixel values. Therefore all
*       simulated data will be positive. If there are any negative
*       real data samples, then the residuals at these samples will 
*       not be able to get below the absolute value of the real data
*       sample. This could prevent convergence to the required value
*       of AIM being reached. 
*	   This routine removes all data samples which are less than
*	minus the sample noise value (stored in file 22). Such samples
*	are removed from files 21 and 22, and also from the files 
*	pointed to by B6_sdc, B6_x, B6_y and B6_sol. The value of ME_mk
*	is then reduced by the number of samples rejected, and the user
*	told how many samples have been rejected.
*	   The pointers to group boundaries contained in B5_fgs and
*	B5_lgs are also corrected for the reordering of the data sets.
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
*   WRITE:
*	/B5_COM/   B5_fgs Sample no. of start of each sample group
*	/B5_COM/   B5_lgs Sample no. of end of each sample group
*       /ME_COM/   ME_mk  Corrected number of usable data samples
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              wruser
*	EDRS:
*	       lbgone,wrerr
*              
*STARLINK PARAMETERS
*	F4ERR1(error)	Accessed if all data has been rejected
*	F4ERR2(error)	Accessed if any data was rejected
*	F4ERR3(error)	Accessed if the correced data is still 
*			significantly negative
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 26/1/90
*-------------------------------------------------------------------
      implicit none

*
* INCLUDE GLOBAL PARAMETER DECLARATIONS
*
      include '(PR_DEC)'

      include 'UTILITIES(IR_PAR)'

*
* INCLUDE COMMON BLOCKS HOLDING...

* ... NO. OF SAMPLE GROUPS
      include '(A8_COM)'

* ... SAMPLE GROUP BOUNDARIES
      include '(B5_COM)'

* ... POINTERS TO INTERNAL FILES
      include '(B6_COM)'

* ... MEMSYS3 infor
      include '(ME_COM)'

* ... USER PARAMETER VALUES
      include '(ZZ_COM)'

*
* DECLARE ARGUMENTS
*
      integer	ierr

*
* DECLARE LOCAL VARIABLES
*
      real	acc	! Data sample accuracy (=1/sigma)
      real	chisq	! Minimum possible value of chi squared
      real	flux	! Sample flux value in Jy
      integer	group	! Sample group no.
      integer	gstart	! New start position of the current sample group
      integer	istat	! Temporary status value
      integer	nrej	! No. of rejected samples
      integer	offset	! Offset into an internal data set or image
      character	prbuf*80! Buffer for screen output
      integer	read	! Pointer to next input sample
      real	sigma	! Noise estimate for current sample
      real	sum	! Sum of squared, normalised residuals
      integer	write	! Pointer to next available output sample

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* LOOP ROUND ALL SAMPLES
*
      nrej=0

      do offset=0,ME_mk-1

*
* IF THIS SAMPLE IS LESS THAN MINUS THE NOISE VALUE, THEN SET THE
* STORED ACCURACY TO ZERO, AND INCREMENT THE COUNT OF REJECTED SAMPLES
*
         flux=ME_st(ME_kb(21)+offset)
         sigma=1.0/ME_st(ME_kb(22)+offset)

         if(flux.lt.-sigma) then
            ME_st(ME_kb(22)+offset)=0.0
            nrej=nrej+1
         endif

      enddo

*
* IF ALL DATA HAS BEEN REJECTED, ABORT
*
      if(nrej.eq.ME_mk) then
         call wrerr('F4ERR1')
         ierr=1
         goto 999
      endif

*
* SHIFT DATA TO MAKE ALL DATA SETS CONTIGUOUS. FIRST INITIALIZE NEXT 
* DESTINATION SAMPLE TO OFFSET ZERO
*
      write=0

*
* LOOP THROUGH ALL NON-EMPTY SAMPLE GROUPS
*
      do group=1,A8_ngp
         if(B5_lgs(group).gt.0) then

*
* SAVE THE CURRENT DESTINATION SAMPLE AS THE NEW GROUP START POSITION
*
            gstart=write+1

*
* READ THROUGH ALL THE SAMPLES DEFINED IN THE OLD GROUP
*
            do read=B5_fgs(group)-1,B5_lgs(group)-1

*
* IF THIS SAMPLE PASSED THE REJECTION TEST, THEN WRITE IT TO THE
* CURRENT DESTINATION SAMPLE, AND UPDATE THE CURRENT DESTINATION
*
               if(ME_st(ME_kb(22)+read).gt.0) then

                  ME_st(ME_kb(21)+write)=ME_st(ME_kb(21)+read)
                  ME_st(ME_kb(22)+write)=ME_st(ME_kb(22)+read)
                  ME_sti(B6_sdc+write)=ME_sti(B6_sdc+read)
                  ME_st(B6_x+write)=ME_st(B6_x+read)
                  ME_st(B6_y+write)=ME_st(B6_y+read)
                  ME_st(B6_sol+write)=ME_st(B6_sol+read)

                  write=write+1
      
               endif

            enddo

*
* ONCE THE CURRENT GROUP HAS BEEN FINISHED, STORE THE NEW START AND END
* OF THE GROUP. IF THE GROUP IS EMPTY SET THE END SAMPLE POSITION TO 
* ZERO.
*
            B5_fgs(group)=gstart
            if(write.ge.gstart) then
               B5_lgs(group)=write
            else
               B5_lgs(group)=0
            endif

*
* DO NEXT NON-EMPTY GROUP
*
         endif
      enddo

*
* MODIFY THE NUMBER OF USABLE DATA SAMPLES
*
      ME_mk=ME_mk-nrej

*
* DO AN INTERNAL CONSISTENCY CHECK
*
      if(write.ne.ME_mk) stop '*** Internal error in MEMCF4'

*
* CALCULATE THE MINIMUM POSSIBLE CHI SQUARED VALUE. THIS ASSUMES THAT
* ALL POSITIVE DATA SAMPLES ARE FITTED EXACTLY, AND THAT NEGATIVE DATA
* SAMPLES PRODUCE SIMULATED DATA SAMPLES LIMITED TO ZERO.
*
      sum=0.0
      do offset=0,ME_mk-1
         flux=ME_st(ME_kb(21)+offset)
         acc=ME_st(ME_kb(22)+offset)
         if(flux.lt.0) sum=sum+(flux*acc)**2
      enddo

      chisq=sum/ME_mk

*
* IF THE MINIMUM POSSIBLE CHI SQUARED IS GREATER THAN ZERO, THEN
* WARN THE USER. ALSO TELL HIM HOW MANY SAMPLES WERE REJECTED.
*
      if(nrej.gt.0) then
         call wrerr('F4ERR2')
         write(prbuf,5) nrej
   5     format('  ',I8,' negative data sample(s) rejected')
         call lbgone(prbuf(3:))
         call wruser(prbuf,istat)
      endif

* 
* TELL USER ABOUT CHANGE IN BOUNDARIES OF SAMPLE GROUPS
*
      if(ZZ_ilv.ge.4.and.nrej.gt.0) then

         call wruser('  Sample group boundaries changed as follows:',
     :                istat)

         do group=1,A8_ngp
            if(B5_lgs(group).gt.0) then
               write(prbuf,6) group,B5_fgs(group),B5_lgs(group)
    6          format('  Group ',I3,' location, start: ',I8,
     :                ' end: ',I8)
               call wruser(prbuf,istat)
            else
               write(prbuf,7) group
    7          format('  Group ',I3,' is empty and will not be used')
               call wruser(prbuf,istat)
            endif
         enddo

      endif

      if(chisq.gt.0.0) then
         call wrerr('F4ERR3')
         write(prbuf,10) chisq
  10     format('  Minimum possible chi squared value: ',G13.6)
         call wruser(prbuf,istat)
      endif

*
* FINISH
*
  999 continue

      end
