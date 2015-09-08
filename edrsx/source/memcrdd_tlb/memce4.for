      subroutine memce4(file,crddf,det,out,size)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Extract the data for a given detector data stream from the given
*	internal data set.
*
*SOURCE
*       MEMCE4.FOR in MEMCRDD.TLB
*
*METHOD
*	Loop round each data sample in the data set examining the SDC
*	identifier. Store any data sample relating to the given data
*	stream at the correct sample position in the output.
*       
*ARGUMENTS       
*   INPUT:
*	file	integer		The input intenal file no.
*	crddf	integer		Index of CRDD file holding required 
*				data stream
*	det	integer	 	Index of detector which generated 
*				required data stream.
*	size	integer		Size of output array
*	ierr	integer		Inherited status: 0 - "OK so far"
*
*   OUTPUTS:
*	out(size) real		Output array to hold extracted data stream
*       ierr    integer         Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*	/B5_COM/,/B6_COM/,/ME_COM/
*
*SUBROUTINES CALLED
*       none
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 13/12/89
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

* ... VALUES NEEDED TO UNPACK THE SDC IDENTIFIERS
      include '(B5_COM)'

* ... POINTERS TO THE START OF MEMCRDD INTERNAL FILES
      include '(B6_COM)'

* ... MEMSYS3 DATA
      include '(ME_COM)'

*
* DECLARE ARGUMENTS
*
      integer	ierr,file,crddf,det,size
      real	out(size)

*
* DECLARE LOCAL VARIABLES
*
      integer	offset	! Offset to current sample from start of a data set
      integer	samp	! Sample no. from input CRDD file
      integer	sdc	! SDC identifier for current sample
      integer	tcrdd	! CRDD file index which contains current sample
      integer	tdet	! Detector index which generated current sample

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* INITIALISE THE OUTPUT TO THE "INVALID" VALUE (I.E. "NO DATA PRESENT")
*
      do samp=1,size
         out(samp)=PR_rin
      enddo

*
*
* LOOP ROUND ALL SAMPLES IN THE INPUT DATA SET
*
      do offset=0,ME_mk-1

*
* GET THE SDC IDENTIFIER FOR THE CURRENT SAMPLE, AND EXTRACT THE CRDD
* FILE INDEX
*
         sdc=ME_st(B6_sdc+offset)
         tcrdd=mod(sdc/16,B5_cfa)+1

*
* IF THIS SAMPLE IS NOT FROM THE REQUIRED CRDD FILE THEN PASS ON TO 
* THE NEXT SAMPLE
*
         if(tcrdd.eq.crddf) then

*
* IF THIS SAMPLE IS FROM THE REQUIRED CRDD FILE THEN EXTRACT THE
* DETECTOR INDEX
*
            tdet=mod(sdc,16)+1

*
* IF THIS SAMPLE IS NOT FROM THE REQUIRED DETECTOR THEN PASS ON
*
            if(tdet.eq.det) then

*
* IF IT IS, THEN EXTRACT THE SAMPLE INDEX AND STORE THE INPUT DATA VALUE
* AT THE EXTRACTED SAMPLE POSITION IN THE OUTPUT FILE
*
               samp=sdc/(16*B5_cfa)+1
               out(samp)=ME_st(ME_kb(file)+offset)

            endif
         endif

*
* DO NEXT SAMPLE
*
      enddo

*
* FINISH
*
  999 continue

      end
