      subroutine memcc8(start,crdd,nsamp,ndet,dim3,crddf,output)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Copies data from a given internal data set to an output 
*	file. Only the samples which derive from a specified
*	input CRDD file are copied.
*
*SOURCE
*       MEMCC8.FOR in MEMCRDD.TLB
*
*METHOD
*	The SDC idnetifiers stored for each usable sample (see routine
*	MEMCB5) are used to identify the samples which should be put
*	in the output file. The SDC identifiers also give the location
*	within the output file. The scale and zero terms used to
*	generate the output integers, are those used to derive internal
*	values from the input integers.
*	
*	Any unusable samples are left invalid in the output.
*       
*ARGUMENTS       
*   INPUT:
*	start	integer		Location of start of input data set 
*				within the array ME_st
*	crdd	integer		Input CRDD file
*	nsamp	integer		No. of samples per detector
*	ndet	integer		No. of detectors
*	dim3	integer		No. of planes of data
*	crddf	integer		CRDD file index
*   OUTPUTS:
*	output	integer		Output CRDD file
*
*COMMON USAGE
*   READ:
*	/B2_COM/,/B5_COM/,/B6_COM/,/ME_COM/
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
*       D.S. Berry (MAVAD::DSB) 6/10/89
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

* ... INFO ABOUT INPUT CRDD FILES
      include '(B2_COM)'

* ... INFO ABOUT PACKING OF VALUES IN SDC IDFENTIFIERS
      include '(B5_COM)'

* ... POINTERS TO START OF MEMCRDD WORK FILES IN ME_ST ARRAY
      include '(B6_COM)'

* ... MEMSYS3 COMMON BLOCKS
      include '(ME_COM)'

*
* DECLARE ARGUMENTS
*
      integer	nsamp,ndet,dim3,crddf,start,output(nsamp,ndet,dim3),
     :          crdd(nsamp,ndet,dim3)

*
* DECLARE LOCAL VARIABLES
*
      integer	det 	! Detector which generated the current sample
      real	intval	! Internal CRDD sample value
      integer	offset	! Offset into the data set
      integer	samp	! Sample no. within the input CRDD file
      integer	scrddf	! Index of CRDD file which contained current sample
      integer	sdc	! SDC identifier (see routine MEMCB5)

*
* IF DEALING WITH AO DATA, COPY THE X AND Y VALUES FROM THE INPUT
* FILE TO THE OUTPUT, AND SET ALL OUTPUT DATA VALUES INVALID. 
*
      if(dim3.eq.3) then
         do det=1,ndet
            do samp=1,nsamp
               output(samp,det,1)=B2_bpx(crddf)
               output(samp,det,2)=crdd(samp,det,2)
               output(samp,det,3)=crdd(samp,det,3)
            enddo
         enddo

*
* IF DEALING WITH SURVEY DATA, JUST SET THE OUTPUT DATA INVALID
*
      else
         do det=1,ndet
            do samp=1,nsamp
               output(samp,det,1)=B2_bpx(crddf)
            enddo
         enddo
      endif

*
* IF THE INPUT CRDD FILE HAD ZERO SCALE FACTOR, QUIT
*
      if(B2_bsc(crddf).eq.0.0) goto 999

*
* LOOP THROUGH ALL SAMPLES IN THE DATA SET
*
      do offset=0,ME_mk-1

*
* GET THE CRDD FILE INDEX FROM THIS SAMPLES SDC IDENTIFIER (SEE ROUTINE
* MEMCB5)
*
         sdc=ME_sti(B6_sdc+offset)
         scrddf=mod(sdc/16,B5_cfa)+1
*
* IF THIS IS NOT THE SELECTED CRDD FILE, PASS ON TO NEXT SAMPLE
*
         if(scrddf.eq.crddf) then

*
* GET SAMPLE AND DETECTOR VALUES FROM THE SDC IDENTIFIER
*
            det=mod(sdc,16)+1
            samp=sdc/(16*B5_cfa)+1

*
* CHECK THE INTERNAL DATA VALUE IS VALID
*
            intval=ME_st(start+offset)
            if(intval.ne.PR_rin) then

*
* CALCULATE THE INTEGER VALUE CORRESPONDING TO THE INTERNAL DATA VALUE,
* USING THE SAME SCALE AND ZERO FACTORS AS WERE USED TO GENERATE THE 
* INTERNAL VALUES FROM THE INPUT CRDD FILE INTEGERS. STORE AT THE 
* CORRECT POINT IN THE OUTPUT CRDD FILE
*
               output(samp,det,1)=nint((intval-B2_bze(crddf))/
     :                                  B2_bsc(crddf))
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
