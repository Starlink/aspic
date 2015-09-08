      subroutine memcd9(ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Generate simulated data and output it to disk.
*
*SOURCE
*       MEMCD9.FOR in MEMCRDD.TLB
*
*METHOD
*	The OPUS routine is used twice. First it is applied to
*	the user-supplied trial sky image held in internal file 1.
*	Invalid pixels have been replaced with the value zero in
*	this image. The effect of such zero pixels are divided out
*	of the data sample values by applying OPUS to an image set
*	to 1.0 at each valid sky pixel, and 0.0 at each invalid sky
*	pixel, and dividing the first data set by the second and then
*	multiplying by the sample solid angle.
*	Any sample which has a value of zero in the second data set
*	(i.e. no valid data contributed to it) is set blank in the
*	output data set.
*	A gaussian noise value is then added to each valid data
*	sample.
*       
*ARGUMENTS       
*   INPUT:
*	ierr	integer		Inherited status: 0 - "OK so far"
*   OUTPUTS:
*       ierr    integer         Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*	/ME_COM/,/B6_COM/,/E0_COM/,/ZZ_COM/
*
*SUBROUTINES CALLED
*       THIS PACKAGE (MEMCRDD.TLB):
*              memca2,opus
*       NAG (single precision):
*              g05cce,g05dde
*
*STARLINK PARAMETERS
*	OUTPUT		The output CRDD bdfs
*
*VAX SPECIFICS
*       implicit none
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

* ... POINTER TO SOLID ANGLE DATA
      include '(B6_COM)'

* ... NO. OF INVALID PIXELS IN SKY IMAGE
      include '(E0_COM)'

* ... MEMSYS3 COMMON BLOCKS
      include '(ME_COM)'

* ... PARAMETER VALUES
      include '(ZZ_COM)'

*
* DECLARE ARGUMENTS
*
      integer	ierr

*
* DECLARE LOCAL VARIABLES
*
      real	data	! Data value obtained from supplied sky image
      real	g05dde	! Random Gaussian value
      integer	istat	! Temporary status value
      real	norm	! Normalisation factor for data value to divide
			! out the effecct of invalid pixels
      integer	samp	! Offset to current sample

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* IF REQUIRED, CALL UDIAG
*
      if(ZZ_ilv.eq.4) call udiag('>>> UDIAG called Prior to data '//
     :                           'simulation:')
*
* GENERATE DATA FROM THE GIVEN IMAGE. THIS DATA WILL INCLUDE AN EFFECT
* FROM THE INVALID PIXELS WHICH ARE SET TO ZERO IN FILE 1
*
      call opus(1,23)

*
* GENERATE DATA FROM THE INVALID PIXEL MASK. THIS DATA WILL GIVE A VALUE
* PROPORTIONAL TO THE NUMBER OF VALID SKY PIXELS CONTRIBUTING TO EACH
* DATA SAMPLE. IF ALL PIXELS ARE VALID IN THE INPUT IMAGE THEN 
* THE DATA VALUES WILL ALL HAVE A NORMALISATION FACTOR OF 1.0
*
      if(E0_val.lt.ME_mj) call opus(2,24)

*
* INITIALISE THE NAG RANDOM NUMBER GENERATORS TO A NON-REPEATABLE VALUE
*
      call g05cce

*
* LOOP ROUND EACH USABLE DATA SAMPLE
*
      do samp=0,ME_mk-1

*
* EXTRACT THE DATA VALUE AND DIVIDE THE SMOOTHED MASK VALUE BY THE
* SAMPLE SOLID ANGLE TO GET A NORMALISATION VALUE BETWEEN 0.0 AND 1.0.
* IF THERE WERE NO INVALID PIXELS IN THE SKY IMAGE, THEN ALL 
* NORMALISATION VALUES WILL BE 1.0.
*
         data=ME_st(ME_kb(23)+samp)

         if(E0_val.lt.ME_mj) then
            norm=ME_st(ME_kb(24)+samp)/ME_st(B6_sol+samp)
         else
            norm=1.0
         endif

*
* IF SUFFICIENT OF THIS DATA SAMPLE SAW VALID DATA, THEN NORMALISE IT
* AND ACCEPT IT AS A VALID DATA SAMPLE. ALSO ADD ON A GAUSSIAN NOISE
* VALUE WITH STANDARD DEVIATION GIVEN BY THE USER
*
         if(norm.gt.PR_cut) then
            ME_st(ME_kb(23)+samp)=(data/norm)+g05dde(0.0,ZZ_fld)

*
* IF THE SAMPLE SAW INSUFFICIENT VALID SKY DATA, THEN SET THE SAMPLE 
* BLANK
*
         else
             ME_st(ME_kb(23)+samp)=PR_rin
         endif

*
* DO THE NEXT SAMPLE
*
      enddo

*
* ADD THE REQUIRED AMOUNT OF STRIPING ONTO THE DATA
*
      call stripe(23)

*
* WRITE OUT THE DATA TO DISK
*
      call memca2('OUTPUT',23,.false.,ierr)

*
* FINISH
*
  999 continue

      end

*--------------------------------------------------------
      subroutine stripe(file)
      implicit none

      include '(PR_DEC)'
      include 'UTILITIES(IR_PAR)'
      include '(B5_COM)'
      include '(B6_COM)'
      include '(ME_COM)'

      integer	ival,istat,file,offset,crddf,det,sdc
      real	mag,base(IR_dts,PR_crd),g05cae,dummy

      mag=0.0
      call getpar('STRIPE','REAL',1,0.0,1.0E32,.true.,ival,mag,istat)

      if(mag.gt.0.0) then

         do crddf=1,PR_crd
            do det=1,IR_dts
               base(det,crddf)=2.0*mag*(g05cae(dummy)-0.5)
            enddo
         enddo

         do offset=0,ME_mk-1
            sdc=ME_sti(B6_sdc+offset)
            det=mod(sdc,16)+1
            crddf=mod(sdc/16,B5_cfa)+1
            ME_st(ME_kb(file)+offset)=ME_st(ME_kb(file)+offset)+
     :                                base(det,crddf)
         enddo

      endif

      end
