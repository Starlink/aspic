      subroutine memcc0(ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Sets up the default entropy model.
*
*SOURCE
*       MEMCC0.FOR in MEMCRDD.TLB
*
*METHOD
*	The user is given the option of giving an EDRS image containing
*       the default model. Before use, the given image has the stored 
*       background image removed. The resulting image is limited to be
*	greater than an estimate of the background noise. 
*          If the user does not give an image, then the model used
*       is flat with value equal to the background surface brightness in
*       the data. In either case, the model is stored in internal file
*       20.
*       
*ARGUMENTS       
*   INPUT:
*	ierr	integer		Inherited status: 0 - "OK so far"
*   OUTPUTS:
*       ierr    integer         Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*	/B6_COM/,/ME_COM/,/ZZ_COM/
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*	       wruser
*       THIS PACKAGE (MEMCRDD.TLB):
*              memcf3
*       EDRS:
*              wrerr
*	INTERIM:
*	       cnpar,frdata
*              
*STARLINK PARAMETERS
*	MODEL		An image containing the model to use
*	C0ERR1(error)	Accessed if the background surface brightness of
*			the data is non-positive
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

      include '(B5_COM)'

* ... POINTERS TO INTERNAL FILES
      include '(B6_COM)'

* ... MEMSYS3 INFO
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
      real	back	! Background image value
      character	bdfnam*30! Name of BDF containing model image
      real	defsb	! Default surface brightness 
      real	flux	! Data sample flux value in Jy
      integer	istat	! Temporary status value
      real	model	! Supplied model pixel value
      real	newmod	! Model pixel value after background subtraction
      integer	offset	! Offset into an internal data set or image
      real	omega	! Sample solid angle in Steradians
      character	prbuf*80! Buffer for screen output
      real	sbsum	! Sum of surface brightness values (in Jy/St)
      real	sigma	! Spread in background values

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* CALCULATE A DEFAULT CONSTANT MODEL VALUE. THIS IS THE MEAN SURFACE
* BRIGHTNESS VALUE IN THE DATA
*
      call getbac(ME_st(ME_kb(21)),ME_st(B6_sol),ME_mk,defsb)

*
* ATTEMPT TO GET AN IMAGE FROM THE USER TO USE AS THE DEFAULT MODEL
*
      call memcf3('MODEL',.true.,ME_st(ME_kb(20)),PR_rin,bdfnam,istat)

*
* IF NO IMAGE WAS OBTAINED, SET THE DEFAULT MODEL EQUAL TO THE DEFAULT
* CONSTANT SURFACE BRIGHTNESS
*
      if(istat.ne.0) then

         do offset=0,ME_mj-1
            ME_st(ME_kb(20)+offset)=defsb
         enddo

*
* IF REQUIRED, TELL THE USER WHAT THE DEFAULT MODEL VALUE IS
*
         if(ZZ_ilv.ge.3) then
            call wruser(' ',istat)
            write(prbuf,10) defsb
  10        format('  Using constant model value of ',G13.6,' Jy/st')
            call wruser(prbuf,istat)
            call wruser('  (after background subtraction).',istat)
         endif

*
* OTHERWISE, IF AN IMAGE WAS OBTAINED SUCCESFULLY...
*
      else

*
* SUBTRACT OFF THE STORED BACKGROUND IMAGE, REPLACE INVALID OR NEGATIVE 
* PIXELS WITH THE DEFAULT SURFACE BRIGHTNESS VALUE
*
         do offset=0,ME_mj-1

            model=ME_st(ME_kb(20)+offset)
            back=ME_st(B6_bac+offset)

            if(model.ne.PR_rin.and.back.ne.PR_rin) then

               model=model-ME_st(B6_bac+offset)
               if(model.le.0) model=defsb
               ME_st(ME_kb(20)+offset)=model

            else
               ME_st(ME_kb(20)+offset)=defsb
            endif

         enddo                  

      endif

*
* FINISH
*
  999 call frdata('MODEL',istat)
      call cnpar('MODEL',istat)

      end



C---------------------------------------------------------------
      subroutine getbac(flux,omega,size,mean)
      implicit none
      integer	size,i,nval
      real	flux(size),omega(size),mean,sum,sb,maxsb,minsb

      maxsb=-1.0E32
      minsb=1.0E32

      do i=1,size
         if(omega(i).gt.0.0) then
            sb=flux(i)/omega(i)
            sum=sum+sb
            nval=nval+1
            maxsb=max(maxsb,sb)
            minsb=min(minsb,sb)
         endif
      enddo

      if(nval.gt.0) then
         mean=sum/nval
      else
         mean=0.01*(maxsb-minsb)
      endif

      end
