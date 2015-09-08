      subroutine memce7(stripe,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Performs one destriping iteration.
*
*SOURCE
*       MEMCE7.FOR in MEMCRDD.TLB
*
*METHOD
*       
*ARGUMENTS       
*   INPUT:
*	real	stripe(IR_dts,PR_crd) Work space
*	ierr	integer		Inherited status: 0 - "OK so far"
*   OUTPUTS:
*       ierr    integer         Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*	/B2_COM/,/B5_COM/,/B6_COM/,/ME_COM/,/ZZ_COM/
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              rffrej,wruser
*       THIS PACKAGE (MEMCRDD.TLB):
*              memce3,memce4
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 19/12/89
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

* ... CRDD FILE INFO
      include '(B2_COM)'

* ... SDC UNPACKING INFO
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
      real	stripe(IR_dts,PR_crd)

*
* DECLARE LOCAL VARIABLES
*
      integer	crddf
      integer	det
      real	diff
      integer	istat	! Temporary status value
      real	mean
      integer	ngood
      integer	nsum
      integer	offset
      character	prbuf*80
      integer	samp
      integer	sdc
      real	sigma
      real	sum
      real	sumsq

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* PROJECT THE SURFACE BRIGHTNESS DATA HELD IN FILE 21 INTO THE WORK
* IMAGE AND THEN IMMEDIATELY SAMPLE THE WORK IMAGE TO GET A SIMULATED
* DATA SET IN WHICH THE EFFECT OF THE DIFFERENT STRIPE LEVELS IN THE 
* DIFFERENT DATA STREAMS WILL HAVE AVERAGED OUT A BIT.
*
      call memce3(ME_st(ME_kb(21)),ME_st(ME_kb(23)),ME_st(ME_kb(1)),
     :            ME_st(B6_wk1),ME_st(B6_wk2),ME_st(B6_wk3),ierr)
      if(ierr.ne.0) goto 999

*
* SUBTRACT THE SIMULATED DATA FROM THE REAL DATA
*
      do offset=0,ME_mk-1
         ME_st(ME_kb(23)+offset)=ME_st(ME_kb(21)+offset)-
     :                           ME_st(ME_kb(23)+offset)
      enddo

*
* EXTRACT EACH DETECTOR DATA STREAM IN TURN FROM THE DIFFERENCES
*
      do crddf=1,B2_ncf
         do det=1,B2_nde(crddf)

            call memce4(23,crddf,det,ME_st(ME_kb(22)),B2_nys(crddf))

*
* CLEAN THIS DIFFERENCES DATA STREAM
*
            call rffrej(ME_st(ME_kb(22)),B2_nys(crddf),1,10,3.0,10,1,
     :                  0,sigma,0.05,ME_st(ME_kb(23)),PR_rin,ngood,
     :                  ME_st(ME_kb(24)),ME_st(ME_kb(25)),
     :                  ME_st(ME_kb(26)),ME_st(ME_kb(27)))

*
* FIND THE MEAN VALUE OF THE CLEANED DIFFERENCES
*
            sum=0
            nsum=0
            do samp=0,B2_nys(crddf)-1
               diff=ME_st(ME_kb(23)+offset)
               if(diff.ne.PR_rin) then
                  sum=sum+diff
                  nsum=nsum+1
               endif
            enddo

*
* STORE THE MEAN DIFFERENCE VALUE
*
            if(nsum.gt.0) then
               stripe(det,crddf)=sum/nsum
            else
               stripe(det,crddf)=0.0
            endif               

*
* DO NEXT DATA STREAM
*
         enddo
      enddo

*
* SUBTRACT OFF THE DIFFERENCE VALUES FROM THE REAL DATA
*
      sum=0.0
      sumsq=0.0
      do offset=0,ME_mk-1
         sdc=ME_st(B6_sdc+offset)
         det=mod(sdc,16)+1
         crddf=mod(sdc/16,B5_cfa)+1
         diff=stripe(det,crddf)
         ME_st(ME_kb(21)+offset)=ME_st(ME_kb(21)+offset)-diff
         sum=sum+diff
         sumsq=sumsq+diff*diff
      enddo         

*
* ADD THE MEAN DIFFERENCE BACK ON TO EACH SAMPLE TO CONSERVE FLUX
*
      mean=sum/ME_mk
      sigma=sqrt(sumsq/ME_mk-mean*mean)
      do offset=0,ME_mk-1
         ME_st(ME_kb(21)+offset)=ME_st(ME_kb(21)+offset)+mean
      enddo         

*
* TELL USER SIGMA OF DIFFERENCES IN SAMPLE VALUES PRODUCED BY THIS 
* ITERATION
*
      if(ZZ_ilv.gt.2) then
         write(prbuf,10) sigma
  10     format('  Stripe noise removed: ',G13.6,' Jy/st')
         call wruser(prbuf,istat)
      endif
     
*
* FINISH
*
  999 continue

      end
