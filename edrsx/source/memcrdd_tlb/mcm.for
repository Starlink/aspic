      subroutine mcm(mcmrun,chisq,istat)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	A very simple implementation of the "Maximum Correlation 
*       Method" algorithm decribed by Aumann et al in Astron. J. 99,
*       p1674 (May 1990).
*
*SOURCE
*       MCM.FOR in MEMCRDD.TLB
*
*METHOD
*       This routine implements the simple algorithm described in the 
*       paper, in terms of the OPUS and TROPUS routines used by MEMSYS3.
*       The process seems not to converge. The Chi squared statistic 
*       drops initially to a minimum value (often larger than 1.0) and
*       then increases again by a large amount, and finally starts 
*       oscillating. This routine terminates (sets ISTAT to zero) as
*       soon as the chisquared value reaches 1.0 or starts to rise.
*       
*ARGUMENTS       
*   INPUT:
*	mcmrun	integer		If 0 then a new run is initialised, 
*                               otherwise an old run is continued.
*       chisq   real		The chi squared value of the old 
*                               reconstruction. Not used if mcmrun is 0.
*
*   OUTPUTS:
*       chisq   real		The chi squared value of the new
*                               reconstruction. 
*       istat   integer         Exit status: 0 - termination reached.
*                                            1 - termination not reached.
*
*COMMON USAGE
*        The MEMSYS3 internal file structure is used, as defined by 
*        common blocks /MECOMP/ and /MECOMS/.
*
*        ON ENTRY: If mcmrun is not 0, file 1 should hold the current 
*                  reconstruction produced by the previous call to MCM.
*
*                  If mcmrun is 0, file 20 should hold the default 
*                  model, otherwise it should hold the mapped 
*                  accuracies produced by the first call to MCM.
*
*                  File 21 hold the data.
*
*                  If mcmrun is 0, file22 holds the original accuracy
*                  values (1/sigma), otherwise it should hold the 
*                  modified accuracies (1/variance) produced by the
*                  first call to MCM.
*
*                  If mcmrun is not 0, file 23 should hold the 
*                  simulated data for the current reconstruction.
*
*        ON EXIT:  File 1 holds the updated reconstruction .
*
*                  If mcmrun is 0, file 20 holds mapped accuracies.
*
*                  If mcmrun is 0, file 22 holds the modified accuracies
*
*                  File 23 holds the simulated data for the updated
*                  reconstruction.
*
*
*SUBROUTINES CALLED
*       THIS PACKAGE (MEMCRDD.TLB):
*              udiag, opus, tropus
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 12/11/90
*-------------------------------------------------------------------
      implicit none

      include '(PR_DEC)'

*
* INCLUDE COMMON BLOCKS HOLDING...

* ... MEMSYS3 INTERNAL FILE STRUCTURE
      include '(ME_COM)'

*
* DECLARE ARGUMENTS
*
      real	chisq
      integer	mcmrun,istat

*
* DECLARE LOCAL VARIABLES
*
      integer	offset
      real	sum
      real	data
      real	simdat
      real	acc
      real	cnew
      real	cor

*
* IF REQUIRED, ANNOUNCE ENTRY INTO MCM
*

      if(ME_l0.eq.3) call udiag('>>> Entry to MCM')


*
* IF REQUIRED, SET UP A NEW RUN OF MCM. 
*

      if(mcmrun.eq.0) then


*
* INITIALIZE THE CURRENT RECONSTRUCTION TO BE THE DEFAULT MODEL.
*

         do offset=0,ME_mj-1
            ME_st(ME_kb(1)+offset)=ME_st(ME_kb(20)+offset)
         enddo


*
* GENERATE SIMULATED DATA.
*

         call opus(1,23)


*
* CONVERT ACCURACIES TO INVERSE VARIANCES.
*

         do offset=0,ME_mk-1
            ME_st(ME_kb(22)+offset)=ME_st(ME_kb(22)+offset)**2
         enddo


* 
* MAP THE ACCURACIES INTO AN IMAGE IN FILE 20
*

         call tropus(22,20)


*
* SET CHISQ TO A LARGE VALUE
*

         chisq=1.0E30


*
* SET NOISE SCALING FACTOR TO 1.0
*

         ME_sig=1.0

      endif


*
* CALCULATE THE WEIGHTED CORRECTION FACTORS FOR EACH SAMPLE. ON ENTRY, 
* FILE 23 SHOULD HOLD THE DATA SIMULATED FROM THE CURRENT 
* RECONSTRUCTION WHICH WAS CALCULATED AT THE END OF THE PREVIOUS 
* ITERATION.
*

      do offset=0,ME_mk-1

         data=ME_st(ME_kb(21)+offset)
         simdat=ME_st(ME_kb(23)+offset)
         acc=ME_st(ME_kb(22)+offset)
         
         if(simdat.ne.0.0) then
            ME_st(ME_kb(23)+offset)=acc*data/simdat
         else
            ME_st(ME_kb(23)+offset)=acc
         endif

      enddo


*
* MAP THE CORRECTION FACTORS INTO AN IMAGE.
*

      call tropus(23,2)


*
* NORMALIZE THE CORRECTION FACTOR IMAGE, UPDATE THE CURRENT 
* RECONSTRUCTION AND STORE IN FILE 2.
*

      do offset=0,ME_mj-1

         acc=ME_st(ME_kb(20)+offset)

         if(acc.gt.0.0) then
            cor=ME_st(ME_kb(2)+offset)/acc
         else
            cor=1.0
         endif

         ME_st(ME_kb(2)+offset)=ME_st(ME_kb(1)+offset)*cor

      enddo


*
* GENERATE SIMULATED DATA FROM THE NEW RECONSTRUCTION.
*

      call opus(2,23)
      if(ME_l0.eq.3) call udiag('>>> Simulated data created in <23>')


*
* FIND THE CHI SQUARED STATISTIC FOR THE NEW RECONSTRUCTION.
*

      sum=0.0
      do offset=0,ME_mk-1

         data=ME_st(ME_kb(21)+offset)
         simdat=ME_st(ME_kb(23)+offset)
         acc=ME_st(ME_kb(22)+offset)
         
         sum=sum+acc*((data-simdat)**2)

      enddo
      cnew=sum


*
* IF THE CHI SQUARED OF THE NEW RECONSTRUCTION IS LARGER THAN THAN THAT
* OF THE OLD RECONSTRUCTION. SET ISTAT TO INDICATE TERMINATION AND 
* RETURN WITH THE SAME RECONSTRUCTION AS ON ENTRY.
*

      if(cnew.gt.chisq) then
         istat=0
         goto 999


*
* OTHERWISE COPY THE CURRENT RECONSTRUCTION FROM FILE 2 TO FILE 1
*

      else

         do offset=0,ME_mj-1
            ME_st(ME_kb(1)+offset)=ME_st(ME_kb(2)+offset)
         enddo


*
* RETURN THE NEW CHI SQUARED VALUE AND IF CHI SQUARED IS LESS THAN 1.0
* SET ISTAT TO INDICATE THAT TERMINATION HAS BEEN REACHED.
*

         chisq=cnew

         if(chisq.le.1.0) then
            istat=0
         else
            istat=1
         endif

      endif


*
* FINISH
*

  999 continue

      end
