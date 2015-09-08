      subroutine deglit(data,ndet,nsamp,blank,band,work,scale,zero,varn,
     :                  ierr)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*SOURCE
*	DEGLIT.FOR in AODESTRIPE.TLB
*-----------------------------------------------------------------------
      implicit none

      include 'UTILITIES(IR_PAR)'

*
* DELCARE ARGUEMENTS
*
      integer	ndet,nsamp,blank,band,ierr,data(nsamp,ndet,3)
      real	work(nsamp,3),scale,zero,varn(IR_dts)

*
* DEFINE PARAMETERS
*
      real	rinval
      integer	fsize
      parameter	(rinval='FFFFFFFF'X,
     :           fsize = 30)

*
* DECLARE LOCAL VARIABLES
*
      integer	det,samp,ival,nrej,nhalf,index,niter,inter
      real	change,sumsq,nval,var,lvar,sigma,wval,sum,nsigma,
     :          filter(-fsize:fsize),xsigma,clplim,power

      data	nhalf/2/,nsigma/3.0/,niter/3/,clplim/2.0/,power/1.0/

      call snx_agop
      call agsetf('NUL/1.',rinval)


*
* LOOP ROUND EACH DETECTOR
*
      do det=1,ndet
*
* SET THE SUCCESS FLAG
*
         ierr=0

  11     write(*,*) 'Doing detector ',det
         write(*,'(A)') '$Enter 1 to interact: '
         read(*,*) inter

*
* FORM DIFFERENCES IN WORK1, SET WEIGHTS TO UNITY IN WORK2, AND STORE
* ORIGINAL DATA IN WORK3
*
         work(1,1)=rinval
         work(1,2)=0.0
         work(1,3)=scale*data(1,det,1)+zero
         do samp=2,nsamp
            if(data(samp,det,1).ne.blank.and.
     :         data(samp-1,det,1).ne.blank) then
               work(samp,1)=scale*(data(samp,det,1)-data(samp-1,det,1))
               work(samp,2)=1.0
               work(samp,3)=scale*data(samp,det,1)+zero
            else
               work(samp,1)=rinval
               work(samp,2)=0.0
               work(samp,3)=rinval
            endif
         enddo


      write(*,*) 'WORK1: differences'
      call seeit(work,nsamp,ndet,rinval,blank,scale,zero,data,det,
     :           nhalf,nsigma,niter,inter,clplim,power,ierr)
      if(ierr.ne.0) goto 11

*
* DO AN NSIGMA CLIP TO FIND THE SIGMA OF THE DIFFERENCES
*
         call sclip(work(1,1),work(1,2),nsamp,nsigma,niter,rinval,sigma,
     :              ierr)
         if(ierr.ne.0) goto 30
         write(*,*) 'Original sigma: ',sigma

*
* SMOOTH THE DIFFERENCES AND PUT IN WORK2
*
         call smooth(work(1,1),work(1,2),nsamp,rinval,nhalf)

      write(*,*) 'WORK1: differences,  WORK2: smoothed differences'
      call seeit(work,nsamp,ndet,rinval,blank,scale,zero,data,det,
     :           nhalf,nsigma,niter,inter,clplim,power,ierr)
      if(ierr.ne.0) goto 11

*
* TAKE MODULUS OF SMOOTHED DIFFERENCES AND SMOOTH, PUTTING ANSWER IN
* WORK3
*
      do samp=1,nsamp
         if(work(samp,2).ne.rinval) then
            work(samp,2)=abs(work(samp,2))
         endif
      enddo
      call smooth(work(1,2),work(1,3),nsamp,rinval,nhalf)

      write(*,*) 'WORK1: differences,  WORK2: twice smoothed diff'
      call seeit(work,nsamp,ndet,rinval,blank,scale,zero,data,det,
     :           nhalf,nsigma,niter,inter,clplim,power,ierr)
      if(ierr.ne.0) goto 11

*
* COPY THE TWICE SMOOTHED DIFFERENCES BACK TO WORK2 AND RESET WORK3
* TO THE DATA
*
      do samp=1,nsamp
         work(samp,2)=work(samp,3)
         if(data(samp,det,1).ne.blank) then
            work(samp,3)=scale*data(samp,det,1)+zero
         else
            work(samp,3)=rinval
         endif
      enddo

*
* CALCULATE THE EXPECTED SIGMA OF THE SMOOTHED DATA
*
         xsigma=sigma/sqrt(real(2*nhalf+1))

*
* CONVERT THE twice SMOOTHED DATA INTO WEIGHTS IN WORK2
*
         do samp=1,nsamp
            if(work(samp,2).ne.rinval) then
               work(samp,2)=1.0/max(work(samp,2)/(power*xsigma),1.0)
            else
               work(samp,2)=0.0
            endif
         enddo

      write(*,*) 'WORK1: differences,  WORK2: weights'
      call seeit(work,nsamp,ndet,rinval,blank,scale,zero,data,det,
     :           nhalf,nsigma,niter,inter,clplim,power,ierr)
      if(ierr.ne.0) goto 11

*
* RECALCULATE THE SIGMA OF THE ORIGINAL DIFFERENCES USING THESE WEIGHTS
*
         call sclip(work(1,1),work(1,2),nsamp,nsigma,niter,rinval,sigma,
     :              ierr)
         if(ierr.ne.0) goto 30
         write(*,*) 'Revised sigma: ',sigma

*
* MULTIPLY THE DIFFERENCES BY THE ABOVE WEIGHTS. THEN SET THE WEIGHTS 
* BACK TO UNITY.
*
         do samp=1,nsamp
            if(work(samp,1).ne.rinval) then
               work(samp,1)=work(samp,1)*work(samp,2)
               work(samp,2)=1.0
            else
               work(samp,2)=0.0
            endif
         enddo

*
* CALCULATE THE SIGMA OF THESE PRODUCTS
*
         call sclip(work(1,1),work(1,2),nsamp,nsigma,niter,rinval,sigma,
     :              ierr)
         if(ierr.ne.0) goto 30
         write(*,*) 'Weighted difference sigma: ',sigma

*
* REJECT POINTS FROM THE ORIGINAL DATA WHICH EXCEED SOME VALUE IN THESE
* DIVIDED DIFFERENCES
*
         do samp=1,nsamp
            if(work(samp,1).ne.rinval) then
               if(abs(work(samp,1)).gt.nsigma*sigma) then
                  write(*,*) 'Sample ',samp,' rejected (@',
     :                        abs(work(samp,1))/sigma,' sigma)'
                  if(samp.gt.1) data(samp-1,det,1)=blank
                  data(samp,det,1)=blank
                  if(samp.lt.nsamp) data(samp+1,det,1)=blank
               endif
            endif
         enddo

      write(*,*) 'WORK1: weighted differences'
      call seeit(work,nsamp,ndet,rinval,blank,scale,zero,data,det,
     :           nhalf,nsigma,niter,inter,clplim,power,ierr)
      if(ierr.ne.0) goto 11
*
* DO THE NEXT DETECTOR
*
  30  continue

      enddo

*
* FINISH
*
  999 continue

      call sgs_close

      end

C-----------------------------------------------
      subroutine seeit(work,nsamp,ndet,rinval,blank,scale,zero,data,det,
     :                 nhalf,nsigma,niter,inter,clplim,power,ierr)
      implicit none

      integer	icmd,nsamp,ndet,samp,blank,data(nsamp,ndet,3),det,ierr,
     :          nhalf,niter,inter
      real	work(nsamp,3),rinval,scale,zero,nsigma,clplim,power,
     :          temp(1000)
      character	title*44

      if(inter.ne.1) return

      call snx_aglab('T',' ')

  10  write(*,*) ' '
      write(*,*) title
      write(*,*) ' '
      write(*,*) ' 0 - Continue'
      write(*,*) ' 1 - Plot work1'
      write(*,*) ' 2 - Plot current data'
      write(*,*) ' 3 - Plot work2'
      write(*,*) ' 4 - Change box size'
      write(*,*) ' 5 - Change nsigma'
      write(*,*) ' 6 - Change niter '
      write(*,*) ' 7 - Change clplim '
      write(*,*) ' 8 - Change power '
      write(*,*) ' 9 - Repeat detector'
      write(*,*) '10 - Plot original data'
      write(*,'(A)') '$Enter option: '
      read(*,*) icmd
      call frame

      if(icmd.eq.0) then
         ierr=0

      else if(icmd.eq.1) then
         call ezy(work(1,1),nsamp,char(0))
         call flush
         call sgs_flush
         goto 10

      else if(icmd.eq.2) then
         do samp=1,min(nsamp,1000)
            if(data(samp,det,1).ne.blank) then
               temp(samp)=scale*data(samp,det,1)+zero 
            else
               temp(samp)=rinval
            endif
         enddo
         call ezy(temp,min(nsamp,1000),char(0))
         call flush
         call sgs_flush
         goto 10

      else if(icmd.eq.3) then

         call ezy(work(1,2),nsamp,char(0))
         call flush
         call sgs_flush
         goto 10

      else if(icmd.eq.4) then
         write(*,'(A)') '$Enter box half size: '
         read(*,*) nhalf
         goto 10

      else if(icmd.eq.5) then
         write(*,'(A)') '$Enter nsigma: '
         read(*,*) nsigma
         goto 10

      else if(icmd.eq.6) then
         write(*,'(A)') '$Enter niter: '
         read(*,*) niter
         goto 10

      else if(icmd.eq.7) then
         write(*,'(A)') '$Enter clplim: '
         read(*,*) clplim
         goto 10

      else if(icmd.eq.8) then
         write(*,'(A)') '$Enter power: '
         read(*,*) power
         goto 10

      else if(icmd.eq.9) then
         do samp=1,nsamp
            if(work(samp,3).ne.rinval) then
               data(samp,det,1)=nint((work(samp,3)-zero)/scale)
            else
               data(samp,det,1)=blank
            endif
         enddo
         ierr=1

      else if(icmd.eq.10) then
         call ezy(work(1,3),nsamp,char(0))
         call flush
         call sgs_flush
         goto 10

      endif

      call frame

      end


C-----------------------------------------------------------------------
      subroutine sclip(data,weight,nsamp,nsigma,niter,rinval,sigma,ierr)
      implicit none

      integer	nsamp,niter,ierr
      real	data(nsamp),weight(nsamp),nsigma,sigma,rinval

      integer	iter,samp
      real	sumsq,sumw


      ierr=0

      do iter=1,niter

         sumsq=0
         sumw=0
         do samp=1,nsamp
            if(data(samp).ne.rinval) then
               if(iter.eq.1.or.abs(data(samp)).le.nsigma*sigma) then
                  sumsq=sumsq+weight(samp)*data(samp)**2
                  sumw=sumw+weight(samp)
               endif
            endif
         enddo

         if(sumw.ne.0) then
            sigma=sqrt(sumsq/sumw)
         else
            call wruser('*** SCLIP: All data rejected',ierr)
            ierr=1
            goto 999
         endif

      enddo

*
* FINISH
*
  999 continue

      end



C---------------------------------------------------------------
      subroutine smooth(in,out,nsamp,rinval,nhalf)
      implicit none

      integer	nsamp,nhalf
      real	in(nsamp),out(nsamp),rinval

      integer	samp,nval,index
      real	sum


      do samp=1,nsamp
         
            sum=0.0
            nval=0
            do index=max(1,samp-nhalf),min(nsamp,samp+nhalf)
               if(in(index).ne.rinval) then
                  sum=sum+in(index)
                  nval=nval+1
               endif
            enddo

            if(nval.gt.0) then
               out(samp)=sum/nval
            else
               out(samp)=rinval
            endif

      enddo

*
* FINISH
*
      end

