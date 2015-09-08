      subroutine aodest
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*SOURCE
*	AODEST.FOR in AODESTRIPE.TLB
*----------------------------------------------------------
      implicit none

      include 'UTILITIES(IR_PAR)'
      include 'UTILITIES(DT_DAT)'
      include 'UTILITIES(AO_COM)'

      integer	band,frmid,ipin,ierr,ipout,imeth,ival,ipwork
      real	pcval,scale,zero,varn(IR_dts)
      character method*7

*
* GET CRDD FILE CONTAINING AO DATA IN FOOTPRINTS FORMAT
*
      call gtfoot('INPUT',.true.,band,frmid,ipin,ierr)
      if(ierr.ne.0) goto 999

*
* GET OUTPUT BDF TO HOLD DESTRIPED DATA
*
      call gt3diw('OUTPUT',104,.false.,AO_nys(frmid),AO_nde(frmid),3,
     :             ipout,ierr)
      if(ierr.ne.0) goto 999

*
* SEE WHAT METHOD IS TO BE USED FOR THE DESTRIPING
*
      imeth=1
      call gtstrn('METHOD',.true.,'MEDIAN,MODE,GENERAL.',1,imeth,method,
     :             ival,ierr)

*
* IF EQUALISATION OF A GENERAL HISTOGRAM POINT IS BEING USED, GET THE
* THE HISTOGRAM POINT TO USE (DEFAULT IS 50% I.E. THE MEDIAN)
*
      if(method.eq.'GENERAL') then
         pcval=50.0
         call getpar('PCPOINT','REAL',1,0.0,100.0,.true.,ival,pcval,
     :                ierr)
      endif

*
* COPY THE INPUT DATA TO THE OUTPUT DATA TO PRESERVE THE X AND Y
* DATA
*
      call aocopy(%val(ipin),%val(ipout),AO_nys(frmid)*AO_nde(frmid)*
     :            3)

*
* CALL DESTRI TO DO THE DESTRIPING
*
      call destri(%val(ipout),AO_nys(frmid),AO_nde(frmid),AO_bpx(frmid),
     :            method,pcval,ierr)

*
* COPY THE INPUT DESCRIPTORS TO THE OUTPUT
*
      call cydscr('INPUT','OUTPUT',ierr)

*
* FINISH
*
  999 call frdata(' ',ierr)

      end

C------------------------------------------------------------------
      subroutine aocopy(in,out,size)
      implicit none
      integer	size,in(size),out(size),index

      do index=1,size
         out(index)=in(index)
      enddo
 
      end

C-------------------------------------------------------------------
      subroutine destri(out,nys,nde,blank,method,pcval,ierr)
      implicit  none
      integer	nys,nde,out(nys,nde),blank,ierr,det,intmax,
     :          intmin,samp,iphist,ans,istat,ipwx,ipwy
      real	pcval
      character method*(*)

      call gtwork('WORKX','REAL',nys,ipwx,ierr)
      call gtwork('WORKY','REAL',nys,ipwy,ierr)

*
* LOOP ROUND EACH DETECTOR, ADDING A CONSTANT VALUE TO EACH TO SET
* THE SELECTED STATISTIC OF THE DETECTOR DATA VALUES TO ZERO
*
      ierr=1
      do det=1,nde

*
* FIND THE MIN AND MAX DATA VALUE
*
         intmax=-2000000000
         intmin=2000000000
         do samp=1,nys
            if(out(samp,det).ne.blank) then
               intmax=max(intmax,out(samp,det))
               intmin=min(intmin,out(samp,det))
            endif
         enddo

         if(intmax.eq.-2000000000) then
            call wruser('*** No valid data for at least 1 detector',
     :                   istat)
            goto 998
         endif
         
*
* OBTAIN STORAGE FOR A HISTOGRAM OF DATA VALUES PRODUCED BY THE CURRENT
* DETECTOR
*
         call gtwork('HIST','INTEGER',intmax-intmin+1,iphist,istat)
         if(istat.ne.0) goto 998

*
* IF THE CHOSEN STATISTIC IS A GIVEN PERCENTAGE POINT OF THE CUMULATIVE
* HISTOGRAM, CALL THE PCHIST ROUTINE, WITH THE PERCENTAGE VALUE GIVEN 
* BY THE USER
*
         if(method.eq.'GENERAL') then
            call pchist(out(1,det),nys,blank,intmax,intmin,
     :                     %val(iphist),intmax-intmin+1,pcval,ans)

*
* IF MEDIAN STATISTIC WAS CHOSEN, CALL PCHIST WITH PCVAL=50 %
*
         else if(method.eq.'MEDIAN') then
            call pchist(out(1,det),nys,blank,intmax,intmin,
     :                     %val(iphist),intmax-intmin+1,50.0,ans)

*
* IF MODE STATISTIC WAS SELECTED, CALL MODE TO CALCULATE IT
*
         else
            call mode(out(1,det),nys,blank,intmax,intmin,
     :                     %val(iphist),intmax-intmin+1,ans)
         endif

*
* RELEASE THE STORAGE USED FOR THE HISTOGRAM
*
         call frdata('HIST',istat)

*
* SUBTRACT THE STATISTIC VALUE FROM THE DATA FOR THE CURRENT DETECTOR
*
         do samp=1,nys
            if(out(samp,det).ne.blank) out(samp,det)=out(samp,det)-ans
         enddo

*
* GO ROUND FOR NEXT DETECTOR (INDICATE SUCCESS FOR AT LEAST ONE 
* DETECTOR)
*
         ierr=0
  998    continue

      enddo

*
* FINISH
*
  999 continue

      end
C---------------------------------------------------------------------
      subroutine pchist(in,nys,blank,intmax,intmin,hist,size,pcval,ans)
      implicit  none
      integer	nys,in(nys),blank,intmax,intmin,size,ans,
     :          bin,samp,cumul,target,nval,hist(size)
      real	pcval

      do bin=1,size
         hist(bin)=0
      enddo

      nval=0
      do samp=1,nys
         if(in(samp).ne.blank) then
            bin=in(samp)-intmin+1      
            hist(bin)=hist(bin)+1
            nval=nval+1
         endif
      enddo

      cumul=0
      target=real(nval)*pcval/100.0
      ans=0
      do while(cumul.lt.target)
         ans=ans+1
         cumul=cumul+hist(ans)
      enddo
      ans=ans+intmin-1

      end
C---------------------------------------------------------------
      subroutine mode(in,nys,blank,intmax,intmin,hist,size,ans)
      implicit  none
      integer	nys,in(nys),blank,intmax,intmin,size,ans,peak,
     :          bin,samp,cumul,nval,hw,blo,bhi,b,hist(size),maxpop
      real	target,maxden,densit,sum
      logical	more

      do bin=1,size
         hist(bin)=0
      enddo

      nval=0
      do samp=1,nys
         if(in(samp).ne.blank) then
            bin=in(samp)-intmin+1      
            hist(bin)=hist(bin)+1
            nval=nval+1
         endif
      enddo


      maxpop=0
      do bin=1,size
         if(hist(bin).gt.maxpop) then
            maxpop=hist(bin)
            peak=bin
         endif
      enddo

      hw=-1
      sum=0
      do while(sum.lt.0.2*nval)
         hw=hw+1
         if(peak-hw.ge.1) sum=sum+hist(peak-hw)
         if(peak+hw.le.size) sum=sum+hist(peak+hw)
      enddo

      maxden=0.0
      do bin=1,size
         blo=max(1,bin-hw)
         bhi=min(size,bin+hw)
         sum=0
         do b=blo,bhi
            sum=sum+hist(b)
         enddo
         densit=sum/(bhi-blo+1)
         if(densit.gt.maxden) then
            maxden=densit
            ans=bin
         endif
      enddo

      ans=ans+intmin-1

      end


*-----------------------------------------------------------
