      subroutine check(title)
*++++++++++++++++++++++++++++++++++++++
*SOURCE
*	CHECK.FOR in MEMCRDD.TLB
*--------------------------------------
      implicit none

      include '(PR_DEC)'
      include '(B2_COM)'

      character title*(*)
      real	maxdif
      integer	det0,crdd0,samp0,crddf

      maxdif=0
      do crddf=1,B2_ncf
         call chk1(%val(B2_pin(crddf)),B2_nys(crddf),B2_nde(crddf),
     :             crddf,maxdif,crdd0,det0,samp0)
      enddo

      write(*,*) ' '
      write(*,*) title
      write(*,*) '** Max difference: ',maxdif
      write(*,*) '   occured in samp,det,crddf: ',samp0,det0,crdd0

      end


      subroutine chk1(crdd,nsamp,ndet,crddf,maxdif,crdd0,det0,samp0)
      implicit none

      include '(PR_DEC)'
      include 'UTILITIES(IR_PAR)'
      include '(B2_COM)'
      include '(B5_COM)'
      include '(B6_COM)'
      include '(ME_COM)'

      integer	nsamp,ndet,crdd(nsamp,ndet),crddf,samp0,det0,crdd0,sdc,
     :          tcrddf,tsamp,tdet,offset
      real	maxdif,datval,diff


      do offset=0,ME_mk-1
         sdc=ME_sti(B6_sdc+offset)
         tcrddf=mod(sdc/16,B5_cfa)+1
         if(tcrddf.eq.crddf) then
            tdet=mod(sdc,16)+1
            tsamp=sdc/(16*B5_cfa)+1
            datval=crdd(tsamp,tdet)*B2_bsc(crddf)+B2_bze(crddf)
            diff=abs(datval-ME_st(ME_kb(21)+offset))

            if(diff.gt.maxdif) then
               maxdif=diff
               samp0=tsamp
               det0=tdet
               crdd0=crddf
            endif
         endif
      enddo

      end
