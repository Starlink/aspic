      subroutine filtsp
 
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO CALCULATE THE STATISTICS OF COMBINATIONS OF OPTICAL FILTERS.
*       THE INDIVIDUAL FILTER DATA IS READ IN FROM TEXT FILES.
*
*METHOD
*       THE DATA FOR EACH FILTER IS OBTAINED FROM THE TEXT FILE WHOSE
*       NAME IS SUPPLIED. THE TRANSMISSION DATA IN THE FILE IS
*       CONVERTED TO DENSITY FOR SUBSEQUENT INTERPOLATION. THE
*       STATISTICS OF THE FILTER COMBINATION OBTAINED BY MULTIPLYING
*       ALL THE SEPARATE INTERPOLATED FUNCTIONS TOGETHER ARE THEN
*       CALCULATED, INCLUDING, IF NECESSARY, A STANDARD CONTINUUM
*       SOURCE SPECTRUM.
*
*ARGUMENTS
*       NONE
*
*CALLS
*       THIS PACKAGE:
*               GETPAR,GETCMD,LBGONE,GETFIL,DEVOPT,FLTSTS,LENS
*       STARLINK INTERIM LIBRARY:
*               RDKEYC,WRERR,CNPAR,CTOR,WRUSER,WRKEYR
*       VAX RUN TIME LIBRARY:
*               LIB$DO_COMMAND
*
*STARLINK PARAMETERS USED
*       ILEVEL
*               INTERACTION LEVEL
*       SOURCE
*               TYPE OF SOURCE CONTINUUM SPECTRUM
*       TEMP
*               TEMPERATURE OF BLACK BODY SPECTRUM (IF USED)
*       INDEX
*               INDEX OF POWER LAW SPECTRUM (IF USED)
*       FILTER1...FILTER10
*               NAMES OF FILTER RESPONSE DATA FILES (AND OPTIONAL
*               REPEAT FACTORS)
*       NONULL (ERROR)
*               ACCESSED IF A NULL IS GIVEN FOR THE FIRST FILTER
*               DATA FILE NAME
*       BADNUMF (ERROR)
*               ACCESSED IF THE FILTER REPEAT FACTOR CANNOT BE
*               INTERPRETED AS A NUMBER
*       BADRANGE (ERROR)
*               ACCESSED IF THE FILTERS DO NOT OVERLAP IN THEIR
*               SPECTRAL RANGES
*       NOFILE (ERROR)
*               ACCESSED IF THE FILTER DATA FILE CANNOT BE OPENED
*       READERR (ERROR)
*               ACCESSED IF THERE IS A READING (FORMAT) ERROR IN
*               THE FILTER DATA FILE
*       BADORDER (ERROR)
*               ACCESSED IF THE FILTER DATA IS NOT IN WAVELENGTH
*               ORDER
*       NEGTRANS (ERROR)
*               ACCESSED IF THE FILTER DATA CONTAINS A NON-POSITIVE
*               TRANSMISSION FACTOR
*       2FEWPTS (ERROR)
*               ACCESSED IF THERE ARE LESS THAN 2 FILTER DATA POINTS
*               IN A FILE
*       XSDATA (ERROR)
*               ACCESSED IF THERE ARE MORE THAN 100 FILTER DATA POINTS
*               IN A FILE
*       DEVICE
*               TYPE OF PLOTTING DEVICE
*       PLOTFILE
*               NAME OF PLOT FILE FOR STORING QUEUED OUTPUT
*       NOPLOTF (ERROR)
*               ACCESSED IF PLOTFILE CANNOT BE OPENED
*       TOTINT
*               TOTAL TRANSAMITTED INTENSITY
*       TPHOT
*               TOTAL TRANSMITTED PHOTON NUMBER/SECOND
*       WMEAN
*               MEAN WAVELENGTH
*       WPEAK
*               PEAK WAVELENGTH
*       IPEAK
*               PEAK TRANSMITTED INTENSITY
*       BWIDTH
*               EFFECTIVE BANDWIDTH
*
*NOTES
*       USES VAX TEXT FILES
*       USES COMMON BLOCKS /FLTBL1/ AND /FLTBL2/ TO STORE FILTER
*       RESPONSES AND PARAMETERS FOR THE SOURCE SPECTRUM. THESE
*       BLOCKS ARE ALSO USED BY FLTFUN AND FLTSTS.
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*----------------------------------------------------------------------
*
*
* INCLUDE COMMON BLOCK TO HOLD INTERPOLATION DATA FOR FILTERS
*
      include 'edrslib(filt1com.inc)'
 
*
* INCLUDE COMMON BLOCK TO HOLD INFORMATION ABOUT THE TYPE OF SOURCE
* SPECTRUM, ETC.
*
      include 'edrslib(filt2com.inc)'
 
*
* DECLARATIONS, ETC.
*
      character txt*8,fname(2,maxfil)*30,opts*200,device*20,source*30
     : ,pfname(1)*30,pr*20
      real r(1)
 
*
* OBTAIN INTERACTION LEVEL
*
      ilevel=2
      call getpar('ILEVEL','INTEGER',1,1.0,3.0,.true.,ilevel,rval,ierr)
 
*
* OBTAIN THE TYPE OF SOURCE SPECTRUM REQUIRED
*
      isourc=1
      call getcmd('SOURCE','UNIFORM,BLACK_BODY,POWER_LAW.',1,isourc
     : ,source,lsrc,ierr)
 
*
* OBTAIN THE SOURCE CONSTANT IF NEEDED
*
 
      if(source.eq.'BLACK_BODY')then
         sconst=9900.0
         call getpar('TEMP','REAL',1,1.0,1.0e20,.true.,ival,sconst,
     :   ierr)
 
      else if(source.eq.'POWER_LAW')then
         sconst=0.0
         call getpar('INDEX','REAL',1,-10.0,+10.0,.true.,ival,sconst
     :    ,ierr)
      endif
 
 
*
* OBTAIN THE NAME OF EACH FILTER FILE IN TURN
*
      wmin=-1.0e20
      wmax=1.0e20
 
      do 1 i=1,maxfil
         write(txt,2)i
2        format('FILTER',i2)
         call lbgone(txt(7:))
         nbad=0
4        fname(1,i)=' '
         fname(2,i)='1.0'
         call rdkeyc(txt,.false.,2,fname(1,i),nval,istat)
 
*
* PERMIT A NULL ENTRY ONLY IF IT IS NOT THE FIRST FILTER RESPONSE
*
 
         if(istat.ne.0.or.fname(1,i).eq.' ')then
 
            if(i.eq.1)then
               nbad=nbad+1
               call wrerr('NONULL')
               call cnpar(txt,istat)
 
*
* ABORT IF TOO MANY ERRORS HAVE OCCURRED
*
 
               if(nbad.ge.3)go to 99
               go to 4
 
 
            else
               go to 3
 
            endif
 
         endif
 
 
*
* APPLY DEFAULT ELEMENT THICKNESS IF REQUIRED, THEN CONVERT TO A NUMBER
* CHECKING FOR ERRORS
*
 
         if(fname(2,i).eq.' ')fname(2,i)='1.0'
         call ctor(fname(2,i),elemts(i),ierr)
 
         if(ierr.ne.0)then
            nbad=nbad+1
            call wrerr('BADNUMF')
            call cnpar(txt,istat)
 
            if(nbad.ge.3)go to 99
            go to 4
 
         endif
 
 
*
* READ AND CHECK THE DATA
*
         call getfil(fname(1,i),wavel(1,i),dens(1,i),maxpts,npts(i)
     :    ,ierr)
 
*
* IF SUCCESSFUL, KEEP TRACK OF THE SPECTRAL RANGE COVERED
*
 
         if(ierr.eq.0)then
            wmin=max(wmin,wavel(1,i))
            wmax=min(wmax,wavel(npts(i),i))
 
*
* IF THE FILTERS DO NOT OVERLAP IN THEIR KNOWN SPECTRAL RANGES,
* GIVE ERROR MESSAGE AND ABORT
*
 
            if(wmin.ge.wmax)then
               call wrerr('BADRANGE')
               go to 99
 
            endif
 
 
*
* IF THE FILE COULD NOT BE OPENED, GIVE ERROR MESSAGE AND TRY AGAIN
*
 
         else if(ierr.eq.1)then
            nbad=nbad+1
            call wrerr('NOFILE')
            call cnpar(txt,istat)
 
*
* ABORT IF TOO MANY ERRORS HAVE OCCURRED
*
 
            if(nbad.ge.3)go to 99
            go to 4
 
 
*
* IF A FATAL ERROR OCCURRED, GIVE ERROR MESSAGE AND ABORT
*
 
         else
 
            if(ierr.eq.2) call wrerr('READERR')
 
            if(ierr.eq.3) call wrerr('BADORDER')
 
            if(ierr.eq.4) call wrerr('NEGTRANS')
 
            if(ierr.eq.5) call wrerr('2FEWPTS')
 
            if(ierr.eq.6) call wrerr('XSDATA')
            go to 99
 
         endif
 
1     continue
 
3     i=i-1
      nfil=i
 
*
* OBTAIN THE REQUIRED PLOTTING DEVICE
*
      call devopt(opts)
      idev=2
      call getcmd('DEVICE','NONE,'//opts,1,idev,device,ldev,ierr)
      idev=idev-1
 
*
* IF REQUIRED, OPEN AN OUTPUT FILE TO CONTAIN PLOT OUTPUT FOR QUEUEING
*
 
      if(device.eq.'PRINTRONIX'.or.device.eq.'VERSATEC')then
         nbad=0
77       pfname(1)='FILTSPEC.DAT'
         call rdkeyc('PLOTFILE',.true.,1,pfname,nval,istat)
         open(0,file=pfname(1),status='NEW',iostat=ioperr)
 
*
* IF THE FILE CANNOT BE OPENED, GIVE ERROR MESSAGE AND TRY AGAIN
*
 
         if(ioperr.ne.0)then
            nbad=nbad+1
            call wrerr('NOPLOTF')
            call cnpar('PLOTFILE',istat)
 
            if(nbad.ge.3)go to 99
            go to 77
 
         endif
 
      endif
 
 
*
* CALCULATE REQUIRED STATISTICS
*
      call fltsts(wmin,wmax,idev,0,totint,tphot,wmean,wpeak,tpeak,
     :bwidth)
 
*
* IF REQUIRED, PRINT RESULTS
*
 
      if(ilevel.ge.2)then
         call wruser(' ',istat)
         call wruser('   SOURCE SPECTRUM',istat)
         call wruser('   ------ --------',istat)
         call wruser(' ',istat)
 
         if(source.eq.'UNIFORM')then
            call wruser('      Uniform energy distribution (1 W/nm)'
     :      ,istat)
 
         else if(source.eq.'BLACK_BODY')then
            write(pr,12)sconst
12          format(g13.6)
            call lbgone(pr)
            call wruser('      Unit area of black body at '//
     :      pr(1:lens(pr))//' K',istat)
 
         else if(source.eq.'POWER_LAW')then
            write(pr,13)sconst
13          format(g11.4)
            call lbgone(pr)
            call wruser('      Power law with spectral index '//pr,
     :      istat)
            call wruser('      Normalized to 1 W/nm at 550 nm waveleng'/
     :       / 'th.',istat)
         endif
 
         call wruser(' ',istat)
         call wruser(' ',istat)
         call wruser('   FILTER SPECIFICATION',istat)
         call wruser('   ------ -------------',istat)
         call wruser(' ',istat)
         call wruser('      Filter type                Repeat factor'
     :   ,istat)
         call wruser('      ------ ----                ------ ------'
     :   ,istat)
 
         do 55 j=1,i
            call wruser('      '//fname(1,j)//'   '//fname(2,j),istat)
55       continue
 
         call wruser(' ',istat)
         call wruser(' ',istat)
         call wruser('   STATISTICS OF EMERGENT SPECTRUM',istat)
         call wruser('   ---------- -- -------- --------',istat)
         call wruser(' ',istat)
         write(pr,12)totint
         call lbgone(pr)
         call wruser('      Total emergent intensity= '//pr(1:lens(pr))
     :    //' W',istat)
         call wruser(' ',istat)
         write(pr,12)tphot
         call lbgone(pr)
         call wruser('      Emergent photon rate= '//pr(1:lens(pr))//
     :   ' /s',istat)
         call wruser(' ',istat)
         write(pr,12)wmean
         call lbgone(pr)
         call wruser('      Mean wavelength= '//pr(1:lens(pr))//' nm'
     :   ,istat)
         call wruser(' ',istat)
         write(pr,12)wpeak
         call lbgone(pr)
         call wruser('      Peak wavelength= '//pr(1:lens(pr))//' nm'
     :   ,istat)
         call wruser(' ',istat)
         write(pr,12)tpeak
         call lbgone(pr)
         call wruser('      Peak intensity= '//pr(1:lens(pr))//' W/nm'
     :   ,istat)
         call wruser(' ',istat)
         write(pr,12)bwidth
         call lbgone(pr)
         call wruser('      Effective bandwidth= '//pr(1:lens(pr))//
     :   ' nm',istat)
         call wruser(' ',istat)
      endif
 
 
*
* WRITE RESULTS TO ENVIRONMENT
*
      r(1)=totint
      call wrkeyr('TOTINT',r,1,istat)
      r(1)=tphot
      call wrkeyr('TPHOT',r,1,istat)
      r(1)=wmean
      call wrkeyr('WMEAN',r,1,istat)
      r(1)=wpeak
      call wrkeyr('WPEAK',r,1,istat)
      r(1)=tpeak
      call wrkeyr('IPEAK',r,1,istat)
      r(1)=bwidth
      call wrkeyr('BWIDTH',r,1,istat)
 
*
* SEND PLOTTED OUTPUT TO THE PRINT QUEUE IF REQUIRED
*
 
      if(device.eq.'PRINTRONIX')then
         istat=lib$do_command('$PRINT/QUEUE=QAB0 '//pfname(1))
      endif
 
 
   99 end
 
 
 
