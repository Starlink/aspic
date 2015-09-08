      subroutine udiag(title)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Allows the user to interupt the MEM algorithm to inspect
*	or modify the contents of internal files.
*
*SOURCE
*       UDIAG.FOR in MEMCRDD.TLB
*
*ARGUMENTS       
*   INPUT:
*	title	chatacter	A string to distinguish between calls
*
*COMMON USAGE
*   READ:
*	/ME_COM/,/B6_COM/
*   WRITE:
*	/ME_COM/,
*		ME_l0	Internal MEMSYS3 diagnostics flag
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              wruser,gtstrn
*       THIS PACKAGE (MEMCRDD.TLB):
*              memca2,memcf3,opus,tropus,(gfile,ident,stats,
*	       exam; sources attached)
*       EDRS:
*              getpar
*       INTERIM:
*	       cnpar
*              
*STARLINK PARAMETERS
*	OPTION		No. identifying required function
*	INPUT		BDF image to read in.
*	OUTPUT		BDF name to dump file to
*	FILE		Specifies which internal file to process
*	GROUP		Sample group number
*	ELEMENT		Element no. within a file
*	SCALAR		Scalar value for file arithmetic
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 10/10/89
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

* ... FITS DESCRIPTORS FOR FINAL IMAGE
      include '(B0_COM)'

* ... POINTERS TO MEMCRDD FILES
      include '(B6_COM)'

* ... MEMSYS3 COMMON BLOCKS
      include '(ME_COM)'

*
* DECLARE ARGUMENTS
*
      character title*(*)

*
* DECLARE LOCAL VARIABLES
*
      character	bdfnam*40 !Name of input BDF image
      integer	file1	! File identifer
      integer	file2	! File identifer
      integer	file3	! File identifer
      integer	ierr	! Inherited error status
      integer	istat	! Temporary status value
      integer	ival	! Dummy integer argument
      integer	ltext	! No. of characters in selected GTSTRN option 
      integer	nopt	! GTSTRN option number
      integer	option	! Selected option number from the displayed menu
      real	rval	! Dummy real argument
      integer	offset	! Offset into the file
      character	prbuf*80! Buffer for screen output
      real	result	! Normalised error between OPUS and TROPUS
      real	scalar	! Scalar value for file arithmetic
      integer	size	! Size of the selected file (ME_mj or ME_mk)
      integer	start1	! Start address of file 1 within ME_st
      integer	start2	! Start address of file 2 within ME_st
      integer	start3	! Start address of file 3 within ME_st
      character text*10 ! Text form of selected GTSTRN option
      character	type*5	! File type; image or data set

*
* LOOP UNTIL USER DECIDES TO CONTINUE WITH THE MEM ALGORITHM
*
      call wruser(title,istat)

   10 continue

*
* DISPLAY MENU
*
      call wruser('  ',istat)
      call wruser('  1 - Copy one file to another',istat)
      call wruser('  2 - Apply OPUS to an image file',istat)
      call wruser('  3 - Apply TROPUS to a data set',istat)
      call wruser('  4 - Dump a file to disk in BDF format',istat)
      call wruser('  5 - Display statistics for a file',istat)
      call wruser('  6 - Examine the contents of a file',istat)
      call wruser('  7 - Identify data samples',istat)
      call wruser('  8 - Stop UDIAG being called for rest of this '//
     :            'iteration',istat)
      call wruser('  9 - Call OPUS/TROPUS checker',istat)
      call wruser(' 10 - 4 function arithmetic on internal files',istat)
      call wruser(' 11 - Scalar arithmetic on internal files',istat)
      call wruser(' 12 - Calculate detector statistics',istat)
      call wruser(' 13 - Read in an EDRS image',istat)
      call wruser(' 14 - Take FFT of a real image',istat)
      call wruser(' 15 - Take inverse FFT of a Hermitian transform',
     :            istat)
      call wruser(' 16 - Multiply two hermitian transforms',istat)
      call wruser(' 17 - Take reciprocal of a REAL hermitian transform',
     :            istat)
      call wruser(' 18 - Continue with MEM algorithm',istat)
      call wruser('  ',istat)

      option=18
      call getpar('OPTION','INTEGER',1,1.0,18.0,.true.,option,rval,
     :             istat)
      call cnpar('OPTION',istat)

*
* PERFORM REQUIRED TASK
*
      ierr=0

*
* COPY ONE FILE TO ANOTHER
*
      if(option.eq.1) then

         type='EITHER'
         call gfile('Give source file',type,file1,start1,size,ierr)
         call gfile('Give destination file',type,file2,start2,size,ierr)

         if(ierr.eq.0) then
            do offset=0,size-1
               ME_st(start2+offset)=ME_st(start1+offset)
            enddo
         endif

         goto 10

*
* APPLY OPUS TO AN IMAGE FILE
*
      else if(option.eq.2) then

         type='IMAGE'
         call gfile('Give source image',type,file1,start1,size,ierr)

         type='DATA'
         call gfile('Give destination data set',type,file2,start2,size,
     :              ierr)

         if(ierr.eq.0) then
            if(file1.gt.0.and.file2.gt.0) then
               call opus(file1,file2)
            else
               call wruser('*** OPUS can only use MEM files',istat)
            endif
         endif
         goto 10

*
* APPLY TROPUS TO A DATA SET
*
      else if(option.eq.3) then

         type='DATA'
         call gfile('Give source data set',type,file1,start1,size,ierr)

         type='IMAGE'
         call gfile('Give destination image',type,file2,start2,size,
     :               ierr)

         if(ierr.eq.0) then
            if(file1.gt.0.and.file2.gt.0) then
               call tropus(file1,file2)
            else
               call wruser('*** TROPUS can only use MEM files',istat)
            endif
         endif
         goto 10

*
* DUMP A FILE TO DISK IN BDF FORMAT
*
      else if(option.eq.4) then
         type='EITHER'
         call gfile('Give file to dump',type,file1,start1,size,ierr)
         if(ierr.eq.0)then
            if(file1.gt.0) then
               call memca2('OUTPUT',file1,.true.,istat)
            else
               call wruser('** Can only dump MEM files',istat)
            endif
         endif
         goto 10

*
* DISPLAY STATISTICS FOR A FILE
*
      else if(option.eq.5) then
         type='EITHER'
         call gfile('Give file',type,file1,start1,size,ierr)
         if(ierr.eq.0) call stats(ME_st(start1),size,start1)
         goto 10

*
* EXAMINE THE CONTENTS OF A FILE
*
      else if(option.eq.6) then
         type='EITHER'
         call gfile('Give file to examine',type,file1,start1,size,ierr)
         if(ierr.eq.0) call exam(ME_st(start1),size)
         goto 10

*
* IDENTIFY DATA SAMPLES
*
      else if(option.eq.7) then
         call ident(ME_sti(B6_sdc),ME_mk)
         goto 10

*
* STOP UDIAG BEING CALLED FOR REST OF THIS ITERATION
*
      else if(option.eq.8) then
         ME_l0=0
         goto 10
      
*
* PERFORM CHECK ON OPUS/TROPUS CONSISTENCY USING CURRENT CONTENTS OF
* FILES 1 AND 26
*
      else if(option.eq.9) then
         call memtrq(0,result)
         write(prbuf,20) result
  20     format('  Normalised error: ',G13.6)
         call wruser(prbuf,istat)
         goto 10

*
* PERFORM 4 FUNCTION ARITHMETIC BETWEEN 2 INTERNAL FILES
*
      else if(option.eq.10) then
         call cnpar('FUNCTION',istat)
         nopt=1
         call gtstrn('FUNCTION',.true.,'+,-,/,*,ADD,SUBTRACT,DIVIDE,'//
     :               'MULTIPLY.',1,nopt,text,ltext,istat)
         nopt=mod(nopt-1,4)+1

         type='EITHER'
         call gfile('Give first input file',type,file1,start1,size,ierr)
         call gfile('Give second input file',type,file2,start2,size,
     :               ierr)
         call gfile('Give output file',type,file3,start3,size,ierr)

         if(ierr.eq.0) then

            if(nopt.eq.1) then
               do offset=0,size-1
                  ME_st(start3+offset)=ME_st(start1+offset)+
     :                                 ME_st(start2+offset)
               enddo

            else if(nopt.eq.2) then
               do offset=0,size-1
                  ME_st(start3+offset)=ME_st(start1+offset)-
     :                                 ME_st(start2+offset)
               enddo

            else if(nopt.eq.3) then
               do offset=0,size-1
                  ME_st(start3+offset)=ME_st(start1+offset)/
     :                                 ME_st(start2+offset)
               enddo

            else if(nopt.eq.4) then
               do offset=0,size-1
                  ME_st(start3+offset)=ME_st(start1+offset)*
     :                                 ME_st(start2+offset)
               enddo

            endif
            
         endif

         goto 10

*
* PERFORM SCALAR 4 FUNCTION ARITHMETIC ON INTERNAL FILES
*
      else if(option.eq.11) then
         call cnpar('FUNCTION',istat)
         nopt=1
         call gtstrn('FUNCTION',.true.,'+,-,/,*,ADD,SUBTRACT,DIVIDE,'//
     :               'MULTIPLY,MAX,MIN,RECIPROCATE.',1,nopt,text,ltext,
     :               istat)
         if(nopt.le.8) nopt=mod(nopt-1,4)+1

         call cnpar('SCALAR',istat)
         scalar=0.0
         if(nopt.eq.11) scalar=1.0
         call getpar('SCALAR','REAL',1,-1.0E30,1.0E30,.true.,ival,
     :                scalar,istat)

         type='EITHER'
         call gfile('Give input file',type,file1,start1,size,ierr)
         call gfile('Give output file',type,file3,start3,size,ierr)

         if(ierr.eq.0) then

            if(nopt.eq.1) then
               do offset=0,size-1
                  ME_st(start3+offset)=ME_st(start1+offset)+scalar
               enddo

            else if(nopt.eq.2) then
               do offset=0,size-1
                  ME_st(start3+offset)=ME_st(start1+offset)-scalar
               enddo

            else if(nopt.eq.3) then
               if(scalar.ne.0) then
                  do offset=0,size-1
                     ME_st(start3+offset)=ME_st(start1+offset)/scalar
                  enddo
               else
                  call wruser('*** Cannot divide by zero!',istat)
               endif

            else if(nopt.eq.4) then
               do offset=0,size-1
                  ME_st(start3+offset)=ME_st(start1+offset)*scalar
               enddo

            else if(nopt.eq.9) then
               do offset=0,size-1
                  ME_st(start3+offset)=max(ME_st(start1+offset),scalar)
               enddo


            else if(nopt.eq.10) then
               do offset=0,size-1
                  ME_st(start3+offset)=min(ME_st(start1+offset),scalar)
               enddo

            else if(nopt.eq.11) then
               do offset=0,size-1
                  ME_st(start3+offset)=scalar/ME_st(start1+offset)
               enddo

            endif
            
         endif

         goto 10

*
* FIND DETECTOR STATISTICS
*
      else if(option.eq.12) then
         type='DATA'
         call gfile('Give input data set',type,file1,start1,size,ierr)

         if(ierr.eq.0) call detsta(ME_st(start1),size)
         goto 10


*
* READ IN A 2D IMAGE
*
      else if(option.eq.13) then
         type='IMAGE'
         call gfile('Give file in which to store the image',type,file1,
     :               start1,size,ierr)

         if(ierr.eq.0) call memcf3('INPUT',.true.,ME_st(start1),
     :                              0.0,bdfnam,ierr)
         call cnpar('INPUT',istat)
         goto 10

*
* TAKE FFT OF AN IMAGE
*
      else if(option.eq.14) then
         type='IMAGE'
         call gfile('Give file to transform',type,file1,start1,size,
     :               ierr)
         call gfile('Give file to hold transform',type,file2,start2,
     :               size,ierr)

         if(ierr.eq.0) call memcc6(.false.,ME_st(start1),ME_st(start2),
     :                             ME_st(B6_wk1),B0_nps,B0_nls,ierr)
         goto 10

*
* TAKE INVERSE FFT OF AN HERMITIAN TRANSFORM
*
      else if(option.eq.15) then
         type='IMAGE'
         call gfile('Give file to inverse transform',type,file1,start1,
     :              size,ierr)
         call gfile('Give file to hold inverse transform',type,file2,
     :              start2,size,ierr)

         if(ierr.eq.0) call memcc7(.true.,ME_st(start1),ME_st(start2),
     :                             ME_st(B6_wk1),B0_nps,B0_nls,ierr)
         goto 10

*
* MULTIPLY TWO FFTS
*
      else if(option.eq.16) then
         type='IMAGE'
         call gfile('Give file holding first transform',type,file1,
     :               start1,size,ierr)
         call gfile('Give file holding second transform',type,file2,
     :               start2,size,ierr)
         call gfile('Give file holding product',type,file3,start3,size,
     :               ierr)

         if(ierr.eq.0) call memcd0(ME_st(start1),ME_st(start2),
     :                             ME_st(start3),B0_nps,B0_nls)
         goto 10

*
* TAKE RECIPROCAL OF A PURELY REAL HERMITIAN FFT
*
      else if(option.eq.17) then
         type='IMAGE'
         call gfile('Give file holding transform',type,file1,
     :               start1,size,ierr)
         call gfile('Give file to hold reciprocal transform',type,file2,
     :               start2,size,ierr)

         if(ierr.eq.0) call memcg5(ME_st(start1),ME_st(start2),B0_nps,
     :                             B0_nls,0.0)
         goto 10

      endif

*
* FINISH
*
  999 continue

      end


C-----------------------------------------------------------------
      subroutine gfile(prompt,type,file,start,size,ierr)
      implicit none

      include '(PR_DEC)'
      include 'UTILITIES(IR_PAR)'
      include '(A8_COM)'
      include '(B6_COM)'
      include '(ME_COM)'

      integer	ierr,istat,file,start,nopt,ltext,group,size
      character prompt*(*),text*10,type*(*),list*36
      real	rval,flo,fhi


      if(ierr.ne.0) goto 999


      if(type.eq.'IMAGE') then
         flo=1
         fhi=20
         list='MEM_FILE,PSF,BACKGROUND.'
      else if(type.eq.'DATA') then
         flo=21
         fhi=40
         list='MEM_FILE,X,Y,SOLID.'
      else
         flo=1
         fhi=40
         list='MEM_FILE,X,Y,SOLID,PSF,BACKGROUND.'
      endif


      call wruser(' ',istat)
      call wruser(prompt,istat)
      nopt=1
      call gtstrn('FILE',.true.,list,1,nopt,text,ltext,istat)
      call cnpar('FILE',istat)

      if(text.eq.'MEM_FILE') then

         file=flo
         call getpar('FILE','INTEGER',1,flo,fhi,.true.,file,rval,istat)
   
         start=ME_kb(file)
         if(start.le.0.or.start.gt.PR_mem) then
            call wruser('*** File is not used by MEMCRDD',istat)
            ierr=1
            goto 999
         else
            if(file.ge.1.and.file.le.20) then
               size=ME_mj
               type='IMAGE'
            else
               size=ME_mk
               type='DATA'
            endif
         endif
   
      else  if(text.eq.'X') then
         file=-1
         start=B6_x
         size=ME_mk
         type='DATA'

      else if(text.eq.'Y') then
         file=-2
         start=B6_y
         size=ME_mk
         type='DATA'

      else if(text.eq.'SOLID') then
         file=-3
         start=B6_sol
         size=ME_mk
         type='DATA'

      else if(text.eq.'PSF') then
         group=1
         call getpar('GROUP','INTEGER',1,1.0,real(A8_ngp),.true.,group,
     :                   rval,istat)
         file=-(10*group+4)
         start=B6_psf(group)
         size=ME_mj
         type='IMAGE'

      else if(text.eq.'BACKGROUND') then
         file=-5
         start=B6_bac
         size=ME_mj
         type='IMAGE'

      else
         call wruser(' *** No such file',istat)

      endif


      call cnpar('FILE',istat)
      call cnpar('GROUP',istat)

 999  continue

      end



C------------------------------------------------------------------
      subroutine stats(data,size,start)
      implicit none

      integer	size,start,index,maxloc,minloc
      real	data(size),datmax,datmin,datsum,sumsq,mean,sigma

      datmax=-1.0E32
      datmin=1.0E32
      datsum=0.0
      sumsq=0.0

      do index=1,size
         if(data(index).gt.datmax) then
            datmax=data(index)
            maxloc=index
         endif
         if(data(index).lt.datmin) then
            datmin=data(index)
            minloc=index
         endif
         datsum=datsum+data(index)
         sumsq=sumsq+data(index)**2
      enddo

      mean=datsum/size
      sigma=sqrt(max(0.0,sumsq/size-mean**2))

      write(*,*) '    Start address: ',start
      write(*,*) '    Size         : ',size
      write(*,*) '    Max value    : ',datmax
      write(*,*) '     (occured at : ',maxloc,')'
      write(*,*) '    Min value    : ',datmin
      write(*,*) '     (occured at : ',minloc,')'
      write(*,*) '    Mean value   : ',mean
      write(*,*) '    Sigma        : ',sigma

      end


C--------------------------------------------------------------
      subroutine ident(data,size)
      implicit none

      include '(PR_DEC)'
      include 'UTILITIES(IR_PAR)'
      include 'UTILITIES(DT_DAT)'
      include '(B2_COM)'
      include '(B5_COM)'

      integer	size,istat,index,start,end,data(size),sdc,det,samp,crddf
      real	rval

      write(*,*) '  Data sets have ',size,' elements'
 10   continue


      start=0
      call wruser('  Give first element no. to indentify (0 quits)',
     :              istat)
      call getpar('ELEMENT','INTEGER',1,0.0,real(size),.true.,start,
     :             rval,istat)
      call cnpar('ELEMENT',istat)


      if(start.gt.0) then

         end=start
         call wruser('  Give last element no. to identify',istat)
         call getpar('ELEMENT','INTEGER',1,real(start),real(size),
     :               .true.,end,rval,istat)
         call cnpar('ELEMENT',istat)

         do index=start,end
            sdc=data(index)
            det=mod(sdc,16)+1
            samp=sdc/(16*B5_cfa)+1
            crddf=mod(sdc/16,B5_cfa)+1
            write(*,*) '   Element ',index,': '
            write(*,*) '      Sample ',samp,' of detector #',
     :                  DT_bal(det,B2_bnd)
            write(*,*) '      from ',B2_nam(crddf)
         enddo

         goto 10

      endif
      
      end
C-----------------------------------------------------------------------
      subroutine exam(data,size)
      implicit none

      integer	size,istat,index,start,end
      real	data(size),rval

      write(*,*) '  File has ',size,' elements'
 10   continue


      start=0
      call wruser('  Give first element no. to display (0 quits)',istat)
      call getpar('ELEMENT','INTEGER',1,0.0,real(size),.true.,start,
     :             rval,istat)
      call cnpar('ELEMENT',istat)


      if(start.gt.0) then

         end=start
         call wruser('  Give last element no. to display ',istat)
         call getpar('ELEMENT','INTEGER',1,real(start),real(size),
     :               .true.,end,rval,istat)
         call cnpar('ELEMENT',istat)

         write(*,20) (data(index),index=start,end)
  20     format(1PE14.4,3E14.4)

         goto 10

      endif
      
      end


C-----------------------------------------------------------------
      subroutine detsta(data,size)
      implicit none

      include '(PR_DEC)'
      include 'UTILITIES(IR_PAR)'
      include 'UTILITIES(DT_DAT)'
      include '(B2_COM)'
      include '(B6_COM)'
      include '(B5_COM)'
      include '(ME_COM)'

      integer	sdc,det,samp,crddf,offset,nv(IR_dts,PR_crd),size,nval
      real	rval,s1(IR_dts,PR_crd),s2(IR_dts,PR_crd),data(size),ms,
     :          mx(IR_dts,PR_crd),mn(IR_dts,PR_crd),datval,mean,sigma
      logical	given

*
* INITIALIZE STATISTIC SUMS TO ZERO FOR EACH DETECTOR DATA STREAM
*
      do crddf=1,PR_crd
         do det=1,IR_dts
            s1(det,crddf)=0.0
            s2(det,crddf)=0.0
            nv(det,crddf)=0
            mx(det,crddf)=-1.0E32
            mn(det,crddf)=1.0E32
         enddo
      enddo

*
* LOOP ROUND EACH USED DATA SAMPLE
*
      do offset=0,ME_mk-1

*
* FIND WHICH DETECTOR AND CRDD FILE THE SAMPLE CAME FROM
*
         sdc=ME_sti(B6_sdc+offset)
         det=mod(sdc,16)+1
         crddf=mod(sdc/16,B5_cfa)+1

*
* INCREMENT THE CORRESPONDING SUMS
*
         datval=data(offset+1)
         s1(det,crddf)=s1(det,crddf)+datval
         s2(det,crddf)=s2(det,crddf)+datval*datval
         nv(det,crddf)=nv(det,crddf)+1
         mx(det,crddf)=max(mx(det,crddf),datval)
         mn(det,crddf)=min(mn(det,crddf),datval)

      enddo


*
*LOOP ROUND ALL CRDD FILES
*
      do crddf=1,PR_crd
         given=.false.

*
* LOOP ROUND ALL DETECTORS FOR THE CURRENT CRDD FILE, DISPLAYING
* STATISTICS IF THERE WAS ANY VALID DATA FOR THIS DETECTOR 
*
         do det=1,IR_dts      
            nval=nv(det,crddf)

            if(nval.gt.0) then

               if(.not.given) then
                  write(*,*) '  CRDD file: ',B2_nam(crddf)
                  given=.true.
               endif

               mean=s1(det,crddf)/nval
               ms=s2(det,crddf)/nval
               sigma=sqrt(max(0.0,ms-mean*mean))

               write(*,*) '    Detector #',DT_bal(det,B2_bnd),':'
               write(*,*) '      Mean : ',mean
               write(*,*) '      Sigma: ',sigma
               write(*,*) '      RMS  : ',sqrt(ms)
               write(*,*) '      Max  : ',mx(det,crddf)
               write(*,*) '      Min  : ',mn(det,crddf)
               write(*,*) '      Nsamp: ',nval
               write(*,*) ' '

            endif
         enddo
      enddo

      end

