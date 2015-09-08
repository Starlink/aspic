      subroutine aocrdd
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       
*
*SOURCE
*       AOCRDD.FOR in AOCRDD.TLB
*
*METHOD
*ARGUMENTS       
*	none
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              
*       THIS PACKAGE (.TLB):
*              
*       EDRS:
*              
*       INTERIM:
*              
*STARLINK PARAMETERS
*
*
*VAX SPECIFICS
*       implicit none
*       %val
*       do while
*       REAL*8
*       Trig functions in degrees
*       enddo
*       end of line comments
*       2 byte integer values
*       RTL routines
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 7/6/88
*-------------------------------------------------------------------
*
      implicit none

*
* INCLUDE HDS FILES
*
      include 'SAE_PAR'
      include 'DAT_ERR'
      include 'CMP_ERR'

*
* INCLUDE IRAS FILES
*
      include 'UTILITIES(IR_PAR)'
      include 'UTILITIES(DT_DAT)'
      
*
* SET LOCAL PARAMETER VALUES
*
      integer	inval	! Invalid pixel value (same as BLANK)
      integer	maxfil	! Max. number of input files allowed
      integer	maxfit	! Max. number of FITS record which can be read
      parameter ( inval=-2000000000,
     :           maxfil=1200,
     :           maxfit=80)

*
* DECLARE LOCAL VARIABLES
*
      real	angle	    ! Mean scan angle for one leg (in degrees)
      integer	ball(maxfil)! Ball no. of the current file
      integer	band(maxfil)! IRAS band number of the current file
      integer	blank(maxfil) ! BLANK descriptor value for each SDF
      real	bscale(maxfil)! BSCALE descriptor value for each SDF
      real	bzero(maxfil) ! BZERO descriptor value for each SDF
      real	cossum	    ! Sum of cosines of scan angles for one leg
      character	cval*1	    ! Dummy character argument
      integer	det(maxfil) ! Cross scan detector no. of current file
      integer	dimens(3)   ! The size of each dimension of output bdf
      character dirn*8      ! Leg direction, with-survey or anti-survey.
      character file(maxfil)*40 ! Full name of current SDF file
      character filenm*40   ! Function giving next suitable file name
      integer	iband	    ! Current IRAS band number
      integer	ierr        ! Error status
      integer	ifile	    ! Pointer to current input file
      integer	ileg	    ! Current leg of the AO
      character in(maxfit)*80 ! FITS header records
      integer	ipoint	    ! Pointer to data array of the current SDF
      integer	ipout(4)    ! Pointers to output BDFs for current leg
      integer	ival	    ! Dummy integer argument
      integer	leg(maxfil) ! AO leg no. of current SDF file
      character macro(maxfil)*31! MACRO descriptor from FITS header of 
			    ! current SDF file
      integer	ndata(2,maxfil) !Dimensions of each SDF data array
      integer	ndim        ! No. of dimensions in the current SDF array
      integer	nfile	    ! No. of input data files
      integer	nin	    ! Number of records read from FITS extension
      integer	nolegs	    ! No. of legs to the AO
      integer	nout(4)	    ! No. of distinct detector sample values
      character object(maxfil)*31! OBJECT descriptor from FITS header of
                            ! current SDF file
      integer	obs(maxfil) ! Observation number for each SDF
      character	outfil*26   ! Name of current output BDF
      character outpar(4)*8 ! Names of INTERIM parameters used to access
			    ! output BDFs
      integer	plen	    ! Non-blank length of prefix
      character	prefix*20   ! Prefix of SDF file names holding AO data
      real	refdec(maxfil)! Reference RA for each SDF
      real	refra(maxfil) ! Reference RA for each SDF
      real	rval	    ! Dummy real argument
      real	scandr(maxfil)! Scan direction; 1 - with survey, 0 - anti survey
C      real	scang(maxfil) ! Scan angle for each SDF
      real	sinsum	    ! Sum of sines of scan angles for one leg
      integer	sop(maxfil) ! SOP number for each SDF
      integer	ustrln	    ! Function giving used length of a string
      character*(DAT__SZLOC) ndf,more,fits ! Locators to structure components

      data	outpar/'OUTPUTB1','OUTPUTB2','OUTPUTB3','OUTPUTB4'/

*
* GET PREFIX OF SDF FILE NAMES HOLDING AO CRDD
*
      call rdkeyc('PREFIX',.false.,1,prefix,ival,ierr)
      if(ierr.ne.0) goto 999
      call lbgone(prefix)
      plen=ustrln(prefix)

*
* START HDS
*
      call hds_start(ierr)
      if(ierr.ne.0) goto 998

*
* LOOP ROUND FOR EACH SUITABLE SDF FILE, READING HEADER INFORMATION INTO
* INTERNAL ARRAYS
*
      nfile=1
   10 continue

*
* CHECK ARRAYS HAVE NOT OVERFLOWED
*
         if(nfile.gt.maxfil) then
            call wruser('*** Too many input files',ierr)
            goto 998
         endif

*
* SEARCH THE DIRECTORY SPECIFIED IN THE PREFIX FOR THE NAME OF THE NEXT 
* CONTAINER FILE (IF NO DIRECTORY SPECIFIED, THE CURRENT DIRECTORY WILL
* BE USED)
*
         file(nfile)=filenm(prefix(:plen)//'*.SDF')

*
* IF A FILE HAS BEEN FOUND, OPEN IT WITH HDS
*
         if(file(nfile).ne.' ') then
            call wruser('  Reading descriptors from '//file(nfile),ierr)
            ierr=SAI__OK
            call hds_open(file(nfile),'READ',ndf,ierr)

*
* EXTRACT THE LEG NUMBER FROM THE FILE NAME
* 
            call group(file(nfile),plen,leg(nfile),ierr)
            
*
* READ FITS HEADER EXTENSION
*
            call dat_find(ndf,'MORE',more,ierr)
            call dat_find(more,'FITS',fits,ierr)
            call cmp_get1c(fits,'IN',maxfit,in,nin,ierr)
            if(ierr.eq.SAI__OK) then
               call dat_annul(fits,ierr)
               call dat_annul(more,ierr)

*
* EXTRACT ITEMS FROM FITS EXTENSION
*
               call fitout(in(11),'REAL',ival,bscale(nfile),cval,ierr)
               call fitout(in(12),'REAL',ival,bzero(nfile),cval,ierr)
               call fitout(in(13),'CHARACTER',ival,rval,object(nfile),
     :                     ierr)
               call fitout(in(14),'CHARACTER',ival,rval,macro(nfile),
     :                     ierr)
               call ctoi(in(15)(26:30),sop(nfile),ierr)
               call ctoi(in(15)(39:43),obs(nfile),ierr)
               call fitout(in(19),'REAL',ival,refra(nfile),cval,ierr)
               call fitout(in(20),'REAL',ival,refdec(nfile),cval,ierr)
               call fitout(in(21),'INTEGER',band(nfile),rval,cval,ierr)
               call fitout(in(22),'INTEGER',ball(nfile),rval,cval,ierr)
               call fitout(in(49),'INTEGER',blank(nfile),rval,cval,ierr)
C               call fitout(in(68),'REAL',ival,scang(nfile),cval,ierr)
               call fitout(in(72),'REAL',ival,scandr(nfile),cval,ierr)

*
* FIND CROSS SCAN POSITION OF THE DETECTOR WITH GIVEN BALL NUMBER
*
               call getdet(ball(nfile),band(nfile),det(nfile),ierr)

*
* FIND DIMENSIONS OF DATA ARRAY, AND CHECK ITS OK
*
               call cmp_shape(ndf,'DATA_ARRAY',2,ndata(1,nfile),ndim,
     :                        ierr)
               if(ndim.ne.2.or.ndata(1,nfile).ne.3)
     :            ierr=DAT__WEIRD

*
* CHECK THAT REFERENCE POINT, SOP AND OBS ARE THE SAME
*
               if(refra(nfile).ne.refra(1).or.
     :            refdec(nfile).ne.refdec(1).or.
     :            sop(nfile).ne.sop(1).or.
     :            obs(nfile).ne.obs(1)) then
                  call wruser('*** File is not from the same AO',ierr)
                  ierr=DAT__WEIRD
               endif
*
* CHECK THAT BSCALE, BZERO AND BLANK HAVE THE RIGHT VALUES
*
               if(bscale(nfile).ne.1.0e-16.or.
     :            bzero(nfile).ne.0.0.or.
     :            blank(nfile).ne.inval) then
                  call wruser('*** Unusual value for BSCALE, BZERO'//
     :                        ' or BLANK',ierr)
                  ierr=DAT__WEIRD
               endif


            endif
*
* IF AN ERROR OCCURED PROCESSING THIS FILE, GIVE A MESSAGE. 
*
            if(ierr.ne.SAI__OK) then
               call wruser('*** Invalid FOOTPRINTS file',ierr)
            else
               nfile=nfile+1
            endif

*
* CLOSE THIS SDF FILE AND GO ROUND FOR THE NEXT ONE
*
            ierr=SAI__OK
            call hds_close(ndf,ierr)
            goto 10
         endif

*
* CORRECT THE NUMBER OF FILES FOR THE LAST PASS THROUGH THE ABOVE LOOP
*
      nfile=nfile-1

*
* IF NO SDF FILES FOUND, GIVE MESSAGE AND QUIT
*
      if(nfile.eq.0) then
         call wruser('*** No suitable SDF files found',ierr)
         goto 998
      endif

*
* FIND THE NUMBER OF LEGS
*
      nolegs=0
      do ifile=1,nfile
         nolegs=max(nolegs,leg(ifile))
      enddo

*
* PRODUCE UPTO 4 OUTPUT BDFs FOR EACH LEG OF THE AO (ONE FOR EACH BAND)
*
      do ileg=1,nolegs

*
* FIND MAX NO OF OUTPUT DATA SAMPLES PER DETECTOR FOR THIS LEG IN EACH
* BAND
*
         do iband=1,4
            nout(iband)=0
         enddo

         do ifile=1,nfile
            if(leg(ifile).eq.ileg) then
               nout(band(ifile))=max(nout(band(ifile)),ndata(2,ifile))
            endif   
         enddo

*
* OPEN THE OUTPUT BDFS (NAMED prefix_ByLx.BDF, x=leg no. y=band) AND
* FILL THEM WITH INVALID VALUES
*
         dimens(3)=3
         do iband=1,4
            if(nout(iband).gt.0) then

               write(outfil,20) prefix(:plen),iband,ileg
   20          format(A,'_B',I1,'L',I2)
               call lbgone(outfil(plen+5:))

               dimens(1)=nout(iband)
               dimens(2)=DT_bns(iband)

               call cnpar(outpar(iband),ierr)
               call wrimdf(outpar(iband),outfil,104,dimens,3,
     :                     ipout(iband),ierr)

               if(ierr.ne.0) then
                  call wruser('*** Error opening output BDF file',ierr)
                  goto 998
               endif

               call filbdf(%val(ipout(iband)),dimens(1)*dimens(2)*3,
     :                     inval)

            else
               ipout(iband)=0
            endif

         enddo

*
* INITIALISE SUMS FOR FINDING AVERAGE SCAN ANGLE FOR THIS LEG
*
c         sinsum=0.0
c         cossum=0.0

*
* GO ROUND EACH FILE CONTAINING DATA FOR THIS LEG AND COPY THE DATA FROM
* THE SDF FILE TO THE CORRECT PART OF A BDF FILE.
*
         do ifile=1,nfile
            iband=band(ifile)
            if(leg(ifile).eq.ileg.and.ipout(iband).ne.0) then

               call wruser(' Copying data from '//file(ifile),ierr)

               call hds_open(file(ifile),'READ',ndf,ierr)

               call cmp_mapv(ndf,'DATA_ARRAY','_INTEGER','READ',ipoint,
     :                       ndata(1,ifile),ierr)
               call copyao(%val(ipoint),ndata(1,ifile)/3,det(ifile),
     :                     %val(ipout(iband)),nout(iband),
     :                     DT_bns(iband),ierr)
               call cmp_unmap(ndf,'DATA_ARRAY',ierr)
               
*
* IF AN ERROR OCCURED, GIVE MESSAGE, OTHERWISE INCREMENT SUMS TO FIND
* AN AVERAGE SCAN ANGLE FOR THIS LEG
*
               if(ierr.ne.SAI__OK) then
                  call wruser('*** Failed to do copy',ierr)
               else
c                  if(abs(scang(ifile)).gt.-2000000) then
c                     sinsum=sinsum+sind(scang(ifile))
c                     cossum=cossum+cosd(scang(ifile))
c                  endif
               endif

               call hds_close(ndf,ierr)
               ierr=SAI__OK

*
* SEE IF THIS LEG IS WITH-SURVEY OR ANTI-SURVEY
*
               if( scandr(ifile) .eq. 1.0 ) then
                  dirn = 'SVY'
               else
                  dirn = 'ANTI-SVY'
               end if

            endif
         enddo

*
* CALCULATE MEAN SCAN ANGLE FOR THIS LEG
*
c         angle=atan2d(sinsum,cossum) !NB problems with SCAN_ANG FITS
				      !header. See next code section

*
* ADD REQUIRED DESCRIPTORS TO OUTPUT BDF FILES AND CLOSE THEM
*

         do iband=1,4
            if(ipout(iband).ne.0) then
               call ptdscr(outpar(iband),'NAXIS','INTEGER',3,rval,cval,
     :                     ierr)
               call ptdscr(outpar(iband),'NAXIS1','INTEGER',nout(iband),
     :                     rval,cval,ierr)
               call ptdscr(outpar(iband),'NAXIS2','INTEGER',
     :                     DT_bns(iband),rval,cval,ierr)
               call ptdscr(outpar(iband),'NAXIS3','INTEGER',3,rval,cval,
     :                     ierr)
               call ptdscr(outpar(iband),'BAND','INTEGER',iband,rval,
     :                     cval,ierr)
               call ptdscr(outpar(iband),'BLANK','INTEGER',inval,rval,
     :                     cval,ierr)
               call ptdscr(outpar(iband),'OBJECT','CHARACTER',ival,rval,
     :                     object(1),ierr)
               call ptdscr(outpar(iband),'MACRO','CHARACTER',ival,rval,
     :                     macro(1),ierr)
               call ptdscr(outpar(iband),'LEG','INTEGER',ileg,rval,
     :                     cval,ierr)
               call ptdscr(outpar(iband),'SOP','INTEGER',sop(1),rval,
     :                     cval,ierr)
               call ptdscr(outpar(iband),'OBS','INTEGER',obs(1),rval,
     :                     cval,ierr)
               call ptdscr(outpar(iband),'REF_RA','REAL',ival,refra(1),
     :                     cval,ierr)
               call ptdscr(outpar(iband),'BSCALE','REAL',ival,
     :                     1.0E-16,cval,ierr)
               call ptdscr(outpar(iband),'BZERO','REAL',ival,
     :                     0.0,cval,ierr)
               call ptdscr(outpar(iband),'REF_DEC','REAL',ival,
     :                     refdec(1),cval,ierr)
               call ptdscr(outpar(iband),'DIRN','CHARACTER',ival,rval,
     :                     dirn,ierr)

*
* DUE TO PROBLEMS WITH INTERPRETING THE FITS SCAN_ANG HEADER, A 
* TEMPORARY FIX IS USED (UNTIL INTERPRETATION IS CLEARED UP) WHICH GETS
* THE SCAN ANGLE FROM THE DETECTOR TRACK (X AND Y VALUES STORED WITH 
* DATA)
*
               call calang(%val(ipout(iband)),DT_bns(iband),nout(iband),
     :                     inval,angle,ierr)
               call ptdscr(outpar(iband),'SCAN_ANG','REAL',ival,angle,
     :                     cval,ierr)
         
               call frdata(outpar(iband),ierr)
            endif

         enddo

*
* DO NEXT LEG
*
      enddo
            
*
* FINISH
*
 998  call hds_stop(ierr)

 999  call frdata(' ',ierr)

      end



C--------------------------------------------------------------------
      subroutine copyao(in,nout,det,out,naxis1,naxis2,ierr)

      implicit  none
      include 	'SAE_PAR'

      integer	nout,det,naxis1,naxis2,ierr,samp,in(3,nout),xin,
     :          yin,datin,out(naxis1,naxis2,3)


      if(ierr.eq.SAI__OK) then

         do samp=1,nout

*
* IF THE DATA VALUE IS ZERO OR NEGATIVE, LEAVE THE OUTPUT VALUES INVALID
*
            if(in(3,samp).gt.0) then
               out(samp,det,1)=in(3,samp)
               out(samp,det,2)=in(1,samp)
               out(samp,det,3)=in(2,samp)
            endif

         enddo

      endif

      end


C------------------------------------------------------------------
      subroutine filbdf(out,size,value)

      implicit  none
      integer	size,index,value,out(size)

      do index=1,size
         out(index)=value
      enddo

      end

C-------------------------------------------------------------------
      subroutine group(file,plen,leg,ierr)

      implicit 	none
      include   'SAE_PAR'
      include   'DAT_ERR'

      integer	leg,ierr,plen,g,dot,start
      character	file*(*)

      if(ierr.eq.SAI__OK) then

         start=index(file,']')
         if(start.eq.0) start=index(file,':')
         start=start+1

         g=index(file(plen+start:),'G')+plen+start-1
         dot=index(file(plen+start:),'.')+plen+start-1

         if(g.gt.plen+start-1.and.g.lt.dot) then
            call ctoi(file(g+1:dot-1),leg,ierr)
            if(ierr.eq.0) then
               ierr=SAI__OK
            else
               ierr=DAT__WEIRD
            endif
         else
            leg=1
            ierr=SAI__OK
         endif
      endif

      end
C-------------------------------------------------------------
      subroutine fitout(rec,type,ival,rval,cval,ierr)

      implicit none
      include 'SAE_PAR'
      include 'DAT_ERR'

      character	rec*(*),type*(*),cval*(*)
      integer	ierr,ival
      real	val,rval

      if(ierr.eq.SAI__OK) then

         if(type.eq.'REAL') then
            call ctor(rec(10:40),rval,ierr)
         else if(type.eq.'INTEGER') then
            call ctoi(rec(10:40),ival,ierr)
         else if(type.eq.'CHARACTER') then
            cval=rec(10:40)
         else
            call wruser('*** Bad data type in FITOUT',ierr)
            ierr=100
         endif
   
         if(ierr.eq.0) then
            ierr=SAI__OK
         else
            call wruser('*** Error reading descriptor '//rec,ierr)
            ierr=DAT__WEIRD
         endif

      endif

      end

C-----------------------------------------------------------------
      subroutine getdet(ball,band,det,ierr)

      implicit  none
      include   'SAE_PAR'
      include   'DAT_ERR'
      include   'UTILITIES(IR_PAR)'
      include   'UTILITIES(DT_DAT)'

      integer	ball,band,det,ierr,index

      if(ierr.eq.SAI__OK) then

         det=0
         do index=1,DT_bns(band)
            if(DT_bal(index,band).eq.ball) det=index
         enddo
         if(det.eq.0) ierr=DAT__WEIRD

      endif

      end
C----------------------------------------------------------------
      subroutine calang(in,nde,nys,inval,ang,ierr)
      implicit none
      integer	shi,slo,det,samp,nys,nde,in(nys,nde,3),inval,ierr
      real	dx,dy,ang

      ierr=0
      shi=0
      slo=0
      det=0
      do while(shi-slo.lt.20)
         det=det+1
         if(det.gt.nde) then
            call wruser('*** Cannot calculate scan angle',ierr)
            ierr=18
            goto 999
         endif
         slo=0
         shi=0
         do samp=1,nys
            if(in(samp,det,1).ne.inval) then
               shi=samp
               if(slo.eq.0) slo=samp
            endif
         enddo
      enddo      

      dy=in(shi,det,3)-in(slo,det,3)
      dx=in(shi,det,2)-in(slo,det,2)
      if((dx.ne.0.0.or.dy.ne.0.0).and.
     :  (abs(dx).lt.100000.and.abs(dy).lt.100000)) then
          ang=180.0-atan2d(dy,dx)
      else
          call wruser('*** Cannot calculate scan angle (b)',ierr)
          ierr=19
      endif

 999  if(ierr.ne.0) ang=-20000000

      end
