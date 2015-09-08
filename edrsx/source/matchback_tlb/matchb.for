      subroutine matchb
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Matches the background levels of upto 20 seperate IRAS scan
*       images.
*
*SOURCE
*       MATCHB.FOR in MATCHBACK.TLB
*
*METHOD
*       A constant value is added to each image so as to match the
*       total image data in overlapping regions. The total data sum
*       in all images is preserved.
*          In order to allow 20 files to be processed, care has to
*       be taken to avoid the user exceeding his open file quota.
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              irastr,gtwork,wruser,gtinam
*       THIS PACKAGE (MATCHBACK.TLB):
*              descrs,plyave
*       EDRS:
*              wrerr,ptdscr,lbgone
*       INTERIM:
*              frdata,wrkeyr
*
*STARLINK PARAMETERS
*       IM1           First input/output image
*        ...
*       IM20          Last possible input/output image
*       OFF1(output)  Offset subtracted from IMAGE1
*        ...
*       OFF20(output) Offset subtracted from IMAGE20
*       MAXINP(error)    Accessed if max. no. of input images exceeded
*       BADPLY(error)    Accessed if PLYAVE returns a fatal error
*       OFFOUT(error)    Accessed if an offset output parameter could
*                        not be written
*       NOOVER(error)    Accessed if no scans overlap
*
*VAX SPECIFICS
*       implicit none
*       %val
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 31/3/88
*-------------------------------------------------------------------
*
      implicit none

*
* DECLARE PARAMETERS
*
      integer   maximg  ! Maximum no. of images which can be processed
      real      rinval  ! Flag for invalid real values
      integer   totsiz  ! Total size of matrix
      integer   vecsiz  ! Size of vector

      parameter (maximg=20,
     :           vecsiz=maximg*maximg/2,
     :           totsiz=vecsiz*maximg,
     :           rinval='FFFFFFFF'X)

*
* DECLARE LOCAL VARIABLES
*
      real      ave            ! Average image value within polygon
      character bdfnam*30      ! Name of BDF file containing an image
      integer   blank(maximg)  ! Array holding BLANK  descriptor values
      real      bscale(maximg) ! Array holding BSCALE descriptor values
      real      bzero(maximg)  ! Array holding BZERO  descriptor values
      real      c(6)           ! Transformation coeffs from image j to
                               ! image k
      real      cdelt1(maximg) ! Array holding CDELT1 descriptor values
      real      cdelt2(maximg) ! Array holding CDELT2 descriptor values
      real      change
      real      crota1(maximg) ! Array holding CROTA1 descriptor values
      integer   crpix1(maximg) ! Array holding CRPIX1 descriptor values
      integer   crpix2(maximg) ! Array holding CRPIX2 descriptor values
      real      crval1(maximg) ! Array holding CRVAL1 descriptor values
      real      crval2(maximg) ! Array holding CRVAL2 descriptor values
      character cval*1         ! Dummy character argument
      real      delta
      real      h(maximg,maximg)! Data sum of image k in overlap region
                               ! with image j
      integer   ic             ! Index into corner coords array
      integer   ierr           ! Error status
      integer   ifail
      integer   image          ! Current image index
      integer   ipin(maximg)   ! Array of pointers to input images
      integer   ipwork
      integer   ival
      integer   j
      integer   k
      real      matrix(vecsiz,maximg) ! Matrix of image coefficients
      integer   naxis1(maximg) ! Array holding NAXIS1 descriptor values
      integer   naxis2(maximg) ! Array holding NAXIS2 descriptor values
      real      newval
      integer   nimage         ! No. of images to be processed
      real      offset(vecsiz) ! Vector of image offset differences
      real      pixsiz         ! Pixel size in square degrees
      character pname*5        ! Parameter names associated with an image
      character prbuf*80       ! Buffer for screen output
      integer   rank
      integer   row
      real      sigma
      real      sum1
      real      sum2
      real      xc(4)          ! X coord of corners of image j
      real      xcorn(4,maximg,maximg) ! Array holding x coord of each
                               ! corner of image j in image k
      real      ycorn(4,maximg,maximg) ! Array holding y coord of each
                               ! corner of image j in image k
      real      yc(4)          ! Y coord of corners of image j


      data matrix/totsiz*0/

*
* LOOP THROUGH ALL INPUT IMAGES, COPYING THE REQUIRED DESCRIPTORS
* AND POINTERS INTO LOCAL ARRAYS. NB, ALL IMAGES MUST HAVE EQUAL PIXEL
* SIZE.
*

      image=1

   10 continue

      call descrs(image,ipin(image),crval1(image),crval2(image),
     :            crpix1(image),crpix2(image),cdelt1(image),
     :            cdelt2(image),crota1(image),naxis1(image),
     :            naxis2(image),bscale(image),bzero(image),
     :            blank(image),pixsiz,ierr)

      if(ierr.eq.0) then
         if(image.lt.maximg) then
            image=image+1
            goto 10
         else
            call wrerr('MAXINP')
            nimage=image
         endif

      else if(ierr.eq.1) then
         nimage=image-1

      else
         goto 999

      endif

*
* FOR EACH IMAGE, FIND THE CO-ORDS OF THE VERTICES OF THE POLYGON
* CONTAINING THE OVERLAP REGION (IF ANY) WITH EACH OTHER IMAGE
*

      do k=1,nimage
         do j=1,nimage
            if(k.ne.j) then

               call irastr(crval1(j),crval2(j),crpix1(j),crpix2(j),
     :                     cdelt1(j),cdelt2(j),crota1(j),crval1(k),
     :                     crval2(k),crpix1(k),crpix2(k),cdelt1(k),
     :                     cdelt2(k),crota1(k),c)

               xc(1)=1
               yc(1)=1
               xc(2)=1
               yc(2)=naxis2(j)
               xc(3)=naxis1(j)
               yc(3)=naxis2(j)
               xc(4)=naxis1(j)
               yc(4)=1

               do ic=1,4
                  xcorn(ic,j,k)=c(1)+c(2)*xc(ic)+c(3)*yc(ic)
                  ycorn(ic,j,k)=c(4)+c(5)*xc(ic)+c(6)*yc(ic)
               enddo

            endif
         enddo
      enddo

*
* FOR EACH IMAGE, FIND THE AVERAGE DATA VALUE IN THE OVERLAP REGION WITH
* EACH OTHER IMAGE
*
      do k=1,nimage
         do j=1,nimage
            if(k.ne.j) then

               call plyave(%val(ipin(k)),naxis1(k),naxis2(k),bscale(k),
     :                     bzero(k),blank(k),xcorn(1,j,k),ycorn(1,j,k),
     :                     4,ave,ierr)

               if(ierr.eq.0) then
                  h(j,k)=ave

               else if(ierr.eq.3.or.ierr.eq.4) then
                  h(j,k)=rinval

               else
                  call wrerr('BADPLY')
                  goto 999
               endif

            else
               h(j,k)=rinval
            endif

         enddo
      enddo

*
* SET UP TRANSFORMATION MATRIX AND OFFSET DIFFERENCE VECTOR PRIOR TO
* SOLVING FOR THE OFFSET OF EACH IMAGE
*
      row=0
      do k=1,nimage
         do j=k+1,nimage
            if(h(j,k).ne.rinval) then
               row=row+1
               offset(row)=h(j,k)-h(k,j)
               matrix(row,k)=1.0
               matrix(row,j)=-1.0
            endif
         enddo
      enddo

*
* IF THERE ARE NO OVERLAPPING SCANS, THEN ABORT
*
      if(row.eq.0) then
         call wrerr('NOOVER')
         goto 999
      endif

*
* SOLVE THE MATRIX EQUATION  "MATRIX*ALPHA=OFFSET" FOR THE VECTOR ALPHA
* WHICH IS THE MINIMAL LEAST SQUARES ESTIMATE OF THE OFFSET OF EACH
* INDIVIDUAL IMAGE (NB, THE VECTOR ALPHA OVERWRITES "OFFSET").
*
      ifail=0
      if(row.ge.nimage) then
         call gtwork('WORK','REAL',4*nimage,ipwork,ierr)
         if(ierr.ne.0) goto 999
         call f04jae(row,nimage,matrix,vecsiz,offset,0.0,sigma,rank,
     :               %val(ipwork),4*nimage,ifail)
      else
         call gtwork('WORK','REAL',row*(row+4),ipwork,ierr)
         if(ierr.ne.0) goto 999
         call f04jde(row,nimage,matrix,vecsiz,offset,0.0,sigma,rank,
     :               %val(ipwork),row*(row+4),ifail)
      endif

      if(ifail.ne.0) goto 999

*
* FIND THE INCREMENT TO ALPHA WHICH WILL RESULT IN TOTAL FLUX BEING
* CONSERVED
*
      sum1=0
      sum2=0
      do j=1,nimage
         sum1=sum1+offset(j)*naxis1(j)*naxis2(j)
         sum2=sum2+naxis1(j)*naxis2(j)
      enddo
      delta=sum1/sum2

*
* MODIFY THE VALUE OF THE BZERO DESCRIPTOR FOR EACH IMAGE, TO
* LOWER THE MEAN LEVEL BY THE IMAGES INCREMENTED OFFSET LEVEL
*
      call wruser(' ',ierr)
      do j=1,nimage

         change=offset(j)-delta

         write(pname,30) j
   30    format('IM',I2)
         call lbgone(pname(3:))

         newval=bzero(j)-change
         call ptdscr(pname,'BZERO','REAL',ival,newval,cval,ierr)

*
* TELL THE USER WHAT VALUE HAS BEEN ADDED SUBTRACTED FROM THE IMAGE
*
         call gtinam(pname,bdfnam,ierr)
         write(prbuf,40) change,bdfnam
   40    format('   ',G13.6,' has been subtracted from image ',A)
         call wruser(prbuf,ierr)

*
* WRITE OUT THE OFFET TO AN OUTPUT PARAMETER
*
         write(pname,50) j
   50    format('OFF',I2)
         call lbgone(pname(4:))
         call wrkeyr(pname,change,1,ierr)
         if(ierr.ne.0) call wrerr('OFFOUT')

      enddo
      call wruser(' ',ierr)

*
* FINISH
*
  999 call frdata(' ',ierr)

      end
