      subroutine convol
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Produces an image which is a convolution of an input EDRS image
*       with a PSF given in another EDRS image. The PSF can optionally
*       be rotated and shifted before being used.
*
*SOURCE
*       CONVOL.FOR in CONVOLVE.TLB
*
*METHOD
*       The input data and PSF images are obtained. The transformation
*       to be applied to the PSF before use is obtained. The PSF is
*       rebinned to be on the same grid as the data image and then
*       converted to reals. The convolved image
*       is produced as real data values. If the convolution was
*       successful, the optimum scaling for the output I*2 image is
*       calculated from the maximum and minimum data values in the
*       convolved image, and the output image is then produced.
*
*ARGUMENTS
*       none
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               gtwork,i2torl,wrerr,convim,rltoi2,putpar
*       THIS PACKAGE (CONVOLVE.TLB):
*               rebsiz
*       EDRS:
*               gt2dir,gtdscr,rebin,gt2diw,ptdscr,lbgone,getpar
*       INTERIM:
*               rdkeyr,rdkeyc,cydscr,frdata,wruser
*
*STARLINK PARAMETERS
*       IMAGE/read/     The input data image
*       PSF/read/       The input psf image
*       TRCOEFFS/read/  The co-efficients of the transformation to be
*                       applied to the psf before use. The
*                       transformation should map output pixel (1,1) to
*                       the pixel in the PSF which is to be used as the
*                       PSF centre.
*       ILEVEL/read/    User infromation level. If greater than 1 then
*                       the relation between i/p and o/p values is
*                       displayed on the terminal scareen.
*       MINPIX/read/    Minimum no. of contributions required
*                       for a valid output pixel.
*       TITLE/read/     The title for the output image
*       OUTPUT/read/    The name of the file to hold the output image
*       NOREBIN/error/  Accessed if error occured rebinning the psf
*       TOOBAD/error/   Accessed if error occured getting output file
*
*VAX SPECIFICS
*       implicit none
*       %val
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 21/11/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE LOCAL VARIABLES
*
      real      bscale  ! Scale factor for integers in output image
      real      bzero   ! Zero level offset for integers in output image
      real      c(6)    ! Transformation co-efficients to apply to PSF
      character cval*1  ! Dummy character argument
      real      factor  ! Expansion in area caused by the transformation
      integer   ierr    ! Error status
      integer   ilevel  ! User information level
      integer   invald  ! Invalid pixel flag for data image
      integer   invalp  ! Invalid pixel flag for PSF image
      integer   ipd     ! Pointer to data image
      integer   ipnout  ! Pointer to INTEGER workspace holding no. of
                        ! valid contributions to each output pixel
      integer   ipout   ! Pointer to I*2 output image
      integer   ipp     ! Pointer to PSF image
      integer   iprbin  ! Pointer to rebinned  I*2 version of PSF
      integer   iprd    ! Pointer to REAL version of data image
      integer   iprout  ! Pointer to real workspace for output data
      integer   ipwout  ! Pointer to real workspace for output weights
      integer   iprpsf  ! Pointer to rebinned REAL version of PSF
      integer   ival    ! Dummy integer argument
      integer   maxint  ! Maximum useful integer value available in I*2
      integer   minint  ! Minimum useful integer value available in I*2
      integer   minpix  ! Min no. of contributions required for an
                        ! output pixel.
      integer   ninval  ! No. of invalid pixels in output image
      integer   nlind   ! No. of lines in data image
      integer   nlinp   ! No. of lines in PSF image
      integer   nlinr   ! No. of lines in rebinned PSF image
      integer   npixd   ! No. of pixels in each line of data image
      integer   npixp   ! No. of pixels in each line of PSF image
      integer   npixr   ! No. of lines in rebinned PSF image
      integer   option  ! Position of selected option within list
      real      outmax  ! Maximum data value in output image
      real      outmin  ! Minimum data value in output image
      character prbuf*80! Buffer for terminal output
      integer   psfxcn  ! X co-ord of pixel in real PSF image to be
                        ! mapped to output pixel (1,1)
      integer   psfycn  ! Y co-ord of pixel in real PSF image to be
                        ! mapped to output pixel (1,1)
      real      rinval  ! The flag for invalid pixels in real images
      real      rval    ! Dummy real argument
      real      scaled  ! Scale factor of data image integers
      real      scalep  ! Scale factor of PSF image integers
      character title*30! Title of data image and output image
      real      zerod   ! Zero level offset of data image integers
      real      zerop   ! Zero level offset of PSF image integers
*
* PARAMETER VALUES
*
      parameter (maxint=32766,minint=-32765,rinval=-1.0e32)
*
* GET INPUT IMAGE AND EDRS DESCRIPTORS
*
      call gt2dir('IMAGE',102,.false.,npixd,nlind,ipd,ierr)
      if(ierr.ne.0) goto 999
      title=' '
      invald=-100000
      scaled=1.0
      zerod=0.0
      call gtdscr('IMAGE','TITLE','CHARACTER',ival,rval,title,ierr)
      call gtdscr('IMAGE','BZERO','REAL',ival,zerod,cval,ierr)
      call gtdscr('IMAGE','BSCALE','REAL',ival,scaled,cval,ierr)
      call gtdscr('IMAGE','INVAL','INTEGER',invald,rval,cval,ierr)
*
* CONVERT TO REAL DATA IMAGE
*
      call gtwork('RA','REAL',npixd*nlind,iprd,ierr)
      if(ierr.ne.0) goto 999
      call i2torl(%val(ipd),npixd,nlind,scaled,zerod,invald,rinval,
     :            %val(iprd))
*
* GET PSF IMAGE AND EDRS DESCRIPTORS
*
      call gt2dir('PSF',102,.false.,npixp,nlinp,ipp,ierr)
      if(ierr.ne.0) goto 999
      zerop=0.0
      scalep=1.0
      invalp=-100000
      call gtdscr('PSF','BZERO','REAL',ival,zerop,cval,ierr)
      call gtdscr('PSF','BSCALE','REAL',ival,scalep,cval,ierr)
      call gtdscr('PSF','INVAL','INTEGER',invalp,rval,cval,ierr)
*
* GET TRANSFORMATION CO-EFFICIENTS TO APPLY TO PSF IMAGE (DEFAULT
* SHIFTS PSF IMAGE SO THAT PSF CENTRE CO-INCIDES WITH PIXEL 1,1)
*
      c(1)=int(npixp/2)
      c(2)=1.0
      c(3)=0.0
      c(4)=int(nlinp/2)
      c(5)=0.0
      c(6)=1.0
      call rdkeyr('TRCOEFFS',.true.,6,c,ival,ierr)
*
* CALL REBSIZ TO CALCULATE THE SIZE AND POSITION OF THE PSF IMAGE
* AFTER IT HAS BEEN REBINNED ONTO THE DATA IMAGE GRID
*
      call rebsiz(c,nlinp,npixp,npixr,nlinr,psfxcn,psfycn,ierr)
      if(ierr.ne.0) goto 999
*
* GET WORKSPACE FOR THE REBINNED INTEGER PSF IMAGE AND A VERSION OF
* THE SAME USING REAL DATA VALUES
*
      call gtwork('RBIN','I*2',npixr*nlinr,iprbin,ierr)
      if(ierr.ne.0) goto 999
      call gtwork('RPSF','REAL',npixr*nlinr,iprpsf,ierr)
      if(ierr.ne.0) goto 999
*
* IF THE TRANSFORMATION GIVEN IS NOT THE UNIT TRANSFORMATION, REBIN
* THE PSF IMAGE
*
      if(c(1).ne.0.or.c(2).ne.1.or.c(3).ne.0.or.
     :   c(4).ne.0.or.c(5).ne.0.or.c(6).ne.1) then
         call rebin(%val(ipp),npixp,nlinp,invalp,invalp,1,npixr,1,nlinr,
     :              c,factor,1,%val(iprbin),npixr,nlinr,ierr)
         if(ierr.ne.0) then
            call wrerr('NOREBIN')
            goto 999
         endif
*
* CONVERT THE REBINNED IMAGE TO REAL REPRESENTATION
*
         call i2torl(%val(iprbin),npixr,nlinr,scalep,zerop,invalp,
     :               rinval,%val(iprpsf))
*
* IF TRANSFORMATION DOES NOT ALTER THE IMAGE, CONVERT THE ORIGINAL PSF
* IMAGE TO REAL REPRESENTATION
*
      else
         call i2torl(%val(ipp),npixr,nlinr,scalep,zerop,invalp,rinval,
     :               %val(iprpsf))
      endif
*
* GET USER INFORMATION LEVEL
*
      ilevel=2
      call getpar('ILEVEL','INTEGER',1,1.0,3.0,.true.,ilevel,rval,
     :             ierr)
*
* GET MINIMUM NO. OF CONTRIBUTIONS REQUIRED FOR A VALID OUTPUT PIXEL
*
      minpix=1
      call getpar('MINPIX','INTEGER',1,1.0,1.0E20,.true.,minpix,rval,
     :             ierr)
*
* GET TEMPORARY WORKSPACE TO HOLD OUTPUT IMAGE, NUMBER OF VALID
* CONTRIBUTIONS TO EACH OUTPUT PIXEL AND SUM OF WEIGHTS
*
      call gtwork('ROUT','REAL',npixd*nlind,iprout,ierr)
      if(ierr.ne.0) goto 999
      call gtwork('NOUT','INTEGER',npixd*nlind,ipnout,ierr)
      if(ierr.ne.0) goto 999
      call gtwork('WOUT','REAL',npixd*nlind,ipwout,ierr)
      if(ierr.ne.0) goto 999
*
* GENERATE OUTPUT IMAGE
*
      call convim(%val(iprd),npixd,nlind,%val(iprpsf),npixr,nlinr,
     :            %val(iprout),rinval,outmin,outmax,psfxcn,psfycn,
     :            %val(ipnout),minpix,ninval,%val(ipwout),ierr)
*
* IF CONVOLUTION WENT OK GET OUTPUT IMAGE FROM USER
*
      if(ierr.eq.0) then
         call gt2diw('OUTPUT',102,.false.,npixd,nlind,ipout,ierr)
         if(ierr.ne.0) then
            call wrerr('TOOBAD')
            goto 999
         endif
*
* DISPLAY THE NUMBER OF INVALID PIXELS IN OUTPUT IF REQUIRED
*
         if(ilevel.gt.2) then
            prbuf=' '
            write(prbuf,20) ninval
   20       format(' There are ',I7,' invalid pixels in the output')
            call lbgone(prbuf(12:))
            call wruser(prbuf,ierr)
            call wruser(' ',ierr)
         endif
*
* COPY THE VALUES FROM THE REAL WORKSPACE TO THE I*2 OUTPUT IMAGE
* USING OPTIMUM SCALING
*
         if((outmax-outmin).gt.1.0e-20) then
            bscale=(outmax-outmin)/(0.75*(maxint-minint))
            bzero=((outmin+outmax)-(maxint+minint)*bscale)*0.5
         else
            bscale=1.0
            bzero=outmin
         endif
         call rltoi2(%val(ipout),%val(iprout),npixd,nlind,invald,rinval,
     :               bscale,bzero)
*
* ADD DESCRIPTOR ITEMS
*
         call cydscr('IMAGE','OUTPUT',ierr)
         call rdkeyc('TITLE',.true.,1,title,ival,ierr)
         call ptdscr('OUTPUT','TITLE','CHARACTER',ival,rval,title,ierr)
         call ptdscr('OUTPUT','INVAL','INTEGER',invald,rval,cval,ierr)
         call ptdscr('OUTPUT','BSCALE','REAL',ival,bscale,cval,ierr)
         call ptdscr('OUTPUT','BZERO','REAL',ival,bzero,cval,ierr)
      endif
*
* FREE ALL DATA AREAS AND FINISH
*
  999 call frdata(' ',ierr)

      end
