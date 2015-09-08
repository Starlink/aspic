      subroutine fixinv
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Replace all invalid pixels within an image with a value
*       specified by the user.
*
*SOURCE
*       FIXINV.FOR in FIXINVAL.TLB
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              gt2dir,gt2diw,wrerrr,repinv
*       EDRS:
*              gtstds,getpar,ptdscr
*       INTERIM:
*              cydscr,rdkeyc,frdata
*STARLINK PARAMETERS
*       INPUT/read/     The input image
*       OUTPUT/read/    The output image
*       VALUE/read/     The value with which to replace invalid pixels
*       TITLE/read/     The title for the output
*       TOOBAD/read/    Accessed if too many bad values are given for a
*                       parameter
*
*VAX SPECIFICS
*       implicit none
*       %val
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 12/4/88
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE LOCAL VARIABLES
*
      integer   cval    ! Dummy character argument
      real      hilim   ! Highest value possible in input
      integer   ierr    ! Error status
      integer   inval   ! The invalid pixel value in the input
      integer   intval  ! Integer representation of value
      integer   ipin    ! Pointer to input
      integer   ipout   ! Pointer to output
      integer   ival    ! Dummy integer argument
      real      lolim   ! Lowest value possible in input
      integer   maxint  ! Maximum integer representable in I*2 format
      integer   minint  ! Minimum integer representable in I*2 format
      integer   nlin    ! No. of lines in i/p
      integer   npix    ! No. of pixels per line in i/p
      integer   rval    ! Dummy real argument
      real      scale   ! BSCALE descriptor value from input
      character title*30! Title from input
      real      value   ! Value with which invalid pixels are replaced
      real      zero    ! BZERO descriptor value from input

      parameter (maxint=32767,minint=-32767)
*
* GET INPUT IMAGE AND STANDARD DESCRIPTORS
*
      call gt2dir('INPUT',102,.false.,npix,nlin,ipin,ierr)
      if(ierr.ne.0) then
         call wrerr('TOOBAD')
         goto 999
      endif
      call gtstds('INPUT',1,inval,scale,zero,title)
*
* GET VALUE WITH WHICH TO REPLACE INVALID PIXELS. THIS VALUE MUST BE
* REPRESENTABLE USING THE INPUT SCALING
*
      hilim=scale*maxint+zero
      lolim=scale*minint+zero
      value=max(lolim,min(0.0,hilim))
      call getpar('VALUE','REAL',1,lolim,hilim,.true.,ival,value,ierr)
*
* CONVERT THE REPLACEMENT VALUE TO ITS INTEGER REPRESENTATION
*
      if(scale.ne.0) then
         intval=nint((value-zero)/scale)
      else
         intval=0
      endif
*
* GET OUTPUT IMAGE
*
      call gt2diw('OUTPUT',102,.false.,npix,nlin,ipout,ierr)
      if(ierr.ne.0) then
         call wrerr('TOOBAD')
         goto 999
      endif
*
* CALL REPINV TO REPLACE ALL THE INVALID PIXELS
*
      call repinv(%val(ipin),npix,nlin,inval,%val(ipout),intval)
*
* COPY THE INPUT DESCRIPTORS TO THE OUTPUT AND MODIFY THE INVAL
* DESCRIPTOR TO INDICATE THAT THERE ARE NO INVALID PIXELS IN THE IMAGE.
* THE TITLE IS ALSO UPDATED
*
      call cydscr('INPUT','OUTPUT',ierr)
      call ptdscr('OUTPUT','INVAL','INTEGER',-100000,rval,cval,ierr)
      call rdkeyc('TITLE',.true.,1,title,ival,ierr)
      call ptdscr('OUTPUT','TITLE','CHARACTER',ival,rval,title,ierr)
*
* RELEASE DATA AREAS AND FINISH
*
  999 call frdata(' ',ierr)

      end
