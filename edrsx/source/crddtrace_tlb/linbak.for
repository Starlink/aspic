      subroutine linbak(data,npix,nlin,ilin,dsize,prouse,inval,pixel,
     :                 slope,const,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Fits a straight line to a section of the unscaled data.
*
*SOURCE
*       LINBAK.FOR in CRDDTRACE.TLB
*
*METHOD
*       Get workspace and call calbak to calculate the slope and
*       constant terms in the linear background
*
*ARGUMENTS
*   INPUTS:
*       data(npix,nlin) real            The unscaled CRDD
*       npix,nlin       integers        The dimensions of the CRDD
*       ilin            integer         The line of CRDD to use in
*                                       determining the background
*       dsize           real            The size of a data pixel in
*                                       arcmins
*       prouse          integer         The length (in data pixels) of
*                                       the non-zero part of the profile
*       inval           integer         The invalid pixel value for CRDD
*       pixel           integer         The nearest pixel to the cursor
*   OUTPUTS:
*       slope           real            Slope of background in unscale
*                                       data units per arcmin
*       const           real            The value of the background at
*                                       the cursor in unscaled units
*       ierr            integer         Error status: 0 - Success
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               gtwork
*       THIS PACKAGE (CRDDTRACE.TLB):
*               calbak
*       INTERIM:
*               frdata
*
*VAX SPECIFICS
*       implicit none
*       %val
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 15/10/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer   npix,nlin,ilin,prouse,inval,pixel,ierr
      real      data(npix,nlin),dsize,slope,const
*
* DECLARE LOCAL VARIABLES
*
      integer   size    ! The no. of data pixels to fit the background
                        ! to, centred on the cursor position
      integer   ipx     ! Pointer to real workspace to hold x positions
                        ! of data pixels
      integer   ipy     ! Pointer to real workspace to hold y positions
                        ! of data pixels
      integer   ipgrad  ! Pointer to real workspace to hold gradient of
                        ! data at each data pixel
      integer   istat   ! Dummy error status

*
* GET WORKSPACE TO HOLD TWO PROFILE WIDTHS
*
      size=prouse*2+1
      call gtwork('X','REAL',size,ipx,ierr)
      if(ierr.eq.0) call gtwork('Y','REAL',size,ipy,ierr)
      if(ierr.eq.0) call gtwork('GRAD','REAL',size,ipgrad,ierr)
*
* CALCULATE SLOPE AND CONSTANT TERMS OF LINEAR BACKGROUND
*
      if(ierr.eq.0) then
         call calbak(prouse,dsize,%val(ipx),%val(ipy),%val(ipgrad),size,
     :               inval,pixel,ilin,data,npix,nlin,slope,const)
      endif
*
* FREE WORKSPACE AND FINISH
*
  999 call frdata('X',istat)
      call frdata('Y',istat)
      call frdata('GRAD',istat)

      end
