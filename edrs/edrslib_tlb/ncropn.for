      subroutine ncropn(device,clear,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Opens AUTOGRAPH (NCAR) graphics package for output onto any
*       SGS supported device. Puts device info into common block
*       /GRCOM/. NB GKS normalisation transformation no. 1 is selected
*       for the AUTOGRAPH 'GRAPH.' parameter.
*
*ARGUMENTS
*   INPUTS:
*       device  charcter        SGS device name
*	clear	logical		If true device is cleared on opening
*   OUTPUTS:
*       ierr    integer         Error status: 0 - Success
*
*SUBROUTINES CALLED
*       THIS PACKAGE:
*               wrerr
*       SGS:
*               sgs_widen,sgs_opnwk,sgs_icurw,sgs_sfont,sgs_sprec
*       GKS:
*               gqcf,gqnt,gqlwk,gqcr
*       AUTOGRAPH:
*               agsetp,setusv,getusv,agpwrt
*
*STARLINK PARAMETERS
*       NODEVICE/error/ Accessed if the device given cannot be
*                       translated into a valid GKS workstation type
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 22/3/88
*-------------------------------------------------------------------
*
      implicit none

*
* INCLUDE COMMON BLOCK TO HOLD INFO ABOUT DEVICE CAPABILITIES
*
      include 'GRCOM.FOR'

*
* INCLUDE COMMON BLOCK TO HOLD NCAR COLOUR INFO
*
      include 'NCCOM.FOR'

*
* DECLARE ARGUMENTS
*
      integer 		ierr
      character*(*) 	device
      logical		clear

*
* DECLARE LOCAL VARIABLES
*
      integer   anycol  ! 1 if colour available on device
      real      b0	! Blue fraction in background colour (0-1)
      integer   bzone   ! SGS base zone identifier
      real      g0	! Green fraction in background colour (0-1)
      integer   icol    ! Colour loop counter
      integer   mcoli   ! Max. no. of colour indices.
      integer   mfabte	! Max. no. of fill area bundle table entries.
      integer   mpai    ! Max. no. of pattern indices.
      integer   mplbte  ! Max. No. of polyline bundle table entries.
      integer   mpmbte  ! Max. No. of polymarker bundle table entries.
      integer   mtxbte  ! Max. No. of text bundle table entries.
      integer   ncol    ! No. of different colours available on device
      integer   npci    ! No. of preset colour indeces on device
      real      r0	! Red fraction in background colour (0-1)
      real      viewp(4)! GKS viewport limits in normalised device co-ords
      real      wind(4) ! GKS window limits in world co-ords


*
* STORE DEVICE NAME IN COMMON
*
      GR_dev=device


*
* OPEN SGS SENDING ERROR MESSAGES TO THE SCREEN
*
      call sgs_init(6,ierr)
      if(.not.clear) call sgs_clrfg(1)
      if(ierr.eq.0) call sgs_opnwk(device,bzone,ierr)
      if(ierr.ne.0) then
         call wrerr('NODEVICE')
         goto 999
      endif

*
* TRANSLATE NAME TO GKS VALUES
*
      call sgs_widen(device,GR_wty,GR_con,ierr)
      if(ierr.ne.0) goto 999

*
* GET GKS WORKSTATION IDENTIFIER
*
      call sgs_icurw(GR_wid)

*
* SELECT GKS NORMALISATION TRANSFORMATION NO. 1 IN ORDER TO ENSURE THAT
* All AUTOGRAPH OUTPUT IS VISIBLE
*
      call gqnt(1,ierr,wind,viewp)
      call agsetp('GRAPH.',viewp,4)

*
* GET THE BACKGROUND COLOUR RGB VALUES.
*
      call gqcr(GR_wid,0,1,ierr,r0,g0,b0)

*
* SEE IF DEVICE HAS REAL COLOUR (AS OPPOSED TO DIFFERING STYLES IN
* MONOCHROME)
*
      call gqcf(GR_wty,ierr,ncol,anycol,npci)

*
* GET THE MAXIMUM NO. OF COLOUR INDICES AVAILABLE AT ANY ONE TIME.
*
      call gqlwk(GR_wty,ierr,mplbte,mpmbte,mtxbte,mfabte,mpai,mcoli)

*
* IF THERE ARE SUFFICIENT REAL COLOUR INDICES AVAILABLE...
*
      if(anycol.eq.1.and.mcoli.gt.NC_ncl) then
         GR_col=.true.

*
* SET NO. OF COLOURS AVAILABLE
*
         call setusv('IM',NC_ncl)

*
*SET UP AND STORE COLOUR INDICIES FOR COLOURS DEFINED IN NC_COM
*
         do icol=1,NC_ncl

*
* IF THIS COLOUR IS THE BACKGROUND COLOUR, STORE THE INVERSE COLOUR 
* INSTEAD (SO THAT LINES DRAWN IN THIS COLOUR WILL BE VISIBLE).
*
            if( NC_ir(icol).eq.r0.and.
     :          NC_ig(icol).eq.g0.and.
     :          NC_ib(icol).eq.b0 ) then

               call setusv('IR',1.0-r0)
               call setusv('IG',1.0-g0)
               call setusv('IB',1.0-b0)

*
* OTHERWISE STORE THE ACTUAL COLOUR.
*
            else
               call setusv('IR',NC_ir(icol))
               call setusv('IG',NC_ig(icol))
               call setusv('IB',NC_ib(icol))

            endif

            call setusv('IN',NC_in(icol))
            call getusv('II',NC_ind(icol))

         enddo

*
* IF NO COLOUR IS AVAILABLE, USE THE INVERSE OF THE BACKGROUND COLOUR 
* FOR ALL PENS.
*
      else
         GR_col=.false.
         call setusv('IM',1)
         do icol=1,NC_ncl
            call setusv('IR',1.0-r0)
            call setusv('IG',1.0-g0)
            call setusv('IB',1.0-b0)
            call setusv('IN',NC_in(icol))
            call getusv('II',NC_ind(icol))
         enddo         
      endif

*
* ENABLE SGS CONTROL OF TEXT FONT AND SELECT FONT 1 (HARDWARE
* CHARACTERS)
*
      call agpwrt(0.0,0.0,' ',0,0,0,-100)
      call sgs_sfont(1)
      call sgs_sprec(2)

*
* FINISH
*
  999 continue

      end
