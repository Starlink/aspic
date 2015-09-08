      subroutine gtfont(param,ifont)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Gets a valid GKS font number from the user, but does not
*       make it the current font.
*
*SOURCE
*       GTFONT.FOR in UTILITIES.TLB
*
*METHOD
*       A GKS based graphics package must have been opened with either
*       GROPEN or NCROPN. The workstation type stored in the common
*       block GR_COM is used to enquire all the valid fonts from GKS.
*       The user is then given the option of selecting one from the
*       list of valid font numbers.
*
*ARGUMENTS
*   INPUTS:
*       param   character       The name of the Starlink Interim
*                               parameter to be used.
*   OUTPUTS:
*       ifont   integer         The selected font
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              ustrln,gtstrn
*       EDRS:
*              lbgone
*       GKS:
*              gqtxf
*
*STARLINK PARAMETERS
*       'param'     The argument param contains the name of the
*                   parameter used to aquire a font number from the user
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 25/3/88
*-------------------------------------------------------------------
*
      implicit none
*
* INCLUDE COMMON BLOCK HOLDING GRAPHICS DEVICE INFO
*
      include 'UTILITIES(GR_COM)'
*
* DECLARE ARGUMENTS
*
      character param*(*)
      integer   ifont
*
* DECLARE LOCAL VARIABLES
*
      integer   maxfnt  ! Maximum no. of fonts which can be handled
      parameter (maxfnt=30)

      character buf*5   ! Buffer for formatted font number
      character cmdlst*100   ! List of formatted valid font numbers
      character cval*1  ! Dummy character argument
      logical   done    ! True if current font number has occured before
      integer   font(maxfnt) ! Array of font numbers
      integer   i       ! Loop count
      integer   ierr    ! Error status
      integer   ival    ! Dummy integer argument
      integer   j       ! Loop count
      real      maxchh  ! Maximum character height
      real      minchh  ! Minimum character height
      real      maxchx  ! Maximum character expansion factor
      real      minchx  ! Minimum character expansion factor
      integer   nchh    ! No. of available character heights
      integer   nchx    ! No. of available character expansion factors
      integer   nfont   ! No. of valid fonts available
      integer   nopt    ! Position of selected option within option list
      integer   nptxi   ! No. of predifined text indecises
      integer   prec    ! Text precision

      integer   ustrln  ! Function giving used length of a string
*
* FIND OUT HOW MANY VALID GKS FONT NUMBERS ARE DEFINED
*
      ifont=1
      call gqtxf(GR_wty,1,ierr,nfont,font(1),prec,nchh,minchh,maxchh,
     :           nchx,minchx,maxchx,nptxi)
      if(ierr.ne.0) goto 999
      write(cmdlst,'(I5)') font(1)
      call lbgone(cmdlst)
*
* LOOP ROUND PUTTING ALL THE FONT NUMBERS INTO A CHARACTER STRING
* SUITABLE FOR USE WITH GTSTRN
*
      do i=2,min(nfont,maxfnt)
         call gqtxf(GR_wty,i,ierr,nfont,font(i),prec,nchh,minchh,maxchh,
     :              nchx,minchx,maxchx,nptxi)
         if(ierr.ne.0) goto 999
*
* IF THIS FONT HAS ALREADY BEEN DONE, JUMP TO NEXT FONT
*
         done=.false.
         do j=1,i-1
            if(font(i).eq.font(j)) done=.true.
         enddo
         if(.not.done) then
            write(buf,'(I5)') font(i)
            call lbgone(buf)
            cmdlst=cmdlst(:ustrln(cmdlst))//','//buf(:ustrln(buf))
         endif
      enddo
*
* GET REQUIRED FONT NUMBER FROM USER
*
      cmdlst=cmdlst(:ustrln(cmdlst))//'.'
      nopt=1
      call gtstrn(param,.true.,cmdlst,1,nopt,cval,ival,ierr)
      ifont=font(nopt)
*
* FINISH
*
 999  continue

      end
