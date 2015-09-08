      subroutine curcmd(optlst,xlo,xhi,ylo,yhi,cmd,title,ierr)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To obtain a command from the user by requiring the user
*       to position the cursor over one of several boxes on the screen,
*       each of which has a command description written inside it.
*
*SOURCE
*       CURCMD.FOR in UTILITIES.TLB
*
*METHOD
*       The argument optlst contains a list of options in the form of
*       strings. Each option string can have spaces in it, in which case
*       the strings are split into lines to make maximum use of the
*       available plotting space. All graphics occur within the box
*       specified by arguments xlo,xhi,ylo and yhi and are independant
*       of the current world co-ordinate system, aspect ratio etc, which
*       are preserved after this routine exits.
*
*ARGUMENTS
*   INPUTS:
*       optlst  character       The list of options to be displayed,
*                               seperated by commas and terminated with
*                               a full stop (spaces are allowed within
*                               the options). Upto 8 options are allowed
*       xlo     real            The lower limit in x of the graphics
*                               area to be used by this routine,
*                               specified in terms of the current world
*                               co-ordinate system and zone.
*       xhi     real            The upper x limit for graphics.
*       yhi     real            The upper y limit for graphics.
*       ylo     real            The lower y limit for graphics.
*       title   character       A title to put above the option boxes
*       cmd     integer         Position of default option within list
*                               of options
*   OUTPUTS:
*       cmd     integer         The position within the list of options
*                               of the selected option, starting at 1
*       ierr    integer         Error status (all fatal errors are
*                               reported):    0 - Success
*                                            16 - Too many bad
*                                                 positions given
*                                            60 - optlst contained no
*                                                 options.
*
*USED BY
*       CRDDTRACE
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               ustrln
*       SGS:
*               sgs_icurz,sgs_izone,sgs_zone,sgs_sw,sgs_sartx,sgs_stxj
*               sgs_shtx,sgs_tx,sgs_spen,sgs_box,sgs_reqcu,sgs_clrbl
*               sgs_selz,sgs_relz,sgs_clrz
*
*STARLINK PARAMETERS
*       TOOBAD/error/   Too many bad cursor positions were given
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*       do while
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 4/9/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      character*(*)     optlst,title
      integer           cmd,ierr
      real              xlo,xhi,ylo,yhi
*
* DECLARE PARAMETERS
*
      integer   maxcmd  ! The no. of command options to display
      Parameter (maxcmd=10)
      integer   maxlen  ! The maximum length of a line of a description
      Parameter (maxlen=13)
*
* DECLARE LOCAL VARIABLES
*
      integer   bzone   ! SGS zone id of display area used for command boxes
      integer   cmdend  ! The position within optlst of the end of the command
      integer   cmdstr  ! The position within optlst of the start of the command
      character cmdtxt(maxcmd)*80       ! The text for each command
      integer   comma   ! The position within optlst of the next comma
      integer   deflt   ! Value of argument cmd on entry
      real      flwdth  ! Full width of command boxes assuming max text height
      integer   i       ! Character position
      integer   icmd    ! Command loop count
      integer   ilast   ! The length of a command text +1
      integer   j       ! Loop count
      integer   linend  ! Char position of end of this line of command text
      integer   lnstrt  ! Char position of start of this line of command text
      integer   lspace  ! Char position of last space in command text
      integer   maxlin  ! The max value of nlines for all command texts
      logical   more    ! Flag indicating end of do loop has been reached
      integer   n       ! SGS choice no.
      integer   nbad    ! No. of bad positions given by user
      integer   ncmd    ! No. of commands in option list limited by maxcmd
      integer   nlines(maxcmd)  ! No. of lines required to display command text
      integer   optlen  ! Length of optlst minus trailing blanks
      integer   origzn  ! Id of current zone on entry
      real      ozxhi   ! Upper limit of original zone world x co-ords
      real      ozxlo   ! Lower limit of original zone world x co-ords
      real      ozxm    ! Extent of original zone in metres along x axis
      real      ozyhi   ! Upper limit of original zone world y co-ords
      real      ozylo   ! Lower limit of original zone world y co-ords
      real      ozym    ! Extent of original zone in metres along y axis
      integer   ustrln  ! Length of string minus trailing blanks
      integer   toplen  ! Max length of lines required for command texts
      real      textht  ! Final text height used
      real      txhmax  ! Max possible height of text in available display area
      real      txwdth  ! Width of text
      real      x       ! x position of cursor
      real      xlim(2,maxcmd)  ! Limits in x of each command box
      real      xmetre  ! Extent of available display area in x (in metres)
      real      xsize   ! Size of a command box in x
      real      xspace  ! The distance between command box centres in x
      real      y       ! y position of cursor
      real      ycen    ! The centre in y of each command box
      real      ylim(2,maxcmd)  ! Limits in y of each command box
      real      ymetre  ! Extent of available display area in y (in metres)
      real      ysize   ! Size of command box in y
*
* INITIALISE ERROR STATUS TO ZERO (SUCCESS)
*
      ierr=0
*
* SAVE DEFAULT OPTION NUMBER FOR USE LATER
*
      deflt=cmd
*
* REMOVE FULL STOP FROM END OF OPTIONS LIST
*
      optlen=ustrln(optlst)
      if(optlst(optlen:optlen).eq.'.') optlen=optlen-1
*
* CHECK OPTIONS LIST IS NOT BLANK
*
      if(optlst.eq.' ') then
         ierr=60
         goto 999
      endif
*
* BREAK OPTION LIST UP INTO SEPERATE COMMAND TEXT STRINGS
*
      more=.true.
      ncmd=0
      cmdend=-1
      do while(more)
         cmdstr=cmdend+2
         comma=index(optlst(cmdstr:),',')+cmdstr-1
         if(comma.ne.cmdstr-1) then
            cmdend=comma-1
         else
            cmdend=optlen
            more=.false.
         endif
         ncmd=ncmd+1
         cmdtxt(ncmd)=optlst(cmdstr:cmdend)
         if(ncmd.eq.maxcmd) more=.false.
      enddo
*
* IF OPTIONS LIST CONTAINS ONLY ONE COMMAND THEN DONT BOTHER ABOUT USER,
* JUST RETURN WITH CMD=1
*
      if(ncmd.eq.1) then
         cmd=1
         goto 999
      endif
*
* FIND HOW MANY LINES THE COMMAND TEXT WILL NEED AND THEIR MAX LENGTH
*
      maxlin=0
      toplen=0
      do icmd=1,ncmd
         ilast=ustrln(cmdtxt(icmd))+1
         lspace=0
         nlines(icmd)=1
         lnstrt=1
         i=1
         do while(i.le.ilast)
            if(cmdtxt(icmd)(i:i).eq.' '.or.i.eq.ilast) then
               if(i-lnstrt.gt.maxlen) then
                  if(i-lspace-1.gt.maxlen) lspace=lnstrt+maxlen
                  cmdtxt(icmd)(lspace:lspace)='.'
                  toplen=max(toplen,lspace-lnstrt)
                  i=lspace
                  nlines(icmd)=nlines(icmd)+1
                  lnstrt=lspace+1
               else
                  lspace=i
               endif
            endif
            i=i+1
         enddo
         maxlin=max(maxlin,nlines(icmd))
      enddo
*
* SAVE INFO ABOUT CURRENT ZONE ON ENTRY
*
      call sgs_icurz(origzn)
      call sgs_izone(ozxlo,ozxhi,ozylo,ozyhi,ozxm,ozym)
*
* SET UP ZONE COVERING AREA OF SCREEN IN WHICH COMMAND BOXES ARE TO APPEAR
* INWHICH X AND Y SCALES ARE EQUAL ON THE SCREEN
*
      call sgs_zone(xlo,xhi,ylo,yhi,bzone,ierr)
      if(ierr.ne.0) goto 999
      call sgs_clrz
      xmetre=ozxm*(xhi-xlo)/(ozxhi-ozxlo)
      ymetre=ozym*(yhi-ylo)/(ozyhi-ozylo)
      call sgs_sw(0.0,xmetre,0.0,ymetre,ierr)
      if(ierr.ne.0) goto 999
*
* LEAVE THE TOP FIFTH FOR THE INSTRUCTIONS MESSAGE
*
      call sgs_sartx(0.6666667)
      call sgs_stxj('CC')
      call sgs_shtx(0.1*ymetre)
      call sgs_tx(0.5*xmetre,0.9*ymetre,title(:ustrln(title)))
      ymetre=0.8*ymetre
*
* CALCULATE MAXIMUM POSSIBLE TEXT HEIGHT
*
      txhmax=ymetre/(1.5*maxlin+0.5)
*
* CALCULATE TEXT WIDTH ASSUMING ASPECT RATIO OF 2/3
*
      txwdth=2*txhmax/3
*
* CALCULATE FULL WIDTH OF BOXES ASSUMING MAX POS TEXT HEIGHT
*
      flwdth=txwdth*ncmd*(toplen+5.5)
*
* IF THIS IS LONGER THAN THE AVAILABLE SPACE SCALE TEXT DOWN UNTIL ITS NOT
*
      if(flwdth.gt.xmetre) then
         txwdth=txwdth*xmetre/flwdth
      endif
      textht=3*txwdth/2

*
* SET TEXT HEIGHT AND JUSTIFICATION
*
      call sgs_shtx(textht)
      call sgs_stxj('CC')
*
* CALCULATE SIZE AND SPACING OF COMMAND BOXES
*
      xspace=xmetre/ncmd
      xsize=(toplen+4.5)*txwdth
      ysize=textht*(1.5*maxlin+0.5)
*
* DRAW BOXES
*
      do icmd=1,ncmd
         xlim(1,icmd)=0.5*(xspace*(2*icmd-1)-xsize)
         xlim(2,icmd)=0.5*(xspace*(2*icmd-1)+xsize)
         ylim(1,icmd)=(ymetre-ysize)*0.5
         ylim(2,icmd)=(ymetre+ysize)*0.5
         ycen=(ylim(1,icmd)+ylim(2,icmd))*0.5
         call sgs_spen(3)
         call sgs_box(xlim(1,icmd),xlim(2,icmd),
     :                ylim(1,icmd),ylim(2,icmd))
         lnstrt=1
         call sgs_spen(1)
         do j=1,nlines(icmd)
            linend=index(cmdtxt(icmd)(lnstrt:),'.')-2+lnstrt
            if(linend.eq.lnstrt-2) linend=ustrln(cmdtxt(icmd))
            call sgs_tx(0.5*xspace*(2*icmd-1),
     :              ycen+0.75*textht*(nlines(icmd)-2*j+1),
     :              cmdtxt(icmd)(lnstrt:linend))
            lnstrt=linend+2
         enddo
      enddo
*
* SET CURSOR TO DEFAULT POSITION
*
      deflt=max(1,min(ncmd,deflt))
      call setcup(0.5*(xlim(1,deflt)+xlim(2,deflt)),
     :            0.5*(ylim(1,deflt)+ylim(2,deflt)),ierr)
      if(ierr.ne.0) goto 999
*
* GET CURSOR POSITION FROM USER
*
      cmd=0
      nbad=0
      do while(cmd.eq.0.and.nbad.le.4)
         call sgs_reqcu(x,y,n)
*
* SEE IF IT INSIDE A BOX
*
         icmd=1
         do while(cmd.eq.0.and.icmd.le.ncmd)
            if(x.ge.xlim(1,icmd).and.x.le.xlim(2,icmd).and.
     :         y.ge.ylim(1,icmd).and.y.le.ylim(2,icmd)) cmd=icmd
            icmd=icmd+1
         enddo
         if(cmd.eq.0) nbad=nbad+1
      enddo

*
* DELETE ALL BUT SELECTED BOX
*
      if(cmd.ne.0) then
         if(cmd.gt.1) then
            call sgs_clrbl(xlim(1,1),xlim(2,cmd-1),
     :                     ylim(1,1),ylim(2,1))
         endif
         if(cmd.lt.ncmd) then
            call sgs_clrbl(xlim(1,cmd+1),xlim(2,ncmd),
     :                     ylim(1,ncmd),ylim(2,ncmd))
         endif
         call sgs_clrbl(0,xmetre,ymetre+0.0005,ymetre*1.251)
      else
         call wrerr('TOOBAD')
         ierr=16
      endif
*
* REINSTATE ORIGINAL ZONE
*
 999  call sgs_selz(origzn,ierr)
      call sgs_relz(bzone)
      call sgs_sw(ozxlo,ozxhi,ozylo,ozyhi,ierr)

      end
