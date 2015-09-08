C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                         ********************
C                         *                  *
C                         * Program   DOODLE *
C                         *                  *
C                         ********************
C
C
C
C             The  quality  of  slides  which  can  be  produced  by  direct
C          photography of the ARGS screen is very high. In the form of black
C          and white copies these may  be  readily  made  into  diagrams  of
C          sufficient  resolution  and  contrast for publication. However at
C          the present  only  limited  software  is  available  for  putting
C          text,lines,arrows  and  images  in  a  flexible  way  onto such a
C          picture to transform it from an "picture" to a "diagram". 
C
C             This program does not  use  the  overlay  planes  -  all  line
C          graphics are written into a specified LUT value (0 --> 255). 
C
C             The program is designed to run  within  the  interim  STARLINK
C          command language DSCL or may be run using RUNSTAR. 
C
C
C              How to use it 
C              ------------- 
C
C             DOODLE operates in terms of "modes" - at any  given  time  you
C          are  in one or other of the modes. To change mode at any time use
C          the GREEN button. The modes are as follows: 
C
C
C                a) LINE mode (L)  - this is for drawing lines from point to
C                point on the ARGS screen using the cursor to define the end
C                points. 
C
C                This is the mode in which you enter the  program.  In  this
C                mode  the  4  buttons  on  the ARGS unit have the following
C                effects: 
C
C                i) The green button allows you to change mode - a  list  of
C                options  will  appear on the terminal. Some of these change
C                the mode but some change the value of one  of  the  display
C                parameters. 
C
C                ii) The second button (white) lowers an  imaginary  pen  to
C                allow the user to start drawing a line. If the pen was down
C                already a line at the current LUT value will be drawn  from
C                the last to the present cursor position. 
C
C                iii) This lifts the pen - if the pen was down at  the  last
C                position no line will be drawn. 
C
C                iv) This quits the program completely. 
C
C                i) and iv) hold for all of the modes. 
C
C
C                b) IMAGE (I) mode - this is for writing an  image  (REAL*4)
C                of  the  standard  STARLINK bulk data frame format into the
C                ARGS. 
C
C                Buttons i) and iv) have the usual effect and either of  the
C                inner  buttons  may  be  used  to define the centre for the
C                image on the screen. 
C
C                Scaling levels will be prompted for. 
C
C
C                c) BOX (B) mode - this  draws  a  rectangular  box  on  the
C                screen  in  the  current  LUT  value. The box is defined by
C                using the cursor to specify a pair of opposite corners  (in
C                any order). 
C
C                Either of the inner buttons may be used. Others have  usual
C                function. 
C
C
C                d) TEXT (T) mode - this allows text to be  written  to  the
C                screen at the current size,direction,LUT value and centered
C                at the cursor position. 
C
C                This again can be specified by either of the inner buttons.
C
C                The "size" of the text indicates the overall dimensions  of
C                the  box  into  which each of the characters is written (in
C                the y direction). Values can be in the range 10 to 250. 
C
C                The "direction" of the text  may  be  L(eft-to-right),U(p),
C                R(ight-to-left)  or  D(own)  - each represents a rotaion of
C                ninety degrees anti-clockwise with resect to  the  previous
C                one.  Hence if "direction" = R the text is inverted but not
C                mirror imaged. 
C
C
C                e) ZERO (Z) mode - this works in the  same  manner  as  BOX
C                mode except that the region defined by the two positions is
C                filled with zero (no box is  drawn),  -  i.e.  set  to  the
C                lowest  entry  in  the  current  LUT. This is not neccesary
C                black!. It can be used to clean the screen initially. 
C
C                It is a special case of... 
C
C
C                f) Set REGION (R) mode - this is  identical  to  ZERO  mode
C                except that the region is filled with the current LUT value
C                (the same as is used for text,lines,boxes etc). 
C
C
C          In Addition: 
C          ----------- 
C
C           ...there are other commands which may be issued after the  GREEN
C          change mode (button 1) has been depressed: 
C
C                i) Load a  new  Look-up-table  (U)  -  this  reads  from  a
C                STARLINK  format (.bdf file) LUT (as used in ASPIC etc) and
C                loads it into the ARGS display unit. 
C
C                ii) Change (C) the current LUT entry in which  plotting  is
C                performed.  Any "LINE","TEXT","BOX" or "SET" mode operation
C                will use this level (can  be  in  the  range  0  -->  255).
C                Knowledge of the LUT allows the user to select the plotting
C                colour required. 
C
C                iii) Change direction of text (D). This is only  meaningful
C                in  "TEXT"  mode  but  may  be  performed  at any time. The
C                meanings of the 4 possible directions is given above  under
C                TEXT. 
C
C                iv) Change size of text (S) allows the default size of text
C                to  be altered. The permitted range and meaning of the size
C                is explained above under TEXT. It has no  effect  on  modes
C                other than "TEXT". 
C
C
C          Any problems, suggestions for enhancements etc etc should be sent
C          to Richard Hook on RGVAD::RGOFPR or ZUVAD::RNH. 
C
C
C       R.N.Hook              RGO/UCL                    8-MAR-1983
C
C-----------------------------------------------------------------------------
C
      program DOODLE
*
*   This is a general purpose routine for drawing pictures on
*   the ARGS colour display device. There are 7 modes:
*
*    1) Line drawing at a specified LUT level.
*
*    2) Image plotting at a position specified by the cursor.
*
*    3) Text writing in a given direction and at a given size at
*       a position specified by use of the cursor.
*
*    4) Box drawing of a specified size at the cursor position.
*
*    5) Graph plotting at a cursor position with specified size
*       shape, and data.
*******************NOT CURRENTLY AVAILABLE!*********************
*
*    6) Zero a rectangular region of the screen - use similar
*       to BOX.
*
*    7) Set a rectangular region of the screen to the current
*       LUT value - use identical to ZERO.
*
*      Routines from TAGLIB are used (these only use ARGSLIB primi-
*      tives and some ARGS cursor routines held in ASPICLIB).
*
*         Richard Hook 1st November 1982.
*
*     Amended to use the latest TAGLIB routines: RNH/UCL 29/3/83
*
      integer status,iii,lll,ggg,bbb,ttt,zzz,pointer,dims(2)

      character string*80

      logical up

*
*   Set up mode parameters...
*
      parameter (lll=1, iii=2, ttt=3, bbb=4, ggg=5, zzz=6, sss=7)
*               line    image   text   box   graph   zero    set
*
*   Starlink parameters...
*
      include 'INTERIM(fmtpar)'
      include 'INTERIM(errpar)'

*
*   Try to get the ARGS - exit if not successful...
*
      call tag_init(' ',status)
      if(status.ne.0) call exit

*
*   Initialize the ARGS cursor...
*
      call args_curop('1234','C')     ! In cyan!

*
*   Fool the database into thinking that it has a 512*512 image
*   on display...
*
      call args_wrim(256,256,512,512,512,512,status)
      if(status.ne.0) then
         call wruser('Failed to set ARGS database!',status)
         call exit
      endif

*
*   Welcoming and expanetary text...
*
      call wruser(' ',status)
      call wruser('DOODLE - a program for interactive ARGS graphics',is)
      call wruser('------------------------------------------------',is)
      call wruser(' ',is)
      call wruser(' March 1983 Version.',ist)
      call wruser(' ',ist)
      call wruser(' You are initially in LINE mode',is)
      call wruser(' ',is)
      call wruser('   GREEN button changes mode',is)
      call wruser('   RED button quits the routine',is)
      call wruser(' 2nd and 3rd buttons raise and lower the pen',is)
      call wruser(' ',is)

*
*   Set flags...
*
      up=.true.       ! Start with the pen up.
      mode=lll        ! Start with mode=Line.
      idir=0          ! Start with left --> right text.
      isiz=10          ! Start with smallest characters.
      ib=0            ! To stay in loop first time round.
      nlut=255        ! Start at top of LUT.
      ncorner=0       ! Corner counter for box mode.

*
*   Loop on the buttons - exit on red...
*


      do while(ib.ne.4)
*
*   Display the current mode...
*
         call show_mode(mode,isiz,idir,nlut,up,ncorner)
*
*      Read the cursor...
*
         call args_rdcur(' ',id,ib,x,y)
*
*      If button 1 (GREEN) is pressed present options...
*
         if(ib.eq.1) then
            call options(mode,nlut,isiz,idir,status)
            ncorner=0
            go to 99
         endif
*
*      Image display mode...
*
         if(mode.eq.iii) then      ! Image mode
*
*         Prompt for and read in a STARLINK INTEGER*2 image...
*                                           =========
*
            call wruser(' ',status)
            call wruser('Name of STARLINK image for display',status)
            call rdimag('IMAGE',fmt_sw,2,dims,iact,pointer,status)
*                               ======
            call cnpar('IMAGE',status)
            if(status.ne.err_parnul) then
               if(status.gt.err_parnul) then
                  call wruser('Unable to get image!',status)
               else
*
*               Get the extreme values in the data...
*
                  call extremes(%val(pointer),dims,il,ih)
                  call wruser(' ',is)
                  call wruser('Desired scaling levels?',is)
                  call rdkeyi('LOWER',.true.,1,il,iact,status)
                  call rdkeyi('UPPER',.true.,1,ih,iact,status)
                  call cnpar('LOWER',status)
                  call cnpar('UPPER',status)
                  call tag_image(%val(pointer),dims(1),dims(2),nint(x),
     :                          nint(y),il,ih,1,1,status)
                  if(status.ne.0) then
                     call wruser('Non-zero status from "IMAGE"!',ist)
                  endif
               endif
            endif
         endif
*
*      Line mode...
*
         if(mode.eq.lll) then    ! Line mode
            if(ib.eq.2 .and. .not.up) then
               call tag_line(nlut,lastx,lasty,nint(x),nint(y),status)
               lastx=nint(x)
               lasty=nint(y)
            else if(ib.eq.3) then
               up=.true.
            else if(ib.eq.2 .and. up) then
               up=.false.
               lastx=nint(x)
               lasty=nint(y)
            endif
*
*         Text mode...
*
         else if(mode.eq.ttt) then     ! Text mode.
            if(ib.eq.2 .or. ib.eq.3) then
               call wruser('Text string?',ist)
               call rdkeyc('STRING',.false.,1,string,iactn,status)
               call cnpar('STRING',ist)
*
*            Get the length of the string...
*
               call char_length(string,len,status)
               call tag_text(nlut,isiz/9-1,nint(x),nint(y),idir,string,
     :                   len,status)
            endif
*
*         Box mode...
*
         else if(mode.eq.bbb) then
            if(ncorner.eq.0) then
               lastx=nint(x)
               lasty=nint(y)
               ncorner=1
            else if(ncorner.eq.1) then
               ncorner=0
               call tag_box(nint(x),nint(y),lastx,lasty,nlut,status)
            endif
*
*         Zero region mode (set to 0)...
*         and Set region mode (set to nlut)
*
         else if(mode.eq.zzz .or. mode.eq.sss) then
            if(ncorner.eq.0) then
               lastx=nint(x)
               lasty=nint(y)
               ncorner=1
            else
               ncorner=0
               i1=min0(lastx,nint(x))
               j1=min0(lasty,nint(y))
               i2=max0(lastx,nint(x))
               j2=max0(lasty,nint(y))
               if(mode.eq.zzz) then
                  call tag_block(0,i1,j1,i2,j2,status)
               else
                  call tag_block(nlut,i1,j1,i2,j2,status)
               endif

               call tag_flush
            endif
         endif
 99      continue
      enddo

*
*   Close down the cursor...
*
      call args_curcl

      end










      subroutine options(mode,nlut,isiz,idir,status)
*
*   This routine presents the options for the user of
*   DOODLE -
*
*     mode = lll - Draw line (buttons 2 and 3 control pen position).
*
*     mode = iii - Draw image
*
*     mode = ttt - Write text
*
*     mode = bbb - Draw box
*
*     mode = ggg - Plot graph
*
*     mode = zzz - Zero area of screen.
*
*     mode = sss - Set region to nlut.
*
*  The default text size and colour and the LUT value for
*  line drawing may also be changed.
*
*           Richard Hook 2nd November 1982
*
      character*1 answer,ans

      save ans

      call wruser(' ',ist)
      call wruser(' Options: i ...enter image mode',ist)
      call wruser('          l ...enter line mode',ist)
      call wruser('          b ...enter box mode',ist)
      call wruser('          t ...enter text mode',ist)
      call wruser('          z ...zero a region of screen',ist)
      call wruser('          r ...set a region of screen to NLUT',ist)
      call wruser('          c ...change LUT value for plotting',ist)
      call wruser('          u ...read in a new look-up-table',ist)
      call wruser('          d ...change text direction',ist)
      call wruser('          s ...change text size',ist)
      call wruser(' (i/l/b/t/z/r/c/u/d/s default no change)? ',ist)

      answer=ans
      call rdkeyc('ANSWER',.true.,1,answer,iactn,status)
      call cnpar('ANSWER',ist)
      call str$upcase(answer,answer)
      ans=answer

      if(answer.eq.'U') then
         call tag_load_lut
      else if(answer.eq.'I') then
         mode=2
      else if(answer.eq.'S') then
         call wruser(' ',ist)
         call wruser('New text size? (10-250)',ist)
         isiz=10
         call rdkeyi('SIZE',.true.,1,isiz,iac,status)
         call cnpar('SIZE',ist)
         if(isiz.lt.10 .or. isiz.gt.250) then
            call wruser(' Text size invalid - will default to 10!',
     :                  ist)
            isiz=10
         endif
      else if(answer.eq.'D') then
         call wruser(' ',ist)
         call wruser(' Direction? (u/d/l/r)',ist)
         answer='L'
         call rdkeyc('DIR',.true.,1,answer,iact,ist)
         call cnpar('DIR',ist)
         call str$upcase(answer,answer)
         if(answer.eq.'L') then
            idir=0
         else if(answer.eq.'R') then
            idir=2
         else if(answer.eq.'U') then
            idir=1
         else if(answer.eq.'D') then
            idir=3
         else
            idir=0
         endif
      else if(answer.eq.'C') then
         nlut=255
         call wruser('Desired LUT level for plotting?',ist)
         call rdkeyi('LEVEL',.true.,1,nlut,iac,status)
         call cnpar('LEVEL',ist)
         if(nlut.lt.0 .or. nlut.gt.255) then
            call wruser('Invalid LUT entry! Defaulting to 255',
     :                  ist)
            nlut=255
         endif
      else if(answer.eq.'L') then
         mode=1
      else if(answer.eq.'T') then
         mode=3
      else if(answer.eq.'B') then
         mode=4
      else if(answer.eq.'G') then
         mode=5
      else if(answer.eq.'Z') then
         mode=6
      else if(answer.eq.'R') then
         mode=7
      endif
      return

 99   continue
      call wruser(' Terminal input error!',is)

      end






      subroutine show_mode(mode,isiz,idir,nlut,up,ncorner)
*
*   This routine tells the user the current values of
*   the display parameters and which mode (see above)
*   he is currently working in...
*
*     Parameters supplied:
*
*       mode - The integer representing the manner of drawing
*              see above for details.
*
*       isiz,idir - size and direction of text writing.
*
*       nlut - the current LUT level for plotting.
*
*       up - logical for LINE mode - pen up/down.
*
*       ncorner - set to 1 in box mode if 1 corner has already
*                 been defined.
*
*

      character*4 adir,chars*72

      logical up    ! Flag for pen position in LINE mode

      data adir/'LURD'/


*
*   Skip a line...
*
      call wruser('Doodle..',ist)

      if(mode.eq.1) then ! Line mode.
         if(up) then
            call wruser('Line mode with pen up.',ist)
         else
            call wruser('Line mode with pen down.',ist)
         endif
         write(chars,'(''Plotting in LUT level: '',i3)') nlut
         call wruser(chars,is)
      else if(mode.eq.2) then   
         call wruser('IMAGE mode - put cursor at desired centre',is)
      else if(mode.eq.3) then   
         write(chars,'('' ( Text size: '',i3,'' Direction: '',a1,
     :    '' Plotting in level: '',i3'')'')') isiz,adir(idir+1:idir+1),
     :       nlut
         call wruser(chars,ist)
      else if(mode.eq.4) then   ! Box mode.
         if(ncorner.eq.0) then
            call wruser('Put cursor on first corner of desired box',ist)
         else
            call wruser('Put cursor on opposite corner',ist)
         endif
         write(chars,'('' Plotting in level '',i3)') nlut
         call wruser(chars,ist)
      else if(mode.eq.5) then   ! Graph mode.
         call wruser('Graph mode currently not implemented!',ist)
      else if(mode.eq.6) then   ! Area zeroing mode.
         if(ncorner.eq.0) then
          call wruser('Put cursor on first corner of area to be zeroed',
     :                  ist)
         else
            call wruser('Put cursor on opposite corner...',ist)
         endif
      else if(mode.eq.7) then
         if(ncorner.eq.0) then
         call wruser('Put cursor on first corner of region to be set',
     :                ist)
         else
         call wruser('Put cursor on opposite corner',ist)
         endif
      else
         call wruser('Invalid mode!',ist)
      endif

      end






      subroutine extremes(data,dims,il,ih)
*
*   This routine returns the highest and lowest values
*   in the supplied integer*2 data area (2d)
*
      integer dims(2)

      integer*2 data(dims(1),dims(2))

      il=32700
      ih=-32700

      do j=1,dims(2)
         do i=1,dims(1)
            if(data(i,j).gt.ih) ih=data(i,j)
            if(data(i,j).lt.il) il=data(i,j)
         enddo
      enddo

      end
      subroutine char_length(string,length,status)
*
*   Routine to find out the length of a supplied character
*   string by counting up trailing blanks...
*
*   Supplied:
*   --------
*
*   string - character string 
*
*   Returned:
*   --------
*
*   length - the length of the supplied string - all subsequent
*            characters being blanks.
*
*   status - integer - set to 0 if length > 0
*                      set to 1 if zero length string (blank).
*
*      Richard Hook UCL 29th March 1983
*
      integer status

      character*(*) string

*
*   Get full length...
*
      lenfull=len(string)

*
*   Count back from the end of the string...
*
      length=lenfull
      do while(length.gt.0)
         if(string(length:length).ne.' ') go to 999
         length=length-1
      enddo

      status=1
      return

999   continue
      status=0

      end
      subroutine tag_block(nlut,xst,yst,xend,yend,status)
*
*   TAURUS ARGS GRAPHICS
*   --------------------
*
*   Routine to set a region of the Display to a supplied
*   colour specified by a Look-up-table value.
*
*   Supplied:
*   --------
*
*   nlut - integer*4 - the colour to be set.(0-->255)
*
*   xst,yst,xend and yend - the region of the screen to be set.
*                           (integer*4 (0-511)).
*
*   Returned:
*   --------
*
*   status - integer*2 - set to 1 if region or LUT values
*                        beyond bounds 0 otherwise.
*
*            Richard Hook 14th March 1983
*
      implicit integer (a-z)
*
*   Check bounds...
*
      if(nlut.lt.0 .or. nlut.gt.255) then
         call wruser('Invalid LUT level!',ist)
         status=1
         return
      endif
      if(xst.lt.0 .or. xst.gt.511 .or.
     :   yst.lt.0 .or. yst.gt.511 .or.
     :   xend.lt.0 .or. xend.gt.511 .or.
     :   yend.lt.0 .or. yend.gt.511) then
         call wruser('Invalid region limits!',ist)
         status=1
      else
*
*      Set the colour region using ARGSLIB call...
*
         call srbloc(xst,yst,xend,yend,nlut)
*
*      N.B. this may need to be FLUSHED using TAG_FLUSH.
*
         status=0
      endif

      end
      subroutine tag_box(xst,yst,xend,yend,nlut,status)
*
*   This is a packaged version of "LINE" which draws
*   a rectangular box on the ARGS display unit.
*
*   The input data is verified and status set to 1
*   if there is a problem.
*
*            Richard Hook 22nd October 1982
*
      implicit integer (S-Z)

*
*   Check data...
*
      if(xst.lt.0 .or. xst.gt.511   .or.
     :   yst.lt.0 .or. yst.gt.511   .or.
     :   xend.lt.0 .or. xend.gt.511 .or.
     :   yend.lt.0 .or. yend.gt.511) then
         status=1
      else
         status=0
         call tag_line(nlut,xst,yst,xst,yend,ist)
         call tag_line(nlut,xst,yend,xend,yend,ist)
         call tag_line(nlut,xend,yend,xend,yst,ist)
         call tag_line(nlut,xend,yst,xst,yst,ist)
      endif

      end
       subroutine tag_circle(nlut,ixcen,iycen,irad,status)
*
*   TAURUS ARGS GRAPHICS
*   --------------------
*
*   This routine draws a circle on the ARGS colour display
*   device in LUT level NLUT.
*
*   Input parameters:
*   ----------------
*
*      nlut - The LUT level (0 --> 255) in which the circle
*             is to be plotted.
*
*      ixcen - The centre position in ARGS pixel coordinates.
*      iycen   (0 --> 511).
*
*      irad - The radius of the circle in pixels.
*
*   Returned:
*   --------
*
*      status - set to zero on successful plotting.
*
*
*   The circle is generated within the ARGS and the routine
*   is implemented using the low level routines from ARGSLIB
*
*       Richard Hook RGO 10th November 1982
*
      integer status

      call args_s1('ZDI',nlut)     ! Set the 8 bit plane to be used.
      call args_s1('XMA',ixcen)    ! Put the pen to the centre.
      call args_s1('YMA',iycen)    !  (and in y)
      call args_put1('5610'X)      ! Send the opcode (SPC0)
      call args_put1(irad)         ! and the x axis
      call args_put1(irad)         ! and the y axis (the same)


      call srsend      ! Send the buffer to the ARGS

      end
      subroutine tag_clear(status)
*
*   TAURUS ARGS GRAPHICS
*   --------------------
*
*   Routine to clear the display by writing 0 into all
*   image planes.
*
*   Returned:
*   --------
*
*   status - the status returned from "tag_block" - 0
*            if clear successful.
*
*         Richard Hook  UCL  15/3/83
*
      integer status
*
*   Set region to 0...
*
      call tag_block(0,0,0,511,511,status)
*
*   Flush to ARGS...
*
      call tag_flush

      end
      subroutine tag_colour(nlut,red,green,blue,status)
*
*   TAURUS ARGS GRAPHICS
*   --------------------
*
*   Routine to set a single entry in the Look-up-table
*   to specified red,green and blue intensity levels.
*
*   Supplied:
*   --------
*
*   nlut - integer*4 (0 --> 255) the entry in the LUT to be
*          set.
*
*   red,green and blue - integer*4 (0 --> 255) the desired
*                        colour intensities.
*
*   Returned:
*   --------
*
*   status - integer*4 - set to 0 if successful and 1 if
*                        supplied values are out of range.
*
*           Richard Hook  UCL  14th March 1983
*
      implicit integer (a-z)

      if(nlut.lt.0 .or. nlut.gt.255) then
         call wruser('Requested LUT level out of range!',ist)
         status=1
         return
      endif

      if(red.lt.0 .or. red.gt.255 .or.
     :   green.lt.0 .or. green.gt.255 .or.
     :   blue.lt.0 .or. blue.gt.255) then
         call wruser('Intensity level(s) out of range!',ist)
         status=1
      else
*
*   Set the LUT value using ARGSLIB routine...
*
         call srcol1(nlut,red,green,blue)
         status=0
      endif

      end
	SUBROUTINE CUR_PAN(ixc,iyc,ixf,iyf,UX,UY)

*+
*
*  - - - - - -
*  :  A P A N :
*  - - - - - -
*
*    D.J.KING   R.G.O.   DEC 81
*
*   AMENDED RNH/RGO 29th April 82
*-
	INTEGER IDMAX,STATUS,ID
	INTEGER*2 ARGSOUT(3),ARGSIN(4),IOFF(3),ION(3),IFLIP(3)
	CHARACTER VALUE*80
	DATA IOFF,ION,IFLIP/3*0,'0038'X,'0008'X,'0003'X,3*0/
	REAL UX,UY



* SET UP INFORMATION TO SEND TO ARGS PROGRAM

		ARGSIN(3) = IXC
		ARGSIN(4) = IYC
		ARGSIN(1) = ((IXF-1)*256) + IYF-1
		IF (IXF.LT.1) THEN
			ARGSIN(2)=0
		ELSE IF (IXF.LT.3) THEN
			ARGSIN(2)=1
		ELSE IF (IXF.LT.7) THEN
			ARGSIN(2)=2
		ELSE IF (IXF.LT.15) THEN
			ARGSIN(2)=3
		ELSE
			ARGSIN(2)=4
		ENDIF


*  SET SYSTEM CURSOR COLOUR AND ALLOW OVERLAYS

		CALL ARGS_VSR(IOFF,ION,IFLIP)

*  SWITCH ON LAMPS

		CALL ARGS_LAMPS(1,1,1,1)

*  LOAD AND RUN ARGS PROGRAM

		CALL LOAD_PANZ
		CALL WRITE_PANZ(ARGSIN)
		CALL RUN_PANZ
		CALL READ_PANZ(ARGSOUT)
		ixc= ARGSOUT(1)
		iyc = ARGSOUT(2)
		IXF =(ARGSOUT(3).AND.'FF'X)+1
		IYF = IXF
		CALL ASP_ITODZ('ZXF',IXF,VALUE,STATUS)
		CALL ASP_ITODZ('ZYF',IYF,VALUE,STATUS)
		CALL ASP_ITODZ('ZXC',AX,VALUE,STATUS)
		CALL ASP_ITODZ('ZYC',AY,VALUE,STATUS)
		CALL ARGS_WRPAR('DISPZOOM',VALUE,1,STATUS)

*  SWITCH OFF LAMPS

		CALL ARGS_LAMPS(0,0,0,0)

*  DISABLE CURSOR

		CALL ARGS_CURS('0')
		CALL ARGS_VSRRST
		CALL SRSEND
*
		CALL ARGS_DOPDB ('ARGS_DEVICE',STATUS)
		CALL ARGS_ATOU (IDMAX,AX,AY,UX,UY,STATUS)
		CALL ARGS_CLDB (STATUS)

	END

      subroutine tag_cursor(colour,xpos,ypos,ix,iy,ibutton,status)
*
*   This routine allows a cursor to be used on a graph
*   drawn by the TAGLIB routine GRAPH or for just reading
*   the ARGS pixel coordinates of the cursor.
*
*   The cursor must be initialized before the routine is called.
*
*   Supplied:
*   --------
*
*     Colour - character*1 - colour for cursor -RGBYCMW
*
*   Returned:
*   --------
*
*      xpos,ypos - the cursor position in the coordinates
*                  of the graph (reals).
*
*       ix,iy -    the ARGS pixel coordinates of selected point.
*
*      ibutton - integer - the number of the button hit (1-4).
*
*      status - set to zero if read successful.
*
*             Richard Hook ucl 26th November 1982
*
      integer status

      character*1 colour

*
*   COMMON storage for the GRAPH display parameters...
*
      common /gparas/ixc,iyc,istretch,iylen,alow,ahigh,nz

*
*   Initialize the ARGS data base for pixel coordinates...
*
      call args_wrim(256,256,512,512,512,512,status)
*
*   Read the position in screen (pixel) coordinates...
*
      call args_rdcur(' ',id,ibutton,x,y)

*
*   Mark the point with a small cross...
*
      ix=nint(x)
      iy=nint(y)
      call tag_line(255,ix-2,iy,ix+2,iy,status)
      call tag_line(255,ix,iy-2,ix,iy+2,status)
*
*   Transform to graph coordinates...
*
*   (Check that the following calculation will not lead
*    to division by zero.)
*
      if(nz*istretch.ne.0) then
         xpos=(x-real(ixc)+nz*istretch/2)*(nz-1)/
     :         (nz*istretch) + 1.0
      endif

      if(real(iylen).ne.0) then
         ypos=(y-iyc+iylen/2)*(ahigh-alow)/real(iylen)+alow
      endif


      end
      subroutine tag_ellipse(nlut,A,B,PHI,XC,YC)
*+
*
*   TAURUS ARGS GRAPHICS
*   --------------------
*
*   call tag_ellipse(nlut,A,B,PHI,XC,YC)
*
*   This routine draws an ellipse on the current image
*   major and minor axis lengths are supplied as a and b and
*   phi is the angle of the minor axis from the vertical.
*   xc and yc are the central position.
*
*   nlut is the level of the Look-up-table in which the
*   ellipse is to be drawn (0 --> 255).
*
*   This version is adapted from EDRAW for use in TAG
*   library - Richard Hook 7/1/83/.
*
*-

*
*   Calculate start point...
*
      Xlast=XC+A*COS(PHI)
      Ylast=YC-A*SIN(PHI)

      do theta=0.05,2.0*3.1415927,0.05
*
*        Calculate radial distance from centre...
*
         r=sqrt(a*a*cos(theta)*cos(theta)+
     :          b*b*sin(theta)*sin(theta))

*
*        Calculate true angular coordinate psi...
*
         psi=atan2(b*sin(theta),a*cos(theta))
*
*      Transform to image coordinates...
*
         xlast=x
         ylast=y
         x=xc+r*cos(psi-phi)
         y=yc+r*sin(psi-phi)

*
*      Plot small segment of ellipse...
*
         call tag_line(nlut,nint(xlast),nint(ylast),
     :                      nint(y),nint(x),ist)
      enddo

      end
      subroutine tag_flush
*
*   TAURUS ARGS GRAPHICS
*   --------------------
*
*   Routine to flush the buffer to the ARGS
*
*   NO ARGUMENTS
*
*      Richard Hook UCL 14th March 1982
*
      call srsend

      end
	subroutine tag_getmsg(status)
c
*
*   TAURUS ARGS GRAPHICS
*   --------------------
*
*
*   This is a slighly altered version of a routine originally part
*   of DCLCOMMS.
*
c   Routine to decode the integer status message and print
c   the interpretation on the terminal
c
c	arguments
c		status	integer	status code
c
c	wfl rgo mar 81
*
*   Altered to go into TAGLIB - Richard Hook  UCL  29th March 1983
c
	implicit integer (a-z)
	character bufadr*256
	gstatus=sys$getmsg(%val(status),msglen,bufadr,%val(15),)
	if(gstatus) then
           call wruser(bufadr(:msglen),ist)         
	else
           call wruser('Invalid status return from SYS$GETMSG!',ist)
	endif

	end
      subroutine tag_graph(data,ndata,iylen,sh,sl,xc,yc,nluta,nlutl,
     :                 stretch,xlabel,ylabel,status)
*
*   This is a general routine for drawing a graph of sorts on the
*   ARGS colour display unit.
*
*   The lines are drawn using LINE and the text with TEXT.
*
*   These both are implemented in the primitive ARGS routines
*   contained in ARGSLIB.
*
*     Input:
*
*      data - real array of numbers to be plotted.
*
*     ndata - integer - how many.
*
*      iylen - length of the y axis in ARGS pixels.
*
*      sh,sl - high and low scaling levels (real).
*
*      xc,yc - integers - the centre position for the graph
*                         in ARGS pixel coordinates.
*
*   nluta,nlutl - the LUT values in which the axes and lines will
*                 be plotted.
*
*      stretch - an expansion factor in the x direction to scale
*                to a better size. (INTEGER).
*
*    xlabel,ylabel - character string labels for the 2 axes.
*
*   Returned:
*   --------
*
*     status - integer - returned as 0 if plotting successful.
*
*             Richard Hook RGO 26th October 1982.
*
*    (Altered slightly to use only TAGLIB calls - RNH/UCL 28/3/83)
*
      integer status,xc,yc,idata(512),stretch

      real data(*)

      character*(*) xlabel,ylabel,chars*72

*
*   Common block for communication with tag_cursor...
*
      common /gparas/iaa,ibb,icc,idd,ee,ff,igg


      iaa=xc
      ibb=yc
      icc=stretch
      idd=iylen
      ee=sl
      ff=sh
      igg=ndata

      ndata=ndata*stretch    ! Scale up.

*
*   Draw the outer box...
*
*   Extend the axes slightly both ways to act as "tick" marks...
*
      call tag_line(nluta,xc-ndata/2-3,yc+iylen/2-1,xc+ndata/2-1,
     :          yc+iylen/2-1,status)
      if(status.ne.0) return
      call tag_line(nluta,xc+ndata/2-1,yc+iylen/2-1,xc+ndata/2-1,
     :          yc-iylen/2-3,status)
      if(status.ne.0) return
      call tag_line(nluta,xc+ndata/2-1,yc-iylen/2,xc-ndata/2-3,
     :          yc-iylen/2,status)
      if(status.ne.0) return
      call tag_line(nluta,xc-ndata/2,yc-iylen/2-3,xc-ndata/2,
     :          yc+iylen/2-1,status)
      if(status.ne.0) return
*
*   Label the axes...
*
      call tag_text(nluta,0,xc,yc-iylen/2-7,0,xlabel,10,status)
      call tag_text(nluta,0,xc-ndata/2-8,yc,1,ylabel,10,status)
*
*   Put on the axis limits...
*
      write(chars,'(i3)',iostat=status) ndata/stretch
      if(status.ne.0) then
         return
      endif
      call tag_text(nluta,0,xc+ndata/2-1,yc-iylen/2-10,0,chars,3,status)
      chars='1'
      call tag_text(nluta,0,xc-ndata/2-1,yc-iylen/2-10,0,chars,1,status)

      write(chars,'(e10.3)',iostat=status) sh
      if(status.ne.0) then
         call wruser('Cannot format axis limit value!',ist)
      endif
      call tag_text(nluta,0,xc-ndata/2-45,yc+iylen/2-1,0,chars,10,
     :              status)

      write(chars,'(e10.3)',iostat=status) sl
      if(status.ne.0) then
         call wruser('Cannot format axis limit value!',ist)
      endif
      call tag_text(nluta,0,xc-ndata/2-45,yc-iylen/2,0,chars,10,status)


*
*   Scale the data into the integer array "IDATA"...
*
      if(sh.eq.sl) sh=sl+1.0
      do k=1,ndata
         idata(k)=nint((data((k-1)/stretch+1)-sl)/(sh-sl)*real(iylen-1))
      enddo
*
*   Do the plot...
*
      iyl=idata(1)+yc-iylen/2+1
      do k=1,ndata
         kk=k+xc-ndata/2-1
         call tag_block(nlutl,kk,iyl,k-ndata/2+xc,idata(k)-iylen/2+yc,
     :                  status)
         iyl=idata(k)-iylen/2+yc+1
      enddo
      call tag_flush

      ndata=ndata/stretch    ! Scale back

      status=0
      end
      subroutine tag_image(data,nx,ny,ixcen,iycen,il,ih,ixzoom,
     :                 iyzoom,status)
*
*   TAURUS ARGS GRAPHICS
*   --------------------
*
*   This is a general routine for displaying a 2d
*   image on the ARGS colour display unit.
*
*   It uses the primitive ARGS routines.
*
*   Input arguments:
*   ---------------
*
*      data  -  Integer*2 array of data to be displayed.
*
*      nx,ny -  The dimensions of this data.
*
*      ixcen,iycen - The position in ARGS pixel coordinates
*                    of the centre of the image.
*
*      il,ih - Levels between which the data is to be scaled.
*              If these are set to 0 250 the data is displayed
*              directly without scaling. (This is quicker).
*
*      ixzoom,iyzoom - Software zoom factors in the 2 directions.
*                      If the data is expanded beyond the bounds
*                      of the 512 * 512 display area it will be
*                      clipped at the edges.
*
*   Returned:
*   --------
*
*     status - 0 if image transferred to the ARGS successfully.
*
*          Richard Hook UCL/AAO 20th October/December 1982
*
*   Updated to send the image parameters to the ARGS database
*   Richard Hook UCL 9/2/83
*
*   Error in dimensions of image sent to the ARGS corrected
*   RNH/UCL 29th March 1983
*

      integer status

      integer*2 array(512,512)       ! Actual data to be plotted.
      integer*2 dummy(1)

      integer*2 data(nx,ny)          ! Unzoomed data as supplied.

      logical scale                  ! Autoscaling flag.

*
*   Check if the data is already scaled into the range 0 --> 250
*   if so it does not need to be scaled again...
*
      if(il.eq.0 .and. ih.eq.250) then
         scale=.false.
      else
         scale=.true.
      endif


*
*   Calculate new effective dimensions of image...
*
      nnx=nx*ixzoom
      nny=ny*iyzoom

*
*   Fill up the actual data area to be plotted...
*
      ist=max0(1,ixcen-nnx/2)
      jst=max0(1,iycen-nny/2)
      iend=min0(512,ixcen+nnx/2-1)
      jend=min0(512,iycen+nny/2-1)

      do j=jst,jend
         jj=ny/2+(j-iycen)/iyzoom+1
         do i=ist,iend
            ii=nx/2+(i-ixcen)/ixzoom+1
            if(scale) then
               val=real(data(ii,jj)-il)/real(ih-il)
               if(val.gt.1.0) val=1.0
               if(val.lt.0.0) val=0.0
               array(i-ist+1,j-jst+1)=nint(250.0*val)
            else
               array(i-ist+1,j-jst+1)=data(ii,jj)
            endif
         enddo
      enddo

*
*   Send the image to the ARGS...
*
      ii=iend-ist+1
      jj=jend-jst+1
      call srpxi2(array,512,ii,jj,ist,jst,16,.false.,
     :            dummy,1)
      status=0
*
*   Update the ARGS database...
*
      call args_wrim(ixcen,iycen,nx,ny,nx,ny,istat)

      if(istat.ne.0) then
         write(*,'('' Failed to update ARGS datbase!'')')
      endif

      end
      subroutine tag_init(devnam,status)
*
*   TAURUS ARGS GRAPHICS
*   --------------------
*
*   Routine to initialise a named graphics device or
*   use the default one.
*
*   Supplied:
*   --------
*
*      devnam - character string - the name of the graphics
*               device (e.g. IDB0:). If this is set to ' '
*               (a null string) then the default device
*               (logical name ARGS_DEVICE) will be used.
*
*   Returned:
*   --------
*
*      status - the status return from call to SRINIT
*
      integer status,sys$crelog

      character*(*) devnam

*
*   If device string is null try to get device...
*
      if(devnam.eq.' ') then
         call srinit(1.0,.false.,status)
      else
*
*   Assign (or create) the logical name ARGS_DEVICE...
*
         status=sys$crelog(%val(2),'ARGS_DEVICE',devnam,)
         if(.not.status) then
            call wruser('Failed to create logical name!',ist)
            call tag_getmsg(status)
         else
*
*         Use the ARGSLIB routine to do the initialisation...
*
            call srinit(1.0,.false.,status)
        endif
      endif

      end
      subroutine tag_line(nlut,xst,yst,xend,yend,status)
*
*   This is a general routine for drawing a line of some
*   specified colour.
*
*
*   Input parameters:
*
*     nlut - The entry in the lut to which the line is to be
*            written.
*
*    xst,yst - The start of the line.
*
*   xend,yend - The end point of the line.
*
* Returned:
*
*    status - Returned as zero if operation completed.
*
*               Richard Hook  UCL    October 1982
*
      implicit integer (S-Z)

*
*   Check for the validity of the end points...
*
      if(xst.lt.0 .or. xst.gt.511  .or.
     :   xend.lt.0 .or. xend.gt.511  .or.
     :   yst.lt.0 .or. yst.gt.511   .or.
     :   yend.lt.0 .or. yend.gt.511) then
         status=1
         return
      else if(nlut.lt.0 .or. nlut.gt.255) then
         status=2
      endif

*
*   Draw the line using the packaged ARGS_Sn routines...
*
      call args_s1('ZDI',nlut)   ! Set the Z plane
      call args_s1('XMA',xst)
      call args_s1('YMA',yst)
      call args_s1('XMA',xend)
      call args_s1('YDA',yend)

      call srsend
      status=0

      end
      subroutine tag_load_lut
*+
*
*   TAURUS ARGS GRAPHICS
*   --------------------
*
*   This routine simply loads a standard starlink
*   image (3*256) look-up-table into the args.
*
*   The STARLINK interim environment is needed to read in
*   the LUT which is of a BDF file format.
*
*   There must be a connection file entry...
*
*   LUT/FRAME(R) {possibly with default value}
*
*   N.B. The ARGS must have been initialised.
*
*   This version (from the old LUTLOAD) - Richard Hook
*   UCL 29/3/83 
*-

      integer*4 idims(2)

      include 'INTERIM(fmtpar)'

*
*   read in the lut...
*
      call rdimag('LUT',fmt_sl,2,idims,iactdims,ipoint,istat)
      call cnpar('LUT',ist)

      if(istat.ne.0) then
         call wruser('Sorry! Unable to load that LUT',ist)
         return
      endif

*
*   Read in the LUT...
*
      call sublutload(%val(ipoint),idims)

      end


      subroutine sublutload(colour,idims)

      integer*4 idims(2)
      integer*4 colour(idims(1),idims(2))
*
*   Call ARGSLIB routine to load LUT into ARGS...
*
      call srcols(0,256,colour)

      end
      subroutine tag_pan(ix,iy,izoom,status)
*
*   TAURUS ARGS GRAPHICS
*   --------------------
*
*   Routine to pan and zoom the ARGS display using the
*   tracker ball and buttons...
*
*   Returned:
*   --------
*
*   ix,iy - the pixel coordinates of selected point.
*
*   izoom - the zoom factor.
*
*   status - currently always set to 0
*
*   Button usage:
*   ------------
*
*   RED - quit routine
*   GREEN - return to centre and zomm factor of 1
*   FIRST WHITE - Zoom up
*   SECOND WHITE - zoom down.
*
*      Richard Hook UCL 15/3/83
*
      integer status,ix,iy,izoom
*
*   Call the ASPIC routine to load a program into ARGS
*   memory...
*
      izx=1
      izy=1
      ix=256
      iy=256
      call cur_pan(ix,iy,izx,izy,ux,uy)

      izoom=izx

      status=0
   
      end
      subroutine tag_text(nlut,size,xc,yc,dir,title,ntitle,status)
c+
c
*   TAURUS ARGS GRAPHICS
*   --------------------
*
*      call tag_text(nlut,size,xc,yc,dir,title,ntitle,status)
*
c	This subrouitine displays text on the args screen.
c
c      Supplied:
*      --------
c
c          nlut: the lut entry (0 -> 255) into which the 
c                text is to be written.
c
c         size:   the size of the characters. valid range is 0-31
c                 size 0 draws text approx 10 args units high
c                 size 31 draws text approx 250 args units high
c
c	  xc,yc   the position on the args which will be the centre
c		   of the text string
c
c         dir:      the direction of the text (help at run time)
c
c         title:     the characters to be displayed
c
c         ntitle:    the number of characters to be displayed.

*   Returned:
*   --------
*
*         status - set to 0 if sucessful.
c
c   Originally...
*
c	D.J.King/rgo      -      17 march 1981
c
c	amended for new argslib  -  27 may 1981
c
C    Re-structured to run as a subroutine Richard Hook Ucl
C    October 1982.
*
*   Tidied up a bit to go into TAGLIB RNH/UCL 30/3/83
c-
	implicit integer (a-z)
	character*56 title
	character*1 fcol
*
*   Calculate character size in ARGS pixels...
*
      charsize=(size+1)*9
      status=0

c
c	Ensure that the text is always visible by adjusting
c	the values of coordinates of the centre of the string
c
		if (xc.gt.(511-(charsize/2))) xc=511-(charsize/2)-1
		if (xc.lt.(charsize/2))       xc=charsize/2
		if (yc.gt.(511-(charsize/2))) yc=511-(charsize/2)-1
		if (yc.lt.(charsize/2))       yc=charsize/2
c
c      Work out the maximum number of characters that can be typed
C      If the requested number of characters is excessive set STATUS=1
C
		if ((dir.eq.0).or.(dir.eq.2)) then 
     			nchars=(256-abs(256-xc))*2/charsize
		else
     			nchars=(256-abs(256-yc))*2/charsize
		endif
         if(nchars.gt.ntitle) then
            status=1
         endif
c
c	having obtained all the necessary information write
c	this to the args processor.
c
		nchars=ntitle
		oddeven=jmod(nchars,2)
		if ((dir.eq.0).or.(dir.eq.2)) then
			if (dir.eq.0) then
				xs=xc-(nchars/2)*charsize-(charsize*oddeven)/2
				ys=yc-(charsize)/2
			else
				xs=xc+(nchars/2)*charsize+(charsize*oddeven)/2
				ys=yc+(charsize)/2
			endif
		else
			if (dir.eq.1) then
				xs=xc+(charsize)/2
				ys=yc-(nchars/2)*charsize-(charsize*oddeven)/2
			else
				xs=xc-(charsize)/2
				ys=yc+(nchars/2)*charsize+(charsize*oddeven)/2
			endif
		endif
*
*   Send the intensity value into which the text is
*   to be written...
*
               call args_s1('ZDI',nlut)

		call args_s1('XMA',xs)
		call args_s1('YMA',ys)
		call args_s1('SSB',13)
		call args_s1('SSZ',size)
		call args_s1('SDD',dir)

		do i=1,nchars
	           call args_s1('JSI',ichar(title(i:i)))
                enddo
   
		call args_s1('SSZ',0)
		call srsend
	end
      subroutine tag_zoom(ix,iy,ixz,iyz,status)
*
*   TAURUS ARGS GRAPHICS
*   --------------------
*
*   Routine to set the display to a zoom factor in x and y
*   with a specified pixel at the centre of the screen.
*
*   Supplied:
*   --------
*
*   ix,iy - integers - the central coordinate for zoom
*                      (both in range 0 -->511)
*
*   ixz,iyz - integers - the zoom factors in x and y
*                        (must be small powers of 2)
*
*   Returned:
*   --------
*
*   status - integer - set to 0 if OK and to 1 if specified
*                      values were out of range.
*
*         Richard Hook UCL 15/3/83
*
      integer status
      status=0
      ixx=ix
      iyy=iy
      ixzz=ixz
      iyzz=iyz
*
*   Check that the supplied values are valid...
*
      if(ix.lt.0 .or. ix.gt.511 .or.
     :   iy.lt.0 .or. iy.gt.511) then
         call wruser('Centre position off screen - will use centre',is)
         ixx=256
         iyy=256
         status=1
      endif

      if(ixz.ne.1 .and.
     :   ixz.ne.2 .and.
     :   ixz.ne.4 .and.
     :   ixz.ne.8 .and.
     :   ixz.ne.16     .or.
     :   iyz.ne.1 .and.
     :   iyz.ne.2 .and.
     :   iyz.ne.4 .and.
     :   iyz.ne.8 .and.
     :   iyz.ne.16) then
         call wruser('Invalid zoom factor! - will use 1',ist)
         ixzz=1
         iyzz=1
         status=1
      endif

*
*   Use ASPIC routine to set zooming...
*
      call izoom(ixx,iyy,ixzz,iyzz,status)

      end

