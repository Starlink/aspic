      subroutine ncrbck(xmin,xmax,ymin,ymax,title,xtitle,ytitle)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Draws a set of axes for an AUTOGRAPH plot.
*
*SOURCE
*       NCRBCK.FOR in UTILITIES.TLB
*
*METHOD
*       Set AUTOGRAPH parameters to give a nice background and then
*       produce the background by a call to ezxy (the drawing of curves
*       is supressed by setting SET to -1).
*
*ARGUMENTS
*   INPUTS:
*       xmin    real            Minimum value of x on x axis
*       xmax    real            Maximum value of x on x axis
*       ymin    real            Minimum value of y on y axis
*       ymax    real            Maximum value of y on y axis
*       title   character*(*)   Title for top of axes
*       xtitle  character*(*)   Title for x axis
*       ytitle  character*(*)   Title for y axis
*
*SUBROUTINES CALLED
*       AUTOGRAPH:
*               agseti,agsetc,agsetf,ezxy
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
* DECLARE ARGUMENTS
*
      real      xmax,xmin,ymax,ymin
      character title*(*),xtitle*(*),ytitle*(*)
*
* DECLARE LOCAL VARIABLES
*
      integer   ustrln  ! A function giving used length of a string
      real      x(2)    ! Temporary storage for x limits
      real      y(2)    ! Temporary storage for y limits
*
* SET MAXIMUM LABEL LENGTH TO 80 CHARACTERS AND DISABLE SQUASHING OF
* LABELS
*
      call agsetf('LIN/MA.',80.0)
      call agsetf('LAB/CO.',1.0)
*
* SET BOTTOM (X) TITLE CHARACTER SIZE AND CONTENT
*
      call agsetc('LABEL/NAME.','B')
      call agseti('LINE/NUMBER.',-100)
      call agsetf('LINE/CHARACTER.',0.022)
      call agsetc('LINE/TEXT.',xtitle(:ustrln(xtitle))//'$')
*
* SET LEFT (Y) TITLE CHARACTER SIZE AND CONTENT
*
      call agsetc('LABEL/NAME.','L')
      call agseti('LINE/NUMBER.',100)
      call agsetf('LINE/CHARACTER.',0.022)
      call agsetc('LINE/TEXT.',ytitle(:ustrln(ytitle))//'$')
*
* SET GRAPH TITLE CHARACTER SIZE
*
      call agsetc('LABEL/NAME.','T')
      call agseti('LINE/NUMBER.',100)
      call agsetf('LINE/CHARACTER.',0.025)
*
* DISABLE SQUASHING AND ROTATING OF TICKS
*
      call agseti('BOTTOM/CO.',1)
*
* DISABLE ROTATING OF TICKS
*
      call agseti('LEFT/CO.',2)
*
* PRODUCE BACKGROUND
*
      x(1)=xmin
      x(2)=xmax
      y(1)=ymin
      y(2)=ymax
      call agseti('SET.',-1)
      call ezxy(x,y,2,title(:ustrln(title))//'$')
*
* FINISH
*
      end
