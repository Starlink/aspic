      subroutine setazm(ixc,iyc,ixf,iyf,ierr)
 
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO ZOOM THE ARGS AND RECORD THE FACT IN THE ARGS DATABASE
*
*METHOD
*       THE ARGS IS ZOOMED ABOUT A GIVEN POINT USING GIVEN X AND Y
*       ZOOM FACTORS. THE CENTRE COORDINATES AND ZOOM FACTORS ARE
*       STORED IN THE ARGS DATABASE
*
*ARGUMENTS
*       IXC,IYC (IN)
*       INTEGER
*               THE ZOOM CENTRE POSITION IN ARGS SCREEN COORDINATES
*       IXF,IYF (IN)
*       INTEGER
*               THE X AND Y ZOOM FACTORS
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG:
*                       0: SUCCESS
*                       1: CANNOT UPDATE ARGS DATABASE
*
*CALLS
*       ARGS_:
*               SRSEND,FLUSH,PUT1,WRPAR
*       ASP_:
*               ITODZ
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*----------------------------------------------------------------------
*
*
      character value*80
 
*
* RESET THE ARGS ZOOM
*
      ierr=0
      call args_flush(4)
      call args_put1('C000'x+ixc)
      call args_put1('A000'x+iyc)
      call args_put1('5001'x)
      call args_put1(256*(iyf-1)+ixf-1)
      call srsend
 
*
* UPDATE ARGS DATABASE WITH ZOOM STATUS
*
      call asp_itodz('ZXF',ixf,value,istat)
      call asp_itodz('ZYF',iyf,value,istat)
      call asp_itodz('ZXC',ixc,value,istat)
      call asp_itodz('ZYC',iyc,value,istat)
      call args_wrpar('DISPZOOM',value,1,istat)
 
      if(istat.ne.0)ierr=1
 
      end
 
 
 
