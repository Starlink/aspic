      subroutine getazm(ixc,iyc,ixf,iyf,ierr)
 
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO OBTAIN THE CURRENT ARGS ZOOM STATUS FROM THE ARGS DATABASE
*
*METHOD
*       FIND LAST DISPLAYED ARGS IMAGE, READ ZOOM STATUS FROM DATABASE
*       AND RETURN ZOOM CENTRE AND X AND Y FACTORS
*
*ARGUMENTS
*       IXC,IYC (OUT)
*       INTEGER
*               THE X AND Y COORDINATES OF THE ZOOM CENTRE IN ARGS
*               SCREEN COORDINATES
*       IXF,IYF (OUT)
*       INTEGER
*               THE X AND Y ZOOM FACTORS
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG:
*                       0: SUCCESS
*                       1: NO PREVIOUS ARGS IMAGE
*                       2: THE INFORMATION IS NOT IN THE DATABASE
*               IN CASE OF ERROR, THE VALUES IXC=256,IYC=256,IXF=1,
*               IYF=1 ARE RETURNED
*
*CALLS
*       ARGS_:
*               NUMIM,RDPAR
*       ASP_:
*               DZTOI
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*----------------------------------------------------------------------
*
*
      character value*80
 
*
* DETERMINE IF THERE IS AN IMAGE ON THE ARGS
*
      ierr=0
      call args_numim(nim)
 
*
* IF NOT, ASSUME NO ZOOM
*
 
      if(nim.eq.0) then
         ierr=1
         ixf=1
         iyf=1
         ixc=256
         iyc=256
         go to 99
 
      endif
 
 
*
* OBTAIN CURRENT ZOOM STATUS FOR THE ARGS
*
      ixf=0
      call args_rdpar('DISPZOOM',1,value,nval,istat)
      call asp_dztoi('ZXC',value,ixc,istat)
      call asp_dztoi('ZYC',value,iyc,istat)
      call asp_dztoi('ZXF',value,ixf,istat)
      call asp_dztoi('ZYF',value,iyf,istat)
 
*
* IF NOT THERE, ASSUME NO ZOOM
*
 
      if(ixf.eq.0)then
         ierr=2
         ixf=1
         iyf=1
         ixc=256
         iyc=256
         go to 99
 
      endif
 
 
   99 end
 
 
 
