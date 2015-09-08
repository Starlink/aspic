      subroutine xypan(ixargs,iyargs,ux,uy,exit,ierr)
 
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO OBTAIN X,Y COORDINATES FROM THE ARGS SCREEN, ALLOWING
*       THE USER TO PAN AND ZOOM TO SELECT THE REQUIRED POINT.
*
*METHOD
*       OBTAIN CURRENT ZOOM STATUS FROM THE ARGS DATABASE, SEND THIS
*       INFORMATION TO THE ARGS AND CALL XYPANA TO CONTROL THE ARGS.
*       ON RETURN, EXTRACT THE COORDINATES AND A LOGICAL FLAG TO
*       INDICATE IF THE USER WANTS TO FINISH. UPDATE THE ZOOM
*       STATUS IN THE DATABASE. FINALLY TRANSFORM TO USER COORDINATES
*       USING THE ARGS TO USER TRANSFORMATION IN THE DATABASE.
*
*ARGUMENTS
*       IXARGS,IYARGS (OUT)
*       INTEGER
*               RETURNS THE SELECTED POSITION IN ARGS COORDINATES
*               (0 TO 511)
*       UX,UY (OUT)
*       REAL
*               RETURNS THE SELECTED POSITION IN USER COORDINATES
*       EXIT (OUT)
*       LOGICAL
*               INDICATES THE USER WANTS TO FINISH. THE COORDINATES
*               RETURNED ARE STILL GOOD, HOWEVER.
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG... 0: SUCCESS
*                             1: NO IMAGE ON ARGS...THE COORDINATES
*                                WILL BE IN ARGS UNITS, NOT USER UNITS
*
*USE OF ARGS BUTTONS
*       BUTTON 1 (LEFT)
*               SELECT X,Y POSITION
*       BUTTON 2
*               DECREASE ARGS ZOOM BY A FACTOR OF 2
*       BUTTON 3
*               INCREASE ARGS ZOOM BY A FACTOR OF 2
*       BUTTON 4 (RIGHT)
*               EXIT
*
*CALLS
*       THIS PACKAGE:
*               LOAD_XYPANA,WRITE_XYPANA,RUN_XYPANA,READ_XYPANA
*       ARGSLIB:
*               SRSEND
*       ASP_:
*               DZTOI,ITODZ
*       ARGS_:
*               NUMIM,RDPAR,VSR,LAMPS,WRPAR,CURS,VSRRST,DOPDB,ATOU,
*               CLDB
*
*NOTES
*       USES INTEGER*2 ARRAYS AND SUBROUTINE NAMES WITH MORE THAN
*       6 CHARACTERS.
*
*WRITTEN BY
*       D.J. KING, RGO (ORIGINALLY CALLED ASP_PAN), MODIFIED AND
*       RENAMED BY R.F. WARREN-SMITH, FOR USE WITH XYCUR
*----------------------------------------------------------------------
*
*
      logical exit
      integer status
      integer*2 argout(4),argsin(4),ioff(3),ion(3),iflip(3)
      character value*80
      data ioff,ion,iflip/3*0,'0038'x,'0008'x,'0003'x,3*0/
 
*
* CHECK DATABASE TO FIND IF ANYTHING IS DISPLAYED
*
      ierr=0
      call args_numim(idmax)
 
      if (idmax.eq.o)ierr=1
 
*
* GET ZOOM FACTOR AND CENTRE FROM ARGS DATABASE
*
      value=' '
      call args_rdpar('DISPZOOM',1,value,nvals,status)
      call asp_dztoi('ZXC',value,ixc,status)
      call asp_dztoi('ZYC',value,iyc,status)
      call asp_dztoi('ZXF',value,ixf,status)
      call asp_dztoi('ZYF',value,iyf,status)
 
*
* IF NO INFORMATION, ASSUME ZOOM FACTOR IS 1
*
 
      if (ixf.eq.0) then
         ixf=1
         iyf=1
         ixc=256
         iyc=256
      endif
 
 
*
* SET UP INFORMATION TO SEND TO ARGS PROGRAM
*
      argsin(3) = ixc
      argsin(4) = iyc
      argsin(1) = ((ixf-1)*256) + iyf-1
 
      if (ixf.lt.1) then
         argsin(2)=0
 
      else if (ixf.lt.3) then
         argsin(2)=1
 
      else if (ixf.lt.7) then
         argsin(2)=2
 
      else if (ixf.lt.15) then
         argsin(2)=3
 
      else
         argsin(2)=4
      endif
 
 
*
*  SET SYSTEM CURSOR COLOUR AND ALLOW OVERLAYS
*
      call args_vsr(ioff,ion,iflip)
 
*
*  SWITCH ON LAMPS
*
      call args_lamps(1,1,1,1)
 
*
*  LOAD AND RUN ARGS PROGRAM
*
      call load_xypana
      call write_xypana(argsin)
      call run_xypana
      call read_xypana(argout)
 
*
* EXTRACT RETURNED VALUES
*
      ixargs = argout(1)
      iyargs = argout(2)
      ixf =(argout(3).and.'FF'x)+1
      iyf = ixf
      exit=(argout(4).ne.0)
 
*
* UPDATE THE DATABASE ZOOM INFORMATION
*
      call asp_itodz('ZXF',ixf,value,status)
      call asp_itodz('ZYF',iyf,value,status)
      call asp_itodz('ZXC',ixargs,value,status)
      call asp_itodz('ZYC',iyargs,value,status)
      call args_wrpar('DISPZOOM',value,1,status)
 
*
* SWITCH OFF LAMPS
*
      call args_lamps(0,0,0,0)
 
*
* DISABLE CURSOR
*
      call args_curs('0')
      call args_vsrrst
      call srsend
 
*
* TRANSFORM TO USER COORDINATES, IF THEY ARE DEFINED
*
 
      if(ierr.eq.0)then
         call args_dopdb ('ARGS_DEVICE',status)
         call args_atou (idmax,ixargs,iyargs,ux,uy,status)
         call args_cldb (status)
 
      else
         ux=ixargs
         uy=iyargs
      endif
 
 
      end
 
 
 
