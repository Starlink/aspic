*    ARGSCOM
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Holds zoom status information from the ARGS,and also the ARGS
*	database id of the last image displayed on the ARGS. Written by
*	routine ARGSOP, read by routine ARGSCL. These routines are 
*	called from the routine XYCUR which is the main	routine of 
*	program XYCUR.
*--------------------------------------------------------------------
*
      integer ixc,iyc,ixf,iyf,imlast
      character value*80
      common /argscom/ ixc,iyc,ixf,iyf,value,imlast
