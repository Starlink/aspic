      subroutine memcrd
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Finds what the user wants MEMCRDD to do and calls a lower
*	level routine to do it.
*
*SOURCE
*       MEMCRD.FOR in MEMCRDD.TLB
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              gtstrn,ctrlc
*       THIS PACKAGE (MEMCRDD.TLB):
*              memcz1,memcz2,memcz3,memcz4,memcz5,memcz6
*       INTERIM:
*              frdata
*
*STARLINK PARAMETERS
*	FUNCTION- Function to be performed by MEMCRDD
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*	control-c handling
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 27/9/89
*-------------------------------------------------------------------
      implicit none

*
* DECLARE LOCAL VARIABLES
*
      character fun*8	! Function to be performed by MEMCRDD
      integer	istat	! Temporary status value
      integer	ltext	! Length of user-selected text string
      integer	ncomm	! Position of selected string within option list

*
* SET UP CONTROL-C HANDLING, FOR USE WITH INTERACTIVE PROCESSES
*
      call ctrlc

*
* SEE WHAT THE USER WANTS THE PROGRAM TO DO (DEFAULT IS TO PRODUCE A
* HIGH RESOLUTION IMAGE)
*
      ncomm=1
      call gtstrn('FUNCTION',.true.,'HIRES,CONTINUE,ANALYSE,LORES,'//
     :            'SIMULATE,DESTRIPE.',1,ncomm,fun,ltext,istat)

*
* IF USER WANTS TO CONTINUE A PREVIOUS RUN, CALL ROUTINE MEMCZ1
*
      if(fun.eq.'CONTINUE') then
         call memcz1

*
* IF USER WANTS TO ANALYSE A PREVIOUS RUN (I.E. DO APERTURE PHOTOMETRY
* ON A CLASSIC HI-RES IMAGE), CALL ROUTINE MEMCZ2
*      
      else if(fun.eq.'ANALYSE') then
         call memcz2

*
* IF USER WANTS TO PRODUCE SIMULATED DATA, CALL ROUTINE MEMCZ3
*
      else if(fun.eq.'SIMULATE') then
	  call memcz3

*
* IF USER WANTS TO PRODUCE A LOW RESOLUTION IMAGE CALL MEMCZ4
*
      else if(fun.eq.'LORES') then
	  call memcz4

*
* IF USER WANTS TO PRODUCE A HIGH RESOLUTION IMAGE CALL MEMCZ5
*
      else if(fun.eq.'HIRES') then
         call memcz5

*
* IF USER WANTS TO PRODUCE DESTRIPED DATA, CALL MEMCZ6
*
      else if(fun.eq.'DESTRIPE') then
         call memcz6

      endif


*
* RELEASE ALL DATA AREAS AND FINISH
*
      call frdata(' ',istat)

      end
