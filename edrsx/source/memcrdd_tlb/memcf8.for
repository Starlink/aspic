      subroutine memcf8(rate,test)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Changes the value of the RATE parameter used by MEMSYS3 
*	between iterations so that the value of parameter TEST is 
* 	kept (hopefully) within the range 0.05 to 0.5.
*	NB, rate is only changed if it is negative (the absolute value
*	is used). If RATE is positive on entry, then it is left as
*	it is.
*
*SOURCE
*       MEMCF8.FOR in MEMCRDD.TLB
*
*ARGUMENTS       
*   INPUT:
*	rate	real		The value of RATE used for the last
*				MEMSYS3 iteration.
*	test	real		The value of TEST produced by the last
*				MEMSYS3 iteration.
*   OUTPUTS:
*       rate	real		The value of RATE to use for the next
*				MEMSYS3 iteration.
*SUBROUTINES CALLED
*       none
*              
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 7/3/90
*-------------------------------------------------------------------
      implicit none

*
* INCLUDE GLOBAL PARAMETER DECLARATIONS
*
      include '(PR_DEC)'

*
* DECLARE ARGUMENTS
*
      real	test,rate

*
* DECLARE LOCAL VARIABLES
*
      real	f	! Fraction of distance to RATE limit to jump
      real	abrate	! Absolute value of RATE

*
* IF RATE IS POSITIVE ON ENTRY THEN DON'T CHANGE IT. OTHERWISE USE
* THE ABSOLUTE VALUE OF RATE.
*
      if(rate.gt.0.0) goto 999

      abrate=abs(rate)

*
* IF TEST IS LESS THAN 0.05 THEN THE MEM ALGORITHM IS CONVERGING
* UNNECESARILY SLOWLY. INCREASE RATE TO SPEED UP THE RATE OF CONVERGENCE
* (GO ONE THIRD OF THE WAY TOWARDS THE UPPER LIMIT OF RATE=1.0, EXCEPT
* WHEN TEST IS CLOSE TO 0.05)
*
      if(test.lt.0.05) then
         f=min(0.33333,1.0-test/0.05)
         abrate=abrate+f*(1.0-abrate)

*
* IF THE VALUE OF TEST PRODUCED BY THE LAST ITERATION WAS GREATER
* THAN 0.5 THEN THE ENTROPY MAXIMISATION IS GETTING DANGEROUSLY
* INACCURATE. REDUCE THE VALUE OF RATE TO MAKE SURE THE MAXIMISATION
* DOESN'T GET MUCH WORSE. (GO ONE THIRD OF THE WAY TOWARDS THE LOWER 
* LIMIT OF RATE=0.01, EXCEPT WHEN TEST IS CLOSE TO 1.0)
*
      else if(test.gt.0.5) then
         f=min(0.33333,test/0.5-1.0)
         abrate=abrate-f*(abrate-0.01)

      endif

      rate=-abrate

*
* FINISH
*
  999 continue

      end
