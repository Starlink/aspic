      integer function opnull(c)

*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       A DUMMY ROUTINE WHICH DOES NOTHING. TO BE CALLED BY RUN TIME
*       LIBRARY ROUTINE LBR$OUTPUT_HELP WHEN NO HELP IS TO BE DISPLAYED.
*
*SOURCE
*       OPNULL.FOR IN UTILITIES.TLB
*
*ARGUMENTS
*       OPNULL (FUNCTION NAME)
*       INTEGER
*               RETURNS A SUCCESS STATUS TO LBR$OUTPUT_HELP. THIS
*               INDICATES SUCCESS, UNLESS FAILURE TO FIND THE
*               INFORMATION IS DETECTED, IN WHICH CASE FAILURE IS
*               REPORTED, WHICH TERMINATES THE OUTPUT SEQUENCE.
*       C (IN)
*       CHARACTER*(*)
*               THE TEXT TO BE OUTPUT (ACTUALLY IT IS JUST THROWN AWAY)
*
*CALLS
*       NONE
*NOTES
*       USES THE COMMON BLOCK /HLPBL1/ AND MAY CHANGE THE VALUES IN
*       THIS BLOCK
*
*WRITTEN BY
*       D.S. BERRY
*----------------------------------------------------------------------
*
*
* INCLUDE COMMON BLOCK FOR COMMUNICATION WITH GTHELP
*
      include 'UTILITIES(HLPBL1COM)'
*
* DEFINE SUCCESS STATUS CODE FOR LBR$OUTPUT_HELP
*
      parameter (ss$_normal=1)
*
* DECLARATIONS, ETC.
*
      character c*(*)
*
* INITIALLISE SUCCESS FLAG TO 'HELP FOUND'
*
      canhlp=.true.
*
* RETURN WITH NO ACTION TO SUPPRESS BLANK OUTPUT LINES
*
      if(c.eq.' ')then
         opnull=ss$_normal
      else
*
* IF NON-BLANK, CHECK FOR MESSAGE INDICATING HELP INFORMATION WAS
* NOT FOUND
*
         if(index(c,'Sorry, no documentation').ne.0)then
*
* IF INFO. NOT AVAILABLE, RETURN WITH FAILURE FLAGS SET
*
            opnull=ss$_normal+1
            canhlp=.false.
         endif
      endif

      end
