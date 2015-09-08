      integer function ophelp(c)
 
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO CONTROL THE FORMATTING OF HELP INFORMATION GIVEN TO THE USER
*       AT THE TERMINAL AND TO DETECT FAILURE TO LOCATE THE REQUIRED
*       HELP INFORMATION.
*
*METHOD
*       THIS ROUTINE IS CALLED VIA THE LIBRARY MANAGEMENT ROUTINE
*       LBR$OUTPUT_HELP, BUT COMMUNICATES WITH GTHELP VIA THE COMMON
*       BLOCK /HLPBL1/. THE PARAMETERS IN THIS BLOCK ARE USED TO:
*
*               1) COUNT THE NUMBER OF NON-BLANK OUTPUT LINES
*                  SO THAT INITIAL INFORMATION CAN BE SUPRESSED IF
*                  REQUIRED (NLOUT)
*               2) INDICATE THE DESIRABILITY OF TRIMMING LEADING BLANKS
*                  FROM OUTPUT LINES (TRIM)
*               3) RETURN STATUS INDICATING IF HELP INFORMATION WAS
*                  ACTUALLY FOUND (CANHLP)
*
*ARGUMENTS
*       OPHELP (FUNCTION NAME)
*       INTEGER
*               RETURNS A SUCCESS STATUS TO LBR$OUTPUT_HELP. THIS
*               INDICATES SUCCESS, UNLESS FAILURE TO FIND THE
*               INFORMATION IS DETECTED, IN WHICH CASE FAILURE IS
*               REPORTED, WHICH TERMINATES THE OUTPUT SEQUENCE.
*       C (IN)
*       CHARACTER*(*)
*               THE TEXT TO BE OUTPUT
*
*CALLS
*       STARLINK:
*               WRUSER
*
*NOTES
*       USES THE COMMON BLOCK /HLPBL1/ AND MAY CHANGE THE VALUES IN
*       THIS BLOCK
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*----------------------------------------------------------------------
*
*
* INCLUDE COMMON BLOCK FOR COMMUNICATION WITH GTHELP
*
      include 'HLPBL1COM.FOR'
 
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
         ophelp=ss$_normal
 
      else
 
*
* IF NON-BLANK, CHECK FOR MESSAGE INDICATING HELP INFORMATION WAS
* NOT FOUND
*
 
         if(index(c,'Sorry, no documentation').ne.0)then
 
*
* IF INFO. NOT AVAILABLE, RETURN WITH FAILURE FLAGS SET
*
            ophelp=ss$_normal+1
            canhlp=.false.
 
         else
 
*
* IF 'ADDITIONAL INFORMATION' LIST IS BEING OUTPUT, REDUCE
* TRIMMING OF LEADING BLANKS (WHICH WOULD RUIN THE FORMAT OF THE LIST)
*
 
            if(index(c,'Additional information available').ne.0)then
               call wruser(' ',istat)
               trim=.false.
            endif
 
 
*
* IF VALID OUTPUT LINE, COUNT IT AND CHECK THE INITIAL NUMBER OF
* SUPPRESSED LINES HAS BEEN PASSED BEFORE WRITING IT TO THE USER
*
            nlout=nlout+1
            ophelp=ss$_normal
 
            if(nlout.gt.0)then
 
*
* ADJUST TRIMMING OF LEADING BLANKS AS REQUIRED
*
 
               if(trim)then
                  call wruser(c(min(len(c),8):),istat)
 
               else
                  call wruser(c(min(len(c),5):),istat)
               endif
 
            endif
 
         endif
 
      endif
 
 
      end
 
 
 
