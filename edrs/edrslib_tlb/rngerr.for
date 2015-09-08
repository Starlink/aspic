      subroutine rngerr(name,type,botlim,toplim)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO PRINT MESSAGES SHOWING THE CORRECT DATA RANGE FOR A PARAMETER
*       AFTER THE USER HAS ENTERED AN INVALID VALUE
*
*METHOD
*       THE ROUTINE DISTINGUISHES BETWEEN INTEGER AND REAL PARAMETERS
*       AND ALLOWED AND EXCLUDED DATA RANGES, AND PRINTS AN APPROPRIATE
*       MESSAGE
*
*ARGUMENTS
*       NAME (IN)
*       CHARACTER*(*)
*               A NAME FOR THE PARAMETER, TO BE USED IN THE MESSAGE
*       TYPE (IN)
*       CHARACTER*(*)
*               'REAL' OR 'INTEGER', SPECIFIES THE PARAMETER TYPE
*       BOTLIM,TOPLIM (IN)
*       REAL
*               THE DATA LIMITS. IF BOTLIM.LE.TOPLIM, IT IS AN ALLOWED
*               RANGE. OTHERWISE IT IS AN EXCLUDED RANGE.
*
*CALLS
*       THIS PACKAGE:
*               LBGONE
*       STARLINK:
*               WRUSER
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      character name*(*),type*(*),errmsg*80
 
*
* IF PARAMETER IS A REAL VALUE
* ----------------------------
*
 
      if(type.eq.'REAL')then
 
*
* IF LIMITS DEFINE A PERMITTED RANGE OF VALUES
*
 
         if(toplim.ge.botlim) then
            write(errmsg,11)botlim,toplim
11          format(' SHOULD LIE IN THE RANGE ',ss,g14.4,' TO ',ss,g14
     :       .4)
            call lbgone(errmsg(44:))
            call lbgone(errmsg(37:))
            call lbgone(errmsg(26:))
 
         else
 
*
* IF LIMITS DEFINE AN EXCLUDED RANGE OF VALUES
*
            write(errmsg,12)toplim,botlim
12          format(' SHOULD NOT LIE IN THE RANGE ',ss,g14.4,' TO ',ss
     :       ,g14.4)
            call lbgone(errmsg(48:))
            call lbgone(errmsg(41:))
            call lbgone(errmsg(30:))
         endif
 
 
      else
 
*
* IF THE PARAMETER IS AN INTEGER VALUE
* ------------------------------------
*
* IF THE LIMITS DEFINE A PERMITTED RANGE OF VALUES
*
 
         if(toplim.ge.botlim) then
            write(errmsg,13)nint(botlim),nint(toplim)
13          format(' SHOULD LIE IN THE RANGE ',ss,i14,' TO ',ss,i14)
            call lbgone(errmsg(44:))
            call lbgone(errmsg(26:))
 
         else
 
*
* IF LIMITS DEFINE AN EXCLUDED RANGE OF VALUES
*
            write(errmsg,14)nint(toplim),nint(botlim)
14          format(' SHOULD NOT LIE IN THE RANGE ',ss,i14,' TO ',ss
     :       ,i14)
            call lbgone(errmsg(48:))
            call lbgone(errmsg(30:))
         endif
 
      endif
 
 
*
* PRINT THE ERROR MESSAGE PRODUCED
*
      call wruser(' '//name//errmsg,istat)
      return
 
      end
 
 
 
