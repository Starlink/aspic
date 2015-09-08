      subroutine trcout(name,c,n,istat)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO FIT SIX REAL NUMBERS INTO LESS THAN 63 CHARACTERS SO THAT
*       THEY MAY BE USED TOGETHER AS AN OUTPUT PROGRAM PARAMETER.
*       MAXIMUM PRECISION IS MAINTAINED.
*
*METHOD
*       WRITE EACH NUMBER TO A CHARACTER STRING. ADD THE EXPONENTIAL
*       IF REQUIRED, KEEPING UNNECESSARY CHARACTERS TO A MINIMUM. WRITE
*       TO ENVIRONMENT AS AN ARRAY OF CHARACTER VALUES.
*
*ARGUMENTS
*       NAME (IN)
*       CHARACTER*(*)
*               NAME OF PROGRAM PARAMETER FOR OUTPUT
*       C (IN)
*       REAL(N)
*               UP TO 6 NUMBERS TO BE OUTPUT
*       N (IN)
*       INTEGER
*               A NUMBER NOT MORE THAN 6, THE NUMBER OF OUTPUT VALUES
*       ISTAT (OUT)
*       INTEGER
*               STATUS RETURN FROM WRKEYC
*
*STARLINK PARAMETERS
*       'NAME'
*               THE PARAMETER SPECIFIED IN THE ARGUMENT 'NAME' IS
*               ASSIGNED A NEW VALUE BY THE ROUTINE
*
*CALLS
*       THIS PACKAGE:
*               LBGONE
*       STARLINK:
*               WRKEYC
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      real c(n)
      character name*(*),buf(6)*10,ebuf*9,bufout(6)*9
 
*
* FORMAT UP TO 6 NUMBERS
*
 
      do 99 i=1,min(n,6)
 
*
* FIND THE EXPONENT FROM THE LOG, ENSURING DOWNWARD ROUNDING
*
 
         if(abs(c(i)).gt.1.0e-20) then
            nexp=int(log10(abs(c(i)))+100.0)-100
 
*
* CONVERT TO THE RANGE 1.000 TO 9.9999..
*
            d=c(i)/(10.0**nexp)
 
         else
            nexp=0
            d=0.0
         endif
 
 
*
* WRITE INTO BUFFER, THEN LEFT JUSTIFY IF NO SIGN PRESENT
*
         write(buf(i),1) d
1        format(ss,f10.7)
         call lbgone(buf(i))
 
*
* IF THE EXPONENT IS NOT ZERO, WRITE TO A SEPARATE BUFFER
*
 
         if(nexp.ne.0) then
            write(ebuf,2) nexp
2           format(ss,i9)
 
*
* COPY EXPONENT INTO OUTPUT BUFFER AT LEAST SIGNIFICANT END
*
 
            do 4 j=9,1,-1
 
               if(ebuf(j:j).ne.' ') then
                  buf(i)(j:j)=ebuf(j:j)
 
               else
 
*
* ADD 'E' IN FRONT OF EXPONENT
*
                  buf(i)(j:j)='E'
                  go to 5
 
               endif
 
4           continue
 
5           continue
         endif
 
         bufout(i)=buf(i)
99    continue
 
 
*
* WRITE RESULTING CHARACTER ARRAY TO ENVIRONMENT
*
      call wrkeyc(name,bufout,min(n,6),istat)
 
      end
 
 
 
