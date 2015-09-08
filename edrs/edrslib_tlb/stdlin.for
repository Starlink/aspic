      subroutine stdlin(itype,botlim,toplim,c,maxord,table,ntab,ierr)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO GENERATE LINEARITY CORRECTION TABLES USING STANDARD FORMULAE
*
*METHOD
*       EVALUATE THE FORMULAE AT REGULAR INTERVALS AND STORE IN A TABLE
*
*ARGUMENTS
*       ITYPE (IN)
*       INTEGER
*               THE TYPE OF LINEARITY CORRECTION REQUIRED
*                       1: POLYNOMIAL
*                       2: ELECTRONOGRAPHIC EMULSION SATURATION CORRN.
*       BOTLIM,TOPLIM (IN)
*       REAL
*               THE LOWER AND UPPER LIMITS FOR THE TABLE
*       C (IN)
*       REAL(MAXORD)
*               SET OF CONSTANTS FOR THE POLYNOMIAL FUNCTION
*               IF ITYPE=2, THE FIRST CONSTANT IS USED AS THE FILM
*               CONSTANT
*       MAXORD (IN)
*       INTEGER
*               NUMBER OF CONSTANTS C
*       TABLE (OUT)
*       REAL(NTAB)
*               THE CORRECTION TABLE
*       NTAB (IN)
*       INTEGER
*               THE NUMBER OF ENTRIES IN THE TABLE
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG: ZERO FOR SUCCESS
*
*CALLS
*       NONE
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      real table(ntab),c(maxord)
      ierr=0
 
*
* CHECK VALIDITY OF ARGUMENTS
*
 
      if(botlim.gt.toplim) then
         ierr=1
 
      else if(ntab.lt.2) then
         ierr=2
 
      else
 
*
* CALCULATE INTERVAL BETWEEN TABLE ENTRIES
*
         dintvl=(toplim-botlim)/(ntab-1)
 
*
* SCAN THROUGH TABLE ENTRIES
*
 
         do 1 iden=1,ntab
            d=botlim+(iden-1)*dintvl
 
*
* CALCULATE THE TABLE VALUE USING THE APPROPRIATE FORMULA
*
 
            if(itype.eq.1) then
               dout=c(maxord)
 
               do 11 j=maxord-1,1,-1
                  dout=dout*d+c(j)
11             continue
 
 
*
 
            else if(itype.eq.2) then
               dout=-c(1)*log(1.0-d/c(1))
            endif
 
            table(iden)=dout
1        continue
 
      endif
 
      return
 
      end
 
 
 
