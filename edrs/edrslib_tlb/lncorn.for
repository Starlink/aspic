      subroutine lncorn(ia,npix,nlines,invala,ascale,azero,botlim,
     :toplim,table,ntab,ib,invalb,bscale,bzero,ierr)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO APPLY A LINEARITY CORRECTION TO AN IMAGE FROM A LOOK-UP
*       TABLE
*
*METHOD
*       SCAN THE TABLE TO DETERMINE THE RANGE OF OUTPUT VALUES AND
*       CALCULATE OUTPUT SCALING TO PRESERVE ACCURACY IN THE OUTPUT.
*       APPLY THE CORRECTION BY INTERPOLATING LINEARLY BETWEEN THE
*       ENTRIES IN THE LOOK-UP TABLE
*
*ARGUMENTS
*       IA (IN)
*       INTEGER*2(NPIX,NLINES)
*               THE INPUT IMAGE
*       NPIX,NLINES (IN)
*       INTEGER
*               THE DIMENSIONS OF IA AND IB
*       INVALA (IN)
*       INTEGER
*               INVALID PIXEL FLAG FOR IA
*       ASCALE,AZERO (IN)
*       REAL
*               SCALE AND ZERO LEVEL FOR IA
*       BOTLIM,TOPLIM (IN)
*       REAL
*               THE INPUT DATA VALUES CORRESPONDING TO THE FIRST AND
*               LAST LOOK-UP TABLE ENTRIES
*       TABLE (IN)
*       REAL(NTAB)
*               THE LOOK-UP TABLE
*       NTAB (IN)
*       INTEGER
*               THE NUMBER OF ENTRIES IN TABLE (2 OR MORE)
*       IB (OUT)
*       INTEGER*2(NPIX,NLINES)
*               THE OUTPUT IMAGE
*       INVALB (IN)
*       INTEGER
*               INVALID PIXEL FLAG FOR IB
*       BSCALE,BZERO (OUT)
*       REAL
*               SCALE AND ZERO LEVEL FOR IB
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG: ZERO FOR SUCCESS
*
*CALLS
*       NONE
*
*NOTES
*       USES INTEGER*2 ARRAYS
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      integer*2 ia(npix,nlines),ib(npix,nlines)
      real table(ntab)
      parameter (minint=-32767,maxint=32767)
 
*
* CHECK ARGUMENT VALIDITY
*
      ierr=0
 
      if(toplim.lt.botlim) then
         ierr=1
 
      else if(ntab.lt.2) then
         ierr=2
 
      else
 
*
* CALCULATE DATA INTERVAL BETWEEN TABLE ENTRIES
*
         dintvl=max((toplim-botlim),1.0e-20)/(ntab-1)
         rdint=1.0/dintvl
 
*
* SCAN THE TABLE TO FIND THE POSSIBLE EXTREMES OF OUTPUT VALUES
*
         outmax=minint
         outmin=maxint
 
         do 1 iden=1,ntab
            outmax=max(outmax,table(iden))
            outmin=min(outmin,table(iden))
1        continue
 
 
*
* SET SCALING OF OUTPUT VALUES SO AS TO USE 0.75 OF THE AVAILABLE
* OUTPUT INTEGER RANGE
*
         bscale=max(1.0e-20,outmax-outmin)/(0.75*(maxint-minint))
         bzero=((outmax+outmin)-(maxint+minint))*0.5
         rbscl=1.0/bscale
 
*
* SCAN IMAGE TO MAKE THE ITF CORRECTION
*
 
         do 101 j=1,nlines
 
            do 100 i=1,npix
 
*
* OUTPUT PIXEL IS INVALID IF INPUT PIXEL IS INVALID
*
 
               if(ia(i,j).eq.invala) then
                  ib(i,j)=invalb
 
               else
 
*
* OTHERWISE CALCULATE INPUT DATA VALUE AND CHECK IT IS WITHIN THE
* TABLE RANGE. IF NOT..OUTPUT IS INVALID
*
                  d=ia(i,j)*ascale+azero
 
                  if(d.gt.toplim.or.d.lt.botlim) then
                     ib(i,j)=invalb
 
                  else
 
*
* IF OK, FIND ADJACENT TABLE ENTRIES AND INTERPOLATE LINEARLY BETWEEN
* THEM
*
                     tabloc=(d-botlim)*rdint+1.0
                     ntab1=int(tabloc)
                     ntab2=min(ntab1+1,ntab)
                     dtab=tabloc-ntab1
                     dout=table(ntab1)*(1.0-dtab)+table(ntab2)*dtab
 
*
* ASSIGN RESULT TO OUTPUT PIXEL, APPROPRIATELY SCALED
*
                     ib(i,j)=nint((dout-bzero)*rbscl)
                  endif
 
               endif
 
100         continue
 
101      continue
 
      endif
 
      return
 
      end
 
 
 
