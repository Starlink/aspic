      subroutine binit(ia,inval,npix,nlin,ic,npixo,nlino,xbin,ybin,
     :minpix,fval,ilevel,ierr)
 
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO PRODUCE A BINNED DOWN VERSION OF THE INPUT IMAGE
*
*METHOD
*       SCAN THROUGH OUTPUT IMAGE SUMMING ALL THE PIXELS IN THE INPUT
*       IMAGE THAT LIE WITHIN THAT BIN.CHECK TO SEE THAT THE BIN
*       CONTAINS SUFFICIENT VALID PIXELS.IF IT DOES DIVIDE THE SUM OF
*       THE INPUT PIXELS BY THE NUMBER OF GOOD PIXELS SUMMED TO GET
*       THERE AVERAGE.IF THERE ARE INSUFFICIENT VALID PIXELS IN THE
*       OUTPUT BIN,SET THAT OUTPUT PIXEL INVALID.
*
*ARGUMENTS
*       IA (IN)
*       INTEGER*2(NPIX,NLIN)
*              THE INPUT IMAGE
*       INVAL (IN)
*       INTEGER
*              THE FLAG FOR INVALID PIXELS
*       NPIX,NLIN (IN)
*       INTEGER
*              THE DIMENSIONS OF IA
*       IC (OUT)
*       INTEGER*2(NPIXO,NLINO)
*              THE OUTPUT IMAGE
*       NPIXO,NLINO (IN)
*       INTEGER
*              THE DIMENSIONS OF IC
*       XBIN (IN)
*       INTEGER
*              THE NUMBER OF PIXELS ACROSS A BIN IN THE X DIRECTION
*       YBIN (IN)
*       INTEGER
*              THE NUMBER OF PIXELS DOWN A BIN IN THE Y DIRECTION
*       MINPIX (IN)
*       INTEGER
*              THE ABSOLUTE MINIMUM NUMBER OF VALID PIXELS PER OUTPUT
*              PIXEL
*       FVAL (IN)
*       REAL
*              THE MINIMUM FRACTION OF VALID PIXELS PER OUTPUT PIXEL
*       ILEVEL (IN)
*       INTEGER
*              THE INTERACTION LEVEL
*       IERR (OUT)
*       INTEGER
*              ERROR FLAG,ZERO FOR SUCCESS
*
*CALLS
*       EDRS:
*              LBGONE
*       STARLINK:
*              WRUSER
*
*NOTES
*       USES INTEGER*2 ARRAYS
*
*WRITTEN BY
*       D.S. BERRY
*----------------------------------------------------------------------
*
*
      integer xbin,ybin,minpix
      integer*2 ia(npix,nlin),ic(npixo,nlino)
      character prbuf*80
 
*
* SET ERROR RETURN FLAG OFF
*
      ierr=0
 
*
* INITIALISE NTOT,TO COUNT NO. OF VALID PIXELS IN INPUT IMAGE,AND
* NBAD,TO COUNT NO. OF INVALID PIXELS IN OUTPUT IMAGE
*
      ntot=0
      nbad=0
 
*
* SCAN THROUGH OUTPUT IMAGE
*
 
      do 40 i=1,nlino
 
         do 30 j=1,npixo
 
*
* INITIALISE THIS OUTPUT PIXEL,AND N,TO COUNT THE NO. OF GOOD PIXELS
* IN IT
*
            sum=0
            n=0
 
*
* SCAN THE AREA OF INPUT IMAGE TO BE BINNED DOWN TO FORM THIS
* OUTPUT PIXEL
*
 
            do 20 k=(i-1)*ybin+1,i*ybin
 
               do 10 l=(j-1)*xbin+1,j*xbin
 
*
* IF THIS INPUT PIXEL IS INVALID,DO NOT INCLUDE IT
*
 
                  if(ia(l,k).ne.inval) then
 
*
* INPUT PIXEL IS OK,ADD TO OUTPUT PIXEL AND INCREMENT GOOD PIXEL COUNT
*
                     sum=sum+ia(l,k)
                     n=n+1
                  endif
 
 
*
* GO ROUND AGAIN WITH NEXT PIXEL ON THIS LINE OF INPUT
*
10             continue
 
 
*
* GO ROUND AGAIN WITH NEXT LINE OF INPUT
*
20          continue
 
 
*
* CHECK THAT OUTPUT PIXEL HAS SUFFICIENT GOOD INPUT PIXELS IN IT
*
            rval=fval*xbin*ybin
            nval=rval
 
            if(nval.ne.rval) nval=nval+1
 
            if((n.lt.minpix).or.(n.lt.nval)) then
               ic(j,i)=inval
 
*
* INCREMENT BAD OUTPUT PIXEL COUNT
*
               nbad=nbad+1
 
            else
               ic(j,i)=sum/real(n)
            endif
 
 
*
* INCREMENT TOTAL GOOD INPUT PIXEL COUNT
*
            ntot=ntot+n
 
*
* GO ROUND AGAIN WITH NEXT PIXEL IN THIS LINE OF THE OUTPUT
*
30       continue
 
 
*
* GO ROUND AGAIN WITH NEXT LINE OF OUTPUT
*
40    continue
 
 
*
* IF THERE WERE NO GOOD PIXELS IN INPUT IMAGE,END
*
 
      if(ntot.eq.0) then
         ierr=1
         goto 99
 
      endif
 
 
*
* IF THERE ARE NO GOOD PIXELS IN OUTPUT IMAGE,END
*
 
      if(nbad.eq.npixo*nlino) then
         ierr=2
         goto 99
 
      endif
 
 
*
* IF REQUIRED,WRITE OUT INFO
*
 
      if(ilevel.ge.2) then
         write(prbuf,50) nbad
50       format(1x,i6,' OUTPUT PIXELS CONTAINED INSUFFICIENT DATA')
         call lbgone(prbuf(50:))
         call wruser(' ',istat)
         call wruser(prbuf,istat)
         call wruser(' ',istat)
      endif
 
 
*
* FINISH
*
99    return
 
      end
 
 
 
