      subroutine clnsta(x,imin,imax,xcen)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO 'CLEAN' A MARGINAL STAR PROFILE, REMOVING NEIGHBOURING
*       STARS AND BLEMISHES
*
*METHOD
*       SUBTRACT LOWER QUARTILE DATA POINT AS A BACKGROUND ESTIMATE
*       WORK OUT FROM CENTRE OF STAR, PREVENTING EACH DATA POINT FROM
*       EXCEEDING THE MAXIMUM OF THE TWO PREVIOUS DATA VALUES. THIS
*       ENSURES THAT THE DATA DOES NOT INCREASE WITH INCREASING
*       DISTANCE FROM THE CENTRE.
*
*ARGUMENTS
*       X (IN/OUT)
*       REAL(IMIN:IMAX)
*               DATA ARRAY..INVALID VALUES ARE SET ABOVE 1.0E20
*       IMIN,IMAX (IN)
*       INTEGER
*               FIRST AND LAST COORDINATES OF DATA ARRAY.. ALSO
*               DEFINE THE DIMENSION OF X
*       XCEN (IN)
*       REAL
*               ESTIMATE OF POSITION OF STAR CENTRE
*
*CALLS
*       THIS PACKAGE:
*               NTHMIN
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      parameter (nmin=2)
      real x(imin:imax),stak(25),smin(nmin)
 
*
* CALCULATE THE NUMBER OF DATA POINTS AND FIND THE LOWER QUARTILE
* POINT AS A BACKGROUND ESTIMATE
*
      nx=imax-imin+1
      nquart=min(max(1,nint(nx*0.25)),25)
      call nthmin(x(imin),nx,nquart,stak,ierr)
 
*
* SUBTRACT THE BACKGROUND FROM ALL THE VALID DATA POINTS, FINDING
* THE MAXIMUM VALUE
*
      smax=-2.0e20
 
      do 17 i=imin,imax
 
         if(x(i).lt.1.0e20) then
            x(i)=x(i)-stak(1)
            smax=max(smax,x(i))
         endif
 
17    continue
 
 
*
* WORK OUT FROM THE CENTRE OF THE STAR IMAGE TOWARDS EACH END
* OF THE DATA ARRAY IN TURN
*
      i0=nint(xcen)
 
      do 61 idirn=-1,1,2
 
         if(idirn.lt.0) then
            iend=imin
 
         else
            iend=imax
         endif
 
 
*
* INITIALLISE THE STORE OF THE LAST NMIN DATA VALUES
*
 
         do 81 j=1,nmin
            smin(j)=smax
81       continue
 
         minloc=1
 
*
* WORK THROUGH THE DATA ARRAY, AT EACH POINT CALCULATING AN UPPER
* DATA LIMIT AS THE MAXIMUM OF THE LAST NMIN VALUES
*
 
         do 60 i=i0,iend,idirn
            uplim=smin(1)
 
            do 82 j=2,nmin
               uplim=max(uplim,smin(j))
82          continue
 
 
*
* LIMIT THE DATA TO THE CURRENT UPPER LIMIT
*
            x(i)=min(uplim,x(i))
 
*
* MINLOC CYCLES FROM 1 TO NMIN AND POINTS TO THE STORE FOR
* THIS VALUE IN THE LIST OF THE LAST NMIN VALUES
*
            minloc=mod(minloc,nmin)+1
 
            if(x(i).ge.0.0) then
               smin(minloc)=x(i)
 
            else
 
*
* DON'T USE NEGATIVE VALUES...REPLACE WITH THE CURRENT UPPER LIMIT
*
               smin(minloc)=uplim
            endif
 
60       continue
 
61    continue
 
      return
 
      end
 
 
 
