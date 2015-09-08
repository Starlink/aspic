      subroutine binint(ia,npix,nlines,inval,idx,idy,x,y,z,w,nbin)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO BIN AN IMAGE INTO RECTANGULAR BINS
*
*METHOD
*       SCAN THROUGH THE BINS, AVERAGING THE VALID PIXELS IN EACH BIN.
*       FORM MEAN IMAGE VALUE,X AND Y POSITION FOR EACH BIN. FORM
*       WEIGHT PROPORTIONAL TO SQUARE ROOT OF NUMBER OF VALID PIXELS
*       IN EACH BIN.
*
*ARGUMENTS
*       IA (IN)
*       INTEGER*2(NPIX,NLINES)
*               INPUT IMAGE
*       NPIX,NLINES (IN)
*       INTEGER
*               IMAGE DIMENSIONS
*       INVAL (IN)
*       INTEGER
*               INVALID PIXEL FLAG
*       IDX,IDY (IN)
*       INTEGER
*               SIZE OF BINS IN PIXELS IN X AND Y DIRECTIONS
*       X,Y (OUT)
*       REAL(*)
*               LISTS OF MEAN X,Y POSITIONS FOR BINS
*       Z (OUT)
*       REAL(*)
*               MEAN VALUE FOR EACH BIN
*       W (OUT)
*       REAL(*)
*               WEIGHT FOR EACH BIN
*       NBIN (OUT)
*       INTEGER
*               NUMBER OF BINS CONTAINING 1 OR MORE VALID PIXELS
*
*CALLS
*       NONE
*
*NOTES
*       USES INTEGER*2 ARRAY
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      integer*2 ia(npix,nlines)
      double precision x(*),y(*),z(*),w(*)
 
*
* INITIALLISE COUNTER FOR NUMBER OF BINS CONTAINING VALID PIXELS
*
      nbin=0
 
*
* SCAN THROUGH BINS, CALCULATING THE MIN AND MAX X AND Y COORDS.
*
 
      do 6 jmin=1,nlines,idy
         jmax=min(jmin+idy-1,nlines)
 
         do 5 imin=1,npix,idx
            imax=min(imin+idx-1,npix)
 
*
* INITIALLISE SUMS FOR FINDING MEAN X,Y AND DATA WITHIN EACH BIN
*
            sum=0.0
            npt=0
            ix=0
            iy=0
 
*
* SCAN THROUGH THE PIXELS WITHIN THE BIN, FORMING SUMS OF VALID PIXELS
*
 
            do 4 j=jmin,jmax
 
               do 3 i=imin,imax
 
                  if(ia(i,j).ne.inval) then
                     sum=sum+ia(i,j)
                     npt=npt+1
                     ix=ix+i
                     iy=iy+j
                  endif
 
3              continue
 
4           continue
 
 
*
* IF THERE WAS AT LEAST 1 VALID PIXEL IN THE BIN, FORM MEAN VALUES
*
 
            if(npt.ge.1) then
               nbin=nbin+1
               x(nbin)=real(ix)/npt
               y(nbin)=real(iy)/npt
               z(nbin)=sum/npt
               w(nbin)=sqrt(real(npt))
            endif
 
5        continue
 
6     continue
 
      return
 
      end
 
 
 
