      subroutine squash(ia,npixa,nlinea,invala,idx,idy,ib,npixb,nlineb
     : ,invalb)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO REDUCE THE SIZE OF AN IMAGE BY AVERAGING AN INTEGER NUMBER OF
*       PIXELS IN X AND Y DIRECTIONS
*
*METHOD
*       COUNT THROUGH THE OUTPUT PIXELS CALCULATING THE EXTENT OF THE
*       CONTRIBUTING BIN IN THE INPUT IMAGE. AVERAGE THE VALID INPUT
*       PIXELS IN EACH BIN
*
*ARGUMENTS
*       IA (IN)
*       INTEGER*2(NPIXA,NLINEA)
*               INPUT IMAGE
*       NPIXA,NLINEA (IN)
*       INTEGER
*               DIMENSIONS OF IA
*       INVALA (IN)
*       INTEGER
*               INVALID PIXEL FLAG FOR IA
*       IDX,IDY (IN)
*       INTEGER
*               NUMBER OF PIXELS TO AVERAGE IN X AND Y DIRECTIONS
*       IB (OUT)
*       INTEGER*2(NPIXB,NLINEB)
*               OUTPUT IMAGE
*       NPIXB,NLINEB (IN)
*       INTEGER
*               DIMENSIONS OF IB. MUST BE CORRECT SIZE FOR BINNED DOWN
*               IMAGE
*       INVALB (IN)
*       INTEGER
*               INVALID PIXEL FLAG FOR IB
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
      integer*2 ia(npixa,nlinea),ib(npixb,nlineb)
 
*
* SCAN THROUGH BINS IN LINE DIRECTION
*
 
      do 100 jmin=1,nlinea,idy
 
*
* CALCULATE BIN EXTENT
*
         jmax=min(nlinea,jmin+idy-1)
 
*
* CALCULATE PIXEL IN OUTPUT IMAGE
*
         jj=(jmin-1)/idy+1
 
*
* NOW SCAN THE PIXEL DIRECTION
*
 
         do 99 imin=1,npixa,idx
            imax=min(npixa,imin+idx-1)
            ii=(imin-1)/idx+1
 
*
* INITIALLISE SUMS FOR FINDING MEAN VALUE OF BIN
*
            npt=0
            isum=0
 
*
* SCAN BIN, FORMING SUMS
*
 
            do 89 j=jmin,jmax
 
               do 88 i=imin,imax
 
                  if(ia(i,j).ne.invala) then
                     npt=npt+1
                     isum=isum+ia(i,j)
                  endif
 
88             continue
 
89          continue
 
 
*
* CALCULATE OUTPUT VALUE IF POSSIBLE
*
 
            if(npt.gt.0) then
               ib(ii,jj)=nint(real(isum)/real(npt))
 
            else
 
*
* OTHERWISE OUTPUT PIXEL IS INVALID
*
               ib(ii,jj)=invalb
            endif
 
99       continue
 
100   continue
 
 
      end
 
 
 
