      subroutine rebin(ia,npixa,nlinea,invala,invalb,minx,maxx,miny
     : ,maxy,c,scale,mode,ib,npixb,nlineb,ierr)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO RESAMPLE AN IMAGE AT POSITIONS GIVEN BY A LINEAR
*       TRANSFORMATION OF THE OUTPUT PIXEL POSITIONS
*
*METHOD
*       SCAN THE OUTPUT IMAGE, TRANSFORMING THE PIXEL POSITIONS.
*       INTERPOLATE IN THE INPUT IMAGE TO DETERMINE THE IMAGE VALUE
*       AT THESE POINTS. THE ROUTINE USES NEAREST-NEIGHBOUR OR LINEAR
*       INTERPOLATION BETWEEN THE 4 NEAREST PIXELS
*
*ARGUMENTS
*       IA (IN)
*       INTEGER*2(NPIXA,NLINEA)
*               THE INPUT IMAGE
*       NPIXA,NLINEA (IN)
*       INTEGER
*               THE DIMENSIONS OF IA
*       INVALA (IN)
*       INTEGER
*               INVALID PIXEL FLAG FOR IA
*       INVALB (IN)
*       INTEGER
*               INVALID PIXEL FLAG FOR IB
*       MINX,MAXX,MINY,MAXY (IN)
*       INTEGER
*               RANGE OF OUTPUT PIXELS TO BE REPLACED
*       C (IN)
*       REAL(6)
*               COEFFICIENTS GIVING THE TRANSFORMATION FROM OUTPUT
*               POSITIONS TO INPUT POSITIONS
*       SCALE (OUT)
*       REAL
*               SCALE FACTOR WHICH IS NEEDED IF TOTAL IMAGE INTENSITY
*               IS TO BE CONSERVED
*       MODE (IN)
*       INTEGER
*               TYPE OF INTERPOLATION
*               1: NEAREST NEIGHBOUR
*               2: LINEAR
*               3: CONSTANT NOISE
*       IB (IN/OUT)
*       INTEGER*2(NPIXB,NLINEB)
*               OUTPUT IMAGE
*       NPIXB,NLINEB (IN)
*       INTEGER
*               DIMENSIONS OF IB
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
      integer*2 ia(npixa,nlinea),ib(npixb,nlineb)
      integer xmin,xmax,ymin,ymax,x,y,xcen,ycen
      real c(6),wx(-1:+1),wy(-1:+1)
 
*
* SET UP WEIGHT ARRAYS FOR CONSTANT NOISE INTERPOLATION
*
      data wx(0)/1.0/,wy(0)/1.0/
 
*
* CHECK ARGUMENT VALIDITY
*
 
      if(minx.gt.maxx) then
         ierr=1
 
      else if(miny.gt.maxy) then
         ierr=2
 
      else
         ierr=0
 
*
* RESTRICT MAX AND MIN X,Y LIMITS TO LIE IN OUTPUT IMAGE
*
         xmin=min(max(1,minx),npixb)
         xmax=min(max(1,maxx),npixb)
         ymin=min(max(1,miny),nlineb)
         ymax=min(max(1,maxy),nlineb)
 
*
* RESTRICT INTERPOLATION METHOD TO BE 1 TO 3
*
         method=min(max(1,mode),3)
 
*
* SCALE FACTOR TO CONSERVE COUNTS IS (ABS. VALUE OF DETERMINANT)
*
         scale=abs(c(2)*c(6)-c(3)*c(5))
 
*
* SCAN THE SELECTED AREA OF THE OUTPUT IMAGE (LOCATION X,Y) AND
* CALCULATE THE TRANSFORMED POSITION (XDASH,YDASH) IN THE INPUT
* IMAGE
*
 
         do 12 y=ymin,ymax
            xref=c(1)+c(3)*y
            yref=c(4)+c(6)*y
 
            do 11 x=xmin,xmax
               xdash=xref+c(2)*x
               ydash=yref+c(5)*x
 
*
* FIND NEAREST PIXEL LOCATION
*
               xcen=nint(xdash)
               ycen=nint(ydash)
 
*
* IF NEAREST PIXEL LIES OUTSIDE INPUT IMAGE, OUTPUT PIXEL IS INVALID
* OTHERWISE CONTINUE WITH INTERPOLATION
*
 
               if((xcen.lt.1).or.(xcen.gt.npixa).or.(ycen.lt.1).or.
     :         (ycen.gt.nlinea)) then
                  ib(x,y)=invalb
 
               else
 
*
* FOR NEAREST-NEIGHBOUR INTERPOLATION, OUTPUT PIXEL=NEAREST PIXEL,
* -----------------------------------
* OR IS INVALID IF INPUT PIXEL IS INVALID
*
 
                  if(method.eq.1) then
 
                     if(ia(xcen,ycen).eq.invala) then
                        ib(x,y)=invalb
 
                     else
                        ib(x,y)=ia(xcen,ycen)
                     endif
 
 
*
* FOR LINEAR INTERPOLATION, OUTPUT PIXEL IS INVALID IF NEAREST INPUT
* ------------------------
* PIXEL IS INVALID. OTHERWISE CONTINUE WITH INTERPOLATION
*
 
                  else if(method.eq.2) then
 
                     if(ia(xcen,ycen).eq.invala) then
                        ib(x,y)=invalb
 
                     else
 
*
* FIND SHIFT FROM NEXT LOWEST PIXEL,LINE LOCATION
*
                        i=xdash
                        j=ydash
                        dx=xdash-i
                        dy=ydash-j
 
*
* INITIALLISE SUMS FOR FORMING WEIGHTED MEAN
*
                        sum=0.0
                        wtsum=0.0
 
*
* FORM WEIGHTED MEAN OF ADJACENT 4 PIXELS, CHECKING THAT EACH LIES
* WITHIN THE INPUT IMAGE AND IS NOT INVALID
*
 
                        if(j.ge.1) then
 
                           if(i.ge.1) then
 
                              if(ia(i,j).ne.invala) then
 
*
* WEIGHT IS CALCULATED FROM THE X,Y SHIFT FROM INTEGER PIXEL LOCATIONS
*
                                 wt=(1.0-dx)*(1.0-dy)
                                 sum=sum+ia(i,j)*wt
                                 wtsum=wtsum+wt
                              endif
 
                           endif
 
 
                           if(i+1.le.npixa) then
 
                              if(ia(i+1,j).ne.invala) then
                                 wt=dx*(1.0-dy)
                                 sum=sum+ia(i+1,j)*wt
                                 wtsum=wtsum+wt
                              endif
 
                           endif
 
                        endif
 
 
                        if(j+1.le.nlinea) then
 
                           if(i.ge.1) then
 
                              if(ia(i,j+1).ne.invala) then
                                 wt=(1.0-dx)*dy
                                 sum=sum+ia(i,j+1)*wt
                                 wtsum=wtsum+wt
                              endif
 
                           endif
 
 
                           if(i+1.le.npixa) then
 
                              if(ia(i+1,j+1).ne.invala) then
                                 wt=dx*dy
                                 sum=sum+ia(i+1,j+1)*wt
                                 wtsum=wtsum+wt
                              endif
 
                           endif
 
                        endif
 
 
*
* ASSIGN WEIGHTED MEAN TO OUTPUT PIXEL (WTSUM CANNOT BE ZERO, SINCE
* AT LEAST 1 INPUT PIXEL MUST BE VALID)
*
                        ib(x,y)=nint(sum/wtsum)
                     endif
 
 
*
* FOR CONSTANT NOISE INTERPOLATION (OUTPUT NOISE INDEPENDENT OF
* --------------------------------
* RESAMPLING PHASE)
*
 
                  else if(method.eq.3) then
 
*
* IF NEAREST PIXEL IS INVALID, SO IS OUTPUT PIXEL. OTHERWISE CONTINUE
* WITH INTERPOLATION
*
 
                     if(ia(xcen,ycen).eq.invala) then
                        ib(x,y)=invalb
 
                     else
 
*
* CALCULATE THE SHIFT FROM THE NEAREST PIXEL,LINE POSITION
*
                        dx=xdash-xcen
                        dy=ydash-ycen
 
*
* CALCULATE THE X AND Y WEIGHT ARRAYS (DEPENDENT ON THE PHASE DX,DY)
*
                        r1=dx*dx+0.25
                        r2=dy*dy+0.25
                        wx(-1)=r1-dx
                        wx(1)=r1+dx
                        wy(-1)=r2-dy
                        wy(1)=r2+dy
 
*
* NOW SCAN THE 9 NEAREST PIXELS, FORMING A WEIGHTED SUM OF ALL THE
* VALID ONES
*
                        sum=0.0
                        wtsum=0.0
 
                        do 22 jshift=-1,1
                           jj=ycen+jshift
 
*
* CHECK WE ARE STILL IN THE IMAGE
*
 
                           if(jj.ge.1.and.jj.le.nlinea) then
 
                              do 21 ishift=-1,1
                                 ii=xcen+ishift
 
                                 if(ii.ge.1.and.ii.le.npixa) then
 
*
* INCLUDE THE PIXEL IF IT IS VALID
*
 
                                    if(ia(ii,jj).ne.invala) then
                                       wt=wx(ishift)*wy(jshift)
                                       sum=sum+ia(ii,jj)*wt
                                       wtsum=wtsum+wt
                                    endif
 
                                 endif
 
21                            continue
 
                           endif
 
22                      continue
 
 
*
* ASSIGN THE INTERPOLATED VALUE TO THE OUTPUT PIXEL
*
                        ib(x,y)=nint(sum/wtsum)
                     endif
 
                  endif
 
               endif
 
11          continue
 
12       continue
 
      endif
 
      return
 
      end
 
 
 
