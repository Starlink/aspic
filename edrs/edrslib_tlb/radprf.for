      subroutine radprf(ia,npix,nlines,inval,ilevel,sig0,axisr,theta
     : ,range,x,y,nxy,wt,device,fwhm,gamma,ierr)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO FIT A RADIAL PROFILE TO A SET OF STAR IMAGES
*
*METHOD
*       FOR EACH STAR, PUT THE DATA INTO BINS REPRESENTING ISOPHOTAL
*       ZONES , ALLOWING FOR IMAGE ELLIPTICITY. FORM A LINKED LIST
*       OF ALL THE PIXELS IN EACH ZONE, THEN PROCESS THE CONTENTS OF
*       EACH ZONE USING MODE TO REJECT ERRONEOUS DATA. FIT A GAUSSIAN
*       TO EACH BINNED STAR AND NORMALISE THE DATA TO UNIT CENTRAL
*       AMPLITUDE. PUT THE NORMALISED DATA INTO A SET OF BINS FILLED
*       WITH DATA FOR ALL STARS. CALL PRFFIT TO FIT A RADIAL PROFILE TO
*       THIS COMBINED DATA. PRINT THE RESULTS.
*       IF REQUIRED, PLOT THE MEAN PROFILE AND THE FITTED FUNCTION
*
*ARGUMENTS
*       IA (IN)
*       INTEGER*2(NPIX*NLINES)
*               THE INPUT IMAGE
*       NPIX,NLINES (IN)
*       INTEGER
*               DIMENSIONS OF IA
*       INVAL (IN)
*       INTEGER
*               INVALID PIXEL FLAG FOR IA
*       ILEVEL (IN)
*       INTEGER
*               INTERACTION LEVEL: CONTROLS PRINTING OF RESULTS
*       SIG0 (IN)
*       REAL
*               STAR 'SIGMA' ACROSS THE MINOR AXIS.
*       AXISR (IN)
*       REAL
*               THE STAR AXIS RATIO
*       THETA (IN)
*       REAL
*               THE INCLINATION OF THE STAR MAJOR AXIS TO THE X AXIS
*               IN RADIANS (X THROUGH Y POSITIVE)
*       RANGE (IN)
*       REAL
*               THE NUMBER OF STAR 'SIGMA'S OUT TO WHICH THE RADIAL
*               PROFILE IS FITTED
*       X,Y (IN)
*       REAL(NXY)
*               THE ACCURATE STAR CENTRE POSITIONS
*       NXY (IN)
*       INTEGER
*               THE NUMBER OF STARS
*       WT (IN)
*       REAL(NXY)
*               POSITIVE FOR STARS TO BE USED, ZERO FOR THOSE TO BE
*               OMITTED
*       DEVICE (IN)
*       CHARACTER
*               SGS WORKSTATION IDENTIFIER, SELECTS 'NONE' OR THE 
*		OUTPUT GRAPHICS DEVICE
*       FWHM (OUT)
*       REAL
*               AN OUTPUT ESTIMATE OF THE FULL WIDTH AT HALF MAXIMUM
*               ACROSS THE MINOR AXIS OF A STAR IMAGE
*       GAMMA (OUT)
*       REAL
*               STAR RADIAL PROFILE PARAMETER
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG: ZERO FOR SUCCESS
*
*CALLS
*       THIS PACKAGE:
*               MODE,PRFFIT,WRUSER,NCROPN,NCRBCK,DREBAR
*       NCAR:
*               AGCURV
*	SGS:
*		SGS_CLOSE
*
*NOTES
*       USES INTEGER*2 ARRAYS AND SUBROUTINE NAMES LONGER THAN 6
*       CHARACTERS
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
*
* SET MAXIMUM RADIUS TO BE CONSIDERED FOR THE RADIAL PROFILE
*
* SET RESOLUTION (NO. OF BINS PER PIXEL SPACING) FOR BINNING THE RADIAL
* PROFILES OF EACH STAR (RESOL1) AND THE MEAN PROFILE (RESOL2)
*
      parameter (maxrad=50,maxpts=(2*maxrad+1)**2,resol1=2.5,resol2=10
     : .0,nbin1=maxrad*resol1+1.0,nbin2=maxrad*resol2+1.0)
 
* SET NUMBER OF RADIAL BINS REQUIRED
      parameter (pbad=0.01,niter=10,nitfit=20,tolfit=0.0005)
 
*
* DIMENSION ARRAYS...NOTE IMAGE IS TREATED AS A 1 DIMENSIONAL ARRAY
*
      integer*2 ia(npix*nlines)
      real x(nxy),y(nxy),wt(nxy),data(maxpts),profil(0:nbin2),
     :profwt(0:nbin2),profr(0:nbin2),r(0:nbin1),dmode(0:nbin1)
      integer lstart(0:nbin1),nxtadr(maxpts),iadr(maxpts),npts(0:nbin1)
     : ,bin,star,bin2
      double precision sumw,sumf,sumf2,sumdf,sumd
      character text*1,prbuf*80,device*(*)
 
*
* CALCULATE CONSTANTS FOR RELATING RADIAL DISTANCE IN AN ELLIPTICAL
* STAR IMAGE TO EFFECTIVE DISTANCE ALONG THE MINOR AXIS
*
      ierr=0
      s=sin(theta)
      c=cos(theta)
      raxisr=(1.0/axisr)**2
      consta=s*s+c*c*raxisr
      constb=c*c+s*s*raxisr
      constc=2.0*c*s*(raxisr-1.0)
 
*
* CALCULATE SCALE FACTORS FOR CONVERTING RADIAL DISTANCE TO BINS
*
      rsig=1.0/sig0
      rscale=resol1*2.0/min(2.0,sig0)
      rscl2=resol2*2.0/min(2.0,sig0)
 
*
* FIND THE RADIUS LIMIT IN THE MINOR AXIS DIRECTION IMPOSED BY
* THE FITTING LIMIT 'RANGE'
*
      rlimit=range*sig0
 
*
* FIND THE SIZE OF SQUARE TO BE SCANNED AROUND EACH STAR TO
* ACCOMMODATE THE RADIAL FITTING RANGE
*
      ishift=min(max(1,nint(rlimit*axisr)),maxrad)
 
*
* FIND THE NUMBER OF BINS TO BE USED IN BINNING THE RADIAL PROFILES
* OF EACH STAR AND OF THE MEAN PROFILE
*
      mxbin1=min(nbin1,int(rlimit*rscale))
      mxbin2=min(nbin2,int(rlimit*rscl2))
 
*
* INITIALLISE THE ARRAYS TO HOLD THE MEAN RADIAL PROFILE
*
 
      do 41 bin=0,nbin2
         profr(bin)=0.0
         profil(bin)=0.0
         profwt(bin)=0.0
41    continue
 
 
*
* CONSIDER EACH STAR WHICH HAS POSITIVE WEIGHT
*
 
      do 991 star=1,nxy
 
         if(wt(star).gt.1.0e-10) then
 
*
* FIND THE EDGES OF THE SEARCH SQUARE CENTRED ON THE STAR
*
            i0=nint(min(max(-1.0e8,x(star)),1.0e8))
            j0=nint(min(max(-1.0e8,y(star)),1.0e8))
            imin=max(1,i0-ishift)
            imax=min(npix,i0+ishift)
            jmin=max(1,j0-ishift)
            jmax=min(nlines,j0+ishift)
 
*
* INITIALLISE ARRAYS TO POINT TO THE START OF A LINKED LIST OF ALL
* THE PIXELS IN A GIVEN RADIAL BIN
* AND TO FORM MEAN RADII FOR EACH BIN
*
 
            do 37 bin=0,mxbin1
               r(bin)=0.0
               npts(bin)=0
               lstart(bin)=0
37          continue
 
 
*
* SCAN THE SQUARE AROUND THE STAR, CALCULATING THE X AND Y
* DISPLACEMENTS FROM THE CENTRE
*
            index=0
            dy=jmin-y(star)-1.0
            dx0=imin-x(star)-1.0
 
            do 97 j=jmin,jmax
               imlocn=(j-1)*npix+imin-1
               dy=dy+1.0
               dy2=dy*dy
               dx=dx0
 
               do 96 i=imin,imax
                  imlocn=imlocn+1
                  dx=dx+1.0
 
*
* IF THE PIXEL IS VALID, CALCULATE THE EFFECTIVE RADIUS
*
 
                  if(ia(imlocn).ne.inval) then
                     dx2=dx*dx
                     dxy=dx*dy
                     rdash=sqrt(consta*dx2+constb*dy2+constc*dxy)
 
*
* FIND THE RADIAL BIN
*
                     bin=rdash*rscale
 
                     if(bin.le.mxbin1.and.rdash.le.rlimit) then
 
*
* FORM SUMS FOR THE EFFECTIVE MEAN RADIUS
*
                        npts(bin)=npts(bin)+1
                        r(bin)=r(bin)+rdash
 
*
* FORM A LINKED LIST OF ALL THE PIXELS IN THIS RADIAL BIN
*
                        index=index+1
                        iadr(index)=imlocn
                        nxtadr(index)=lstart(bin)
                        lstart(bin)=index
                     endif
 
                  endif
 
96             continue
 
97          continue
 
 
*
* INITIALLISE SUMS FOR FORMING A LEAST SQUARES GAUSSIAN FIT TO THE
* STAR RADIAL PROFILE AND BACKGROUND
*
            sumw=0.0d0
            sumf=0.0d0
            sumf2=0.0d0
            sumdf=0.0d0
            sumd=0.0d0
 
*
* CONSIDER EACH BIN WITH ONE OR MORE POINTS IN IT
*
 
            do 100 bin=0,mxbin1
 
               if(lstart(bin).gt.0) then
 
*
* EXTRACT THE PIXELS IN THIS BIN FROM THE LINKED LIST AND STORE
* IN ARRAY 'DATA'
*
                  icount=0
                  index=lstart(bin)
 
144               if(index.gt.0) then
                     icount=icount+1
                     data(icount)=ia(iadr(index))
                     index=nxtadr(index)
                     go to 144
 
                  endif
 
 
*
* CALL MODE TO FIND THE MOST LIKELY VALUE IN THE BIN
*
                  call mode(data,icount,pbad,niter,0.1,dmode(bin),dsig)
 
*
* FORM THE MEAN EFFECTIVE RADIUS FOR THE BIN
*
                  r(bin)=r(bin)/npts(bin)
 
*
* FORM SUMS FOR FITTING A GAUSSIAN PROFILE
* USING THE NUMBER OF DATA POINTS AS A WEIGHT
*
                  funct=exp(-0.5*(r(bin)*rsig)**2)
                  fw=funct*npts(bin)
                  sumw=sumw+npts(bin)
                  sumf=sumf+fw
                  sumf2=sumf2+(funct*fw)
                  sumdf=sumdf+dmode(bin)*fw
                  sumd=sumd+dmode(bin)*npts(bin)
               endif
 
100         continue
 
 
*
* SOLVE THE NORMAL EQUATIONS FOR THE GAUSSIAN FIT
*
            det=sumf2*sumw-sumf*sumf
 
            if(det.ne.0.0) then
               amp=(sumw*sumdf-sumd*sumf)/det
               back=(sumf2*sumd-sumdf*sumf)/det
 
*
* INSERT THE STAR PROFILE INTO THE MEAN PROFILE BINS
*
 
               do 141 bin=0,mxbin1
 
                  if(lstart(bin).gt.0) then
                     bin2=r(bin)*rscl2
 
                     if(bin2.le.mxbin2) then
 
*
* USE THE NO. OF POINTS AND THE STAR AMPLITUDE AS A WEIGHT
*
                        profil(bin2)=profil(bin2)+(dmode(bin)-back)
     :                   *npts(bin)
                        profr(bin2)=profr(bin2)+r(bin)*amp*npts(bin)
                        profwt(bin2)=profwt(bin2)+amp*npts(bin)
                     endif
 
                  endif
 
141            continue
 
            endif
 
         endif
 
991   continue
 
 
*
* CALCULATE THE MEAN RADIAL PROFILE AND ASSOCIATED RADII FROM THE
* BINNED DATA
*
 
      do 167 bin=0,mxbin2
 
         if(profwt(bin).gt.0.0) then
            profil(bin)=profil(bin)/profwt(bin)
            profr(bin)=profr(bin)/profwt(bin)
         endif
 
167   continue
 
 
*
* SET INITIAL ESTIMATES OF PROFILE PARAMETERS AND CALL PRFFIT TO
* OBTAIN A FULL LEAST SQUARES FIT TO THE PROFILE
*
      amp=1.0
      back=0.0
      gamma=2.0
      sigma=sig0
      call prffit(profil,profwt,profr,mxbin2+1,nitfit,tolfit,amp,back
     : ,sigma,gamma,ierrf)
 
*
* IF FIT WAS NOT SUCCESSFUL, SET IERR=1
*
 
      if(ierrf.ne.0) then
         ierr=1
 
*
* OTHERWISE PRINT RESULTS IF ILEVEL.GE.2
*
 
      else
 
         if(ilevel.ge.2) then
 
*
* CALCULATE FULL WIDTH HALF MAXIMUM SEEING
*
            fwhm=2.0*sigma*(1.38629**(1.0/gamma))
            write(prbuf,21)fwhm
21          format('   FWHM SEEING=',ss,g11.4)
            call wruser(prbuf,istat)
            call wruser(' ',istat)
            write(prbuf,22)gamma
22          format('   GAMMA=',ss,g11.4)
            call wruser(prbuf,istat)
            call wruser(' ',istat)
         endif
 
 
*
* PLOT RESULTS, IF GRAPHICS IS REQUIRED
*
 
         if(device.ne.'NONE') then
 
*
* COMPRESS THE DATA ARRAYS TO REMOVE EMPTY BINS AND ALSO FIND MAX AND
* MIN VALUES TO BE PLOTTED IN X AND Y
*
            ndata=-1
 
            xmax=-1.0e32
            xmin=1.0e32
            ymax=-1.0e32
            ymin=1.0e32

            do 604 bin=0,mxbin2
 
               if(profwt(bin).gt.0.0) then
                  ndata=ndata+1
                  profil(ndata)=profil(bin)-back
                  profr(ndata)=profr(bin)
                  profwt(ndata)=0.0

                  xmax=max(xmax,profr(ndata))
                  xmin=min(xmin,profr(ndata))
                  ymax=max(ymax,profil(ndata)+profwt(ndata))
                  ymin=min(ymin,profil(ndata)-profwt(ndata))

               endif
 
604         continue
 
 
*
* PLOT AN ERROR BAR GRAPH OF THE DATA USING NCAR GRAPHICS PACKAGE
*
            call ncropn(device,.true.,ierr)
            if(ierr.ne.0) goto 99
            call ncrbck(xmin,xmax,ymin,ymax,'MEAN STAR PROFILE',
     :                  'RADIAL DISTANCE','INTENSITY')
            call drebar(profr,profil,profwt,ndata+1)
 
*
* CALCULATE THE FITTED PROFILE OVER THE DATA RANGE
*
            rmax=profr(ndata)
 
            do 605 bin=0,mxbin2
               radius=(rmax*bin)/mxbin2
               profr(bin)=radius
               profil(bin)=amp*exp(-0.5*((radius/max(0.001,sigma))*
     :          *gamma))
605         continue
 
 
*
* PLOT THE FITTED FUNCTION
*
            call agcurv(profr,1,profil,1,mxbin2+1,1)
            call sgs_close
         endif
 
      endif
 
99    return
 
      end
 
 
 
