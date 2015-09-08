      subroutine sparam(ia,npix,nlines,inval,isize,range,id,x,y,nxy
     : ,ilevel,device,axisr,theta,fwhm,gamma,sig,ierr)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO FIND A SET OF PARAMETERS DESCRIBING A MODEL STAR IMAGE
*       FITTED TO A SET OF STAR IMAGES AND TO DISPLAY THE RESULTS.
*
*METHOD
*       CALL STARIM TO DETERMINE THE ELLIPTICITY OF THE STAR IMAGES
*       PRINT HEADINGS AND A TABLE OF THE ELLIPSE PARAMETERS FOR
*       EACH STAR USED. PRINT THE MEAN ELLIPSE PARAMETERS.
*       CALL RADPRF TO DETERMINE THE MODEL RADIAL STAR PROFILE AND TO
*       PRINT THE RESULTS.
*
*ARGUMENTS
*       IA (IN)
*       INTEGER*2(NPIX,NLINES)
*               THE INPUT IMAGE CONTAINING THE STARS TO BE FITTED
*       NPIX,NLINES (IN)
*       INTEGER
*               THE DIMENSIONS OF IA
*       INVAL (IN)
*       INTEGER
*               INVALID PIXEL FLAG FOR IA
*       ISIZE (IN)
*       INTEGER
*               LENGTH OF THE SEARCH SQUARE SIDE USED IN FINDING STARS
*               AND CALCULATING THEIR ELLIPTICITY
*       RANGE (IN)
*       REAL
*               THE RADIUS IN UNITS OF THE STAR 'SIGMA' OUT TO WHICH
*               THE RADIAL PROFILE IS FITTED
*       ID (IN)
*       BYTE(20,NXY)
*               A LIST OF ASCII IDENTIFIERS FOR THE STARS
*       X,Y (IN)
*       REAL(NXY)
*               THE APPROXIMATE POSITIONS OF THE STARS TO BE FITTED
*       NXY (IN)
*       INTEGER
*               THE NUMBER OF STARS TO BE USED
*       ILEVEL (IN)
*       INTEGER
*               INTERACTION LEVEL: CONTROLS PRINTING OF RESULTS
*       DEVICE (IN)
*       CHARACTER
*               SGS WORKSTATION IDENTIFIER, SELECTS GRAPHICS DEVICE.
*       AXISR (OUT)
*       REAL
*               THE AXIS RATIO OF THE STAR IMAGES
*       THETA (OUT)
*       REAL
*               THE INCLINATION OF THE MAJOR AXIS OF THE STAR IMAGES TO
*               THE X DIRECTION IN RADIANS (X THROUGH Y POSITIVE)
*       FWHM (OUT)
*       REAL
*               THE FULL WIDTH AT HALF MAXIMUM OF THE STAR IMAGES IN
*               THE MINOR AXIS DIRECTION
*       GAMMA (OUT)
*       REAL
*               THE EXPONENT IN THE RADIAL STAR PROFILE
*       SIG (WORKSPACE)
*       REAL(NXY,5)
*               INTERMEDIATE STORAGE FOR THE WIDTHS OF THE STAR MARGINAL
*               PROFILES
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG: ZERO FOR SUCCESS
*
*CALLS
*       THIS PACKAGE:
*               STARIM,ELLIPS,LBGONE,RADPRF
*       STARLINK:
*               WRUSER
*
*NOTES
*       USES INTEGER*2 AND BYTE ARRAYS
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      integer*2 ia(npix,nlines)
      byte id(20,nxy)
      real x(nxy),y(nxy),sig(nxy,5),prf(4)
      character idchr*20,prbuf*80,device*(*)
 
*
* CALL STARIM TO DETERMINE THE MEAN ELLIPTICITY OF THE STAR IMAGES
*
      ierr=0
      call starim(ia,npix,nlines,inval,x,y,nxy,isize,sig0,axisr,theta
     : ,ngood,sig)
 
*
* IF NO STARS COULD BE FOUND TO DETERMINE THE ELLIPTICITY, SET IERR=1
* AND ABORT
*
 
      if(ngood.le.0) then
         ierr=1
         go to 99
 
      endif
 
 
*
* IF ILEVEL.GE.3 PRINT A TABLE OF THE AXIS RATIOS OF EACH STAR USED
*
 
      if(ilevel.ge.3) then
 
*
* PRINT HEADINGS
*
         call wruser(' ',istat)
         write(prbuf,10)
10       format(t52,'GAUSSIAN',t67,'AXIS RATIO')
         call wruser(prbuf,istat)
         write(prbuf,11)
11       format(5x,'IDENTIFIER',t24,' X COORD.',5x,' Y COORD.',4x,
     :   'FWHM SEEING',5x,'/ ANGLE (DEG)')
         call wruser(prbuf,istat)
         write(prbuf,12)
12       format(5x,'----------',t24,' --------',5x,' --------',4x,
     :   '-----------',5x,'-------------')
         call wruser(prbuf,istat)
 
*
* CONSIDER EACH STAR
*
 
         do 101 i=1,nxy
 
*
* EXTRACT ITS IDENTIFIER
*
 
            do 104 l=1,20
               nchar=id(l,i)
               idchr(l:l)=char(nchar)
104         continue
 
 
*
* IF THE WIDTHS OF THE MARGINAL PROFILES WERE FOUND, CALL ELLIPS
* TO FIND THE PARAMETERS SPECIFYING THE STAR SHAPE
*
 
            if(sig(i,5).gt.1.0e-10) then
               prf(1)=sig(i,1)
               prf(2)=sig(i,2)
               prf(3)=sig(i,3)
               prf(4)=sig(i,4)
               call ellips(prf,stasig,staaxs,stathe)
 
*
* CALCULATE THE FWHM SEEING DISK, ASSUMING A GAUSSIAN PROFILE
* AND PRINT THE STAR PARAMETERS
*
               seeing=stasig*2.35482
               angle=stathe*57.29578
               write(prbuf,13)idchr,x(i),y(i),seeing,staaxs
13             format(' ',a20,t23,2(ss,g13.6,1x),1x,ss,g10.3,6x,ss,g10
     :          .3)
               call wruser(prbuf,istat)
               write(prbuf,303) stathe*57.29578
303            format(66x,ss,g10.3)
               call wruser(prbuf,istat)
 
            else
 
*
* IF THE MARGINAL PROFILE WIDTHS WERE NOT FOUND, PRINT DETAILS
*
               write(prbuf,14)idchr,x(i),y(i)
14             format(' ',a20,t23,2(ss,g13.6,1x),3x,
     :         'CANNOT FIT THIS STAR')
               call wruser(prbuf,istat)
            endif
 
101      continue
 
      endif
 
 
*
* NOW PRINT THE NUMBER OF STARS FOUND OK
*
 
      if(ilevel.ge.2) then
         call wruser(' ',istat)
         write(prbuf,15)ngood
15       format(3x,i10,' STAR(S) FITTED SUCCESSFULLY')
         call lbgone(prbuf(4:))
         call wruser(prbuf,istat)
         call wruser(' ',istat)
 
*
* PRINT THE MEAN RESULTS
*
         write(prbuf,16)axisr
16       format('   MEAN AXIS RATIO=',ss,g11.4)
         call wruser(prbuf,istat)
         call wruser(' ',istat)
         write(prbuf,17)theta*57.29578
17       format('   MEAN INCLINATION OF MAJOR AXIS=',ss,g11.4,' DEGREES'
     :   )
         call lbgone(prbuf(43:))
         call wruser(prbuf,istat)
         call wruser(' ',istat)
      endif
 
 
*
* CALL RADPRF TO DETERMINE THE FORM OF THE RADIAL PROFILE
*
      call radprf(ia,npix,nlines,inval,ilevel,sig0,axisr,theta,range
     : ,x,y,nxy,sig(1,5),device,fwhm,gamma,ierrf)
 
*
* IF THE RADIAL PROFILE COULD NOT BE FOUND, SET IERR=2 AND ABORT
*
 
      if(ierrf.ne.0) ierr=2
99    return
 
      end
 
 
 
