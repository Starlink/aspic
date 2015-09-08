      subroutine starfi
 
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO DETERMINE THE PARAMETERS OF A MODEL STAR PROFILE BY FITTING
*       STAR IMAGES
*
*METHOD
*       OBTAIN INPUT IMAGE CONTAINING STARS. OBTAIN LIST OF STAR
*       POSITIONS. OBTAIN WORKSPACE AND EXTRACT THE POSITIONS FROM THE
*       LIST. OBTAIN PARAMETERS CONTROLLING THE FITTING, THEN CALL
*       SPARAM TO PERFORM THE FITTING AND RETURN THE PROFILE PARAMETERS
*       WRITE THE RESULTS TO THE ENVIRONMENT.
*
*ARGUMENTS
*       NONE
*
*STARLINK PARAMETERS
*       ILEVEL
*               INTERACTION LEVEL: CONTROLS PRINTING OF RESULTS
*       IMAGE
*               THE INPUT IMAGE
*       INPUT
*               THE INPUT X,Y LIST OF STAR POSITIONS
*       NOSPACE/ERROR/
*               ACCESSED IF WORKSPACE CANNOT BE OBTAINED
*       ISIZE
*               LENGTH OF THE SEARCH SQUARE SIDE TO BE USED IN LOCATING
*               THE STARS AND FINDING THEIR ELLIPTICITY
*       RANGE
*               RADIUS IN UNITS OF THE STAR 'SIGMA' OUT TO WHICH THE
*               RADIAL STAR PROFILE IS TO BE FITTED
*       NOSTARS/ERROR/
*               ACCESSED IF NO STARS COULD BE FOUND
*       NOFIT/ERROR/
*               ACCESSED IF THE RADIAL PROFILE COULD NOT BE FITTED
*       SEEING
*               OUTPUT PARAMETER: THE FWHM ACROSS THE STARS' MINOR AXIS
*       AXISR
*               OUTPUT PARAMETER: THE AXIS RATIO OF THE STAR IMAGES
*       THETA
*               OUTPUT PARAMETER: THE INCLINATION OF THE MAJOR AXIS TO
*               THE X AXIS IN DEGREES (X THROUGH Y POSITIVE)
*       GAMMA
*               OUTPUT PARAMETER: THE EXPONENT IN THE RADIAL PROFILE
*       DEVICE
*               SELECTS 'NONE' OR THE OUTPUT GKS GRAPHICS DEVICE
*
*CALLS
*       THIS PACKAGE:
*               GETPAR,GT2DIR,GTDSCR,GTXYLR,EXTLST,SPARAM,NEWSCL,GETCMD
*		DEFDEV,GETDEV
*       STARLINK:
*               GETDYN,WRERR,WRKEYR,FRDATA
*
*NOTES
*       USES VAX %VAL FACILITY
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*----------------------------------------------------------------------
*
*
      character cval*1,device*30,opts*512
      real axisr(1),theta(1),fwhm(1),gamma(1)
 
*
* OBTAIN INTERACTION LEVEL
*
      ilevel=2
      call getpar('ILEVEL','INTEGER',1,1.0,3.0,.true.,ilevel,rval,ierr)
 
*
* OBTAIN INPUT IMAGE FRAME
*
      call gt2dir('IMAGE',102,.false.,npix,nlines,ipimg,ierrim)
 
      if(ierrim.eq.0) then
 
*
* IMAGE OBTAINED SUCCESSFULLY...EXTRACT REQUIRED DESCRIPTOR ITEMS
*
         inval=-100000
         scale=1.0
         call gtdscr('IMAGE','INVAL','INTEGER',inval,rval,cval,ierr)
         call gtdscr('IMAGE','BSCALE','REAL',ival,scale,cval,ierr)
 
*
* IF THE SCALE FACTOR IS NEGATIVE, THE IMAGE DATA IS STORED UPSIDE
* DOWN... OBTAIN NEW IMAGE FRAME AND INVERT IT
*
 
         if(scale.lt.0.0) then
            call getdyn('NEWIMG',102,npix*nlines,ipnew,istnew)
 
*
* IF SPACE IS NOT AVAILABLE, GIVE MESSAGE AND ABORT
*
 
            if(istnew.ne.0) then
               call wrerr('NOSPACE')
               go to 99
 
            endif
 
 
*
* CALL NEWSCL TO INVERT THE SCALE FACTOR
*
            call newscl(%val(ipimg),npix,nlines,inval,scale,0.0,inval
     :       ,-scale,0.0,%val(ipnew))
 
*
* SUBSEQUENTLY USE THE NEW IMAGE
*
            scale=-scale
            ipimg=ipnew
         endif
 
 
*
* OBTAIN LIST OF INITIAL STAR POSITIONS
*
         call gtxylr('INPUT',.false.,nitem,lstlen,ipin,ierrxy)
 
         if(ierrxy.eq.0) then
 
*
* POSITION LIST OBTAINED SUCCESSFULLY...OBTAIN WORKSPACE FOR THE
* X,Y POSITIONS AND IDENTIFIERS
*
            call getdyn('ID',104,5*lstlen,ipid,istid)
            call getdyn('X',104,lstlen,ipx,istx)
            call getdyn('Y',104,lstlen,ipy,isty)
            call getdyn('WORK',204,5*lstlen,ipwrk,istwrk)
 
*
* IF SPACE WAS NOT AVAILABLE, GIVE ERROR MESSAGE AND ABORT
*
 
            if((istid.ne.0).or.(istx.ne.0).or.(isty.ne.0).or.(istwrk
     :       .ne.0)) then
               call wrerr('NOSPACE')
               go to 99
 
            endif
 
 
*
* COPY IDENTIFIERS AND X,Y POSITIONS TO WORKSPACE
*
            call extlst(%val(ipin),nitem,lstlen,%val(ipid),1,20)
            call extlst(%val(ipin),nitem,lstlen,%val(ipx),21,24)
            call extlst(%val(ipin),nitem,lstlen,%val(ipy),25,28)
 
*
* OBTAIN SEARCH AREA SIZE (ISIZE) AND THE RANGE OF RADII TO BE
* USED IN THE PROFILE FIT (RANGE,IN UNITS OF SIGMA)
*
            isize=15
            call getpar('ISIZE','INTEGER',1,3.0,101.0,.true.,isize,rval
     :       ,ierr)
            range=4.0
            call getpar('RANGE','REAL',1,1.0,10.0,.true.,ival,range
     :       ,ierr)
 
*
* OBTAIN OUTPUT GRAPHICS DEVICE
*
            call defdev(device)
            call getdev('DEVICE',device,.true.,ierr)
            if(ierr.ne.0) goto 99
 
*
* CALL SPARAM TO FIND THE MEAN STAR PROFILE PARAMETERS
*
            call sparam(%val(ipimg),npix,nlines,inval,isize,range,
     :      %val(ipid),%val(ipx),%val(ipy),lstlen,ilevel,device,axisr(1)
     :       ,theta(1),fwhm(1),gamma(1),%val(ipwrk),ierr)
 
*
* IF IERR IS NOT ZERO, NO FIT HAS BEEN OBTAINED... GIVE ERROR MESSAGE
* AND ABORT
*
 
            if(ierr.eq.1) then
               call wrerr('NOSTARS')
               go to 99
 
 
            else if(ierr.ge.2) then
               call wrerr('NOFIT')
               go to 99
 
            endif
 
 
*
* WRITE RESULTS TO THE ENVIRONMENT
*
            theta(1)=theta(1)*57.29578
            call wrkeyr('SEEING',fwhm,1,istat)
            call wrkeyr('AXISR',axisr,1,istat)
            call wrkeyr('THETA',theta,1,istat)
            call wrkeyr('GAMMA',gamma,1,istat)
         endif
 
      endif
 
 
*
* RELEASE DATA AREAS AND RETURN
*
99    call frdata(' ',istat)
      return
 
      end
 
 
 
