      subroutine starma
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO ESTIMATE STELLAR MAGNITUDES BY FITTING A STANDARD FUNCTION
*       TO STAR IMAGES
*
*METHOD
*       OBTAIN THE INPUT IMAGE AND EXTRACT THE REQUIRED DESCRIPTOR ITEMS
*       OBTAIN THE LIST OF APPROXIMATE STAR POSITIONS AND OBTAIN
*       WORKSPACE. COPY THE X,Y POSITIONS AND IDENTIFIERS TO THE
*       WORKSPACE. OBTAIN THE PARAMETERS GOVERNING THE FIT. CALL PHTLST
*       TO PERFORM THE PHOTOMETRIC FITTING. OBTAIN THE OUTPUT LIST
*       AND ADD THE RESULTS TO IT. UPDATE THE OUTPUT DESCRIPTOR.
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
*               THE INPUT LIST OF POSITIONS
*       NOSPACE/ERROR/
*               ACCESSED IF WORKSPACE CANNOT BE OBTAINED
*       SEEING
*               THE FWHM SEEING
*       AXISR
*               THE AXIS RATIO OF ELLIPTICAL STAR IMAGES
*       THETA
*               THE INCLINATION OF THE MAJOR AXIS TO THE X DIRECTION
*               IN DEGREES (X THROUGH Y POSITIVE)
*       GAMMA
*               THE RADIAL EXPONENT IN THE STAR PROFILE FUNCTION
*       RANGE
*               THE RADIUS IN UNITS OF THE STAR 'SIGMA' TO WHICH THE
*               FIT EXTENDS
*       ZEROMAG
*               A ZERO LEVEL TO BE ADDED TO ALL MAGNITUDES CALCULATED
*       NOLIST/ERROR/
*               ACCESSED IF NO RESULTS ARE OBTAINED
*       OUTPUT
*               OUTPUT LIST OF ACCURATE POSITIONS AND MAGNITUDES
*       TITLE
*               A TITLE TO REPLACE THE INPUT TITLE IN THE OUTPUT LIST
*
*CALLS
*       THIS PACKAGE:
*               GETPAR,GT2DIR,GTDSCR,GTXYLR,EXTLST,PHTLST,GTXYLW,ADDLST,
*               PTDSCR,NEWSCL
*       STARLINK:
*               GETDYN,WRERR,CYDSCR,RDKEYC,FRDATA
*
*NOTES
*       USES VAX %VAL FACILITY
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      character cval*1,title(1)*30
 
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
* INPUT IMAGE OBTAINED SUCCESSFULLY...EXTRACT REQUIRED DESCRIPTOR
* ITEMS
*
         zero=0.0
         scale=1.0
         inval=-100000
         call gtdscr('IMAGE','BZERO','REAL',ival,zero,cval,ierr)
         call gtdscr('IMAGE','BSCALE','REAL',ival,scale,cval,ierr)
         call gtdscr('IMAGE','INVAL','INTEGER',inval,rval,cval,ierr)
 
*
* IF THE IMAGE SCALE FACTOR IS NEGATIVE, THE IMAGE DATA IS STORED UPSIDE
* DOWN... OBTAIN ANOTHER IMAGE AND CONVERT TO A POSITIVE SCALE FACTOR
*
 
         if(scale.lt.0.0) then
            call getdyn('WORK',102,npix*nlines,ipwrk,istwrk)
 
*
* IF NEW IMAGE SPACE IS NOT AVAILABLE, GIVE MESSAGE AND ABORT
*
 
            if(istwrk.ne.0) then
               call wrerr('NOSPACE')
               go to 99
 
            endif
 
 
*
* CALL NEWSCL TO CONVERT THE SCALE FACTOR
*
            call newscl(%val(ipimg),npix,nlines,inval,scale,zero,inval
     :       ,-scale,zero,%val(ipwrk))
 
*
* SUBSEQUENTLY USE THE NEW IMAGE AND SCALE FACTOR
*
            scale=-scale
            ipimg=ipwrk
         endif
 
 
*
* OBTAIN INPUT LIST OF POSITIONS
*
         call gtxylr('INPUT',.false.,nitem,lstlen,ipin,ierrxy)
 
         if(ierrxy.eq.0) then
 
*
* INPUT LIST OBTAINED SUCCESSFULLY...EXTRACT TITLE
*
            title(1)=' '
            call gtdscr('INPUT','TITLE','CHARACTER',ival,rval,title(1)
     :       ,ierr)
 
*
* OBTAIN WORKSPACE FOR INPUT AND OUTPUT LISTS OF POSITIONS AND
* MAGNITUDES
*
            call getdyn('IDA',104,5*lstlen,ipida,istida)
            call getdyn('IDB',104,5*lstlen,ipidb,istidb)
            call getdyn('XA',104,lstlen,ipxa,istxa)
            call getdyn('YA',104,lstlen,ipya,istya)
            call getdyn('XB',104,lstlen,ipxb,istxb)
            call getdyn('YB',104,lstlen,ipyb,istyb)
            call getdyn('MAG',104,lstlen,ipmag,istmag)
 
*
* IF SPACE WAS NOT AVAILABLE, GIVE MESSAGE AND ABORT
*
 
            if((istida.ne.0).or.(istidb.ne.0).or.(istxa.ne.0).or.(istya
     :       .ne.0).or.(istxb.ne.0).or.(istyb.ne.0).or.(istmag.ne.0))
     :         then
               call wrerr('NOSPACE')
               go to 99
 
            endif
 
 
*
* COPY INPUT IDENTIFIERS AND X,Y POSITIONS TO WORKSPACE
*
            call extlst(%val(ipin),nitem,lstlen,%val(ipida),1,20)
            call extlst(%val(ipin),nitem,lstlen,%val(ipxa),21,24)
            call extlst(%val(ipin),nitem,lstlen,%val(ipya),25,28)
 
*
* OBTAIN PARAMETERS DESCRIBING THE STAR IMAGE:
*
            seeing=5.0
            call getpar('SEEING','REAL',1,1.0,50.0,.true.,ival,seeing
     :       ,ierr)
            e=1.0
            call getpar('AXISR','REAL',1,1.0,2.0,.true.,ival,e,ierr)
            theta=0.0
            call getpar('THETA','REAL',1,-180.0,+180.0,.true.,ival,
     :      theta,ierr)
 
*
* CONVERT TO RADIANS
*
            theta=theta*0.0174533
            gamma=2.0
            call getpar('GAMMA','REAL',1,1.0,5.0,.true.,ival,gamma,
     :      ierr)
            range=3.5
            call getpar('RANGE','REAL',1,1.0,5.0,.true.,ival,range,
     :      ierr)
            zermag=0.0
            call getpar('ZEROMAG','REAL',1,-1.0e20,1.0e20,.true.,ival
     :       ,zermag,ierr)
 
*
* CALCULATE THE 'STANDARD DEVIATION' FROM THE FWHM SEEING AND
* THE SIZE OF BOX REQUIRED TO CONTAIN A STAR IMAGE
*
            sigma=(0.5*seeing)/(1.38629**(1.0/gamma))
            isize=nint(seeing*e*range)
 
*
* CALL PHTLST TO EVALUATE THE MAGNITUDES OF EACH STAR IN THE LIST
* AND TO DISPLAY THE RESULTS
*
            call phtlst(%val(ipxa),%val(ipya),%val(ipida),lstlen,
     :      %val(ipimg),npix,nlines,inval,scale,zero,isize,sigma,e,
     :      theta,gamma,range,zermag,ilevel,%val(ipxb),%val(ipyb),
     :      %val(ipidb),%val(ipmag),nout)
 
*
* IF THERE ARE NO RESULTS IN THE OUTPUT LISTS, GIVE ERROR MESSAGE
* AND ABORT
*
 
            if(nout.le.0) then
               call wrerr('NOLIST')
               go to 99
 
            endif
 
 
*
* OBTAIN THE OUTPUT FRAME
*
            call gtxylw('OUTPUT',.false.,8,nout,ipout,ierrb)
 
            if(ierrb.eq.0) then
 
*
* OUTPUT FRAME OBTAINED SUCCESSFULLY...COPY RESULTS INTO IT
*
               call addlst(%val(ipout),8,nout,%val(ipidb),1,20)
               call addlst(%val(ipout),8,nout,%val(ipxb),21,24)
               call addlst(%val(ipout),8,nout,%val(ipyb),25,28)
               call addlst(%val(ipout),8,nout,%val(ipmag),29,32)
 
*
* COPY DESCRIPTOR FROM INPUT TO OUTPUT AND ADD NEW ITEMS
*
               call cydscr('INPUT','OUTPUT',istat)
               call ptdscr('OUTPUT','NITEM','INTEGER',8,rval,cval,ierr)
               call ptdscr('OUTPUT','LSTLEN','INTEGER',nout,rval,cval
     :          ,ierr)
               call rdkeyc('TITLE',.true.,1,title,nval,ierr)
               call ptdscr('OUTPUT','TITLE','CHARACTER',ival,rval,
     :         title(1),ierr)
            endif
 
         endif
 
      endif
 
 
*
* FREE DATA AREAS AND RETURN
*
99    call frdata(' ',istat)
      return
 
      end
 
 
 
