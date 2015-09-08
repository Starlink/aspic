      subroutine poscon(x,y,boxxlo,boxxhi,boxylo,boxyhi,arcmin,pixel,
     :                  unsdat,ilin,nstdet,xlim,samplo,samphi,
     :                  unsmax,unsmin,offset,dets,ndets,ndtout,data,
     :                  npix,nlin,inval,scndir,nstpix,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Converts a screen x,y position into the nearest pixel of the
*       nearest trace. The data value at that pixel is also returned.
*
*SOURCE
*       POSCON.FOR in CRDDTRACE.TLB
*
*METHOD
*       It is assumed that the currently active SGS zone is the
*       base zone.
*
*ARGUMENTS
*   INPUTS:
*       x,y             reals   The screen position in SGS base zone
*                               co-ords
*       boxxlo,boxxhi   reals   The limits of the plotting box
*       boxylo,boxyhi
*       ilin            integer The detector id of the closest trace
*       arcmin          real    The position in arcmins north of centre
*                               of nearest pixel
*       xlim(2) real    The south and north limits of the x
*                               axis in arcmins north of centre
*       samplo,samphi   reals   The lower and upper limit of the x axis
*                               in data sample numbers (i.e. pixels)
*       unsmax,unsmin   reals   The limits of the y axis scale in
*                               unscaled data units
*       offset(ndets)   real    The offsets in y (in unscaled data
*                               units) of each detector
*       dets(ndets)     integer The list of detectors displayed
*       ndets           integer Size of arrays dets,ballno and offset
*       ndtout          integer The number of displayed detectors
*       data(npix,nlin) real    The unscaled time-corrected data
*       npix,nlin       integer The dimensions of array data
*       inval           integer The value stored in invalid pixels
*       scndir        character Direction of scan north or south
*       nstpix          logical True if the returned values should be
*                               those of the nearest pixel. If false,
*                               then the value of unsdat is that at the
*                               cursor and x and y are not changed
*   OUTPUT
*       pixel           integer The nearest pixel
*       unsdat          real    The unscaled data value of the nearest
*                               pixel (see argument nstpix)
*       x,y             reals   Screen posn of pixel used
*       ilin            integer Detector id of nearest trace
*       nstdet          integer The trace no. with the pixel on
*       ierr            integer Error status: 0 - Success
*                                             1 - No data at cursor
*                                                       (reported)
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               wrerr
*
*STARLINK PARAMETERS
*       NODATA/error/   Accessed if the cursor x position does not
*                       intersect any valid data points
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 23/9/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer   ndets,dets(ndets),ndtout,npix,nlin,inval,nstdet
      real      data(npix,nlin),boxxlo,boxxhi,boxylo,boxyhi,xlim(2)
      real      samplo,samphi,unsmax,unsmin,offset(ndets)
      real      x,y,unsdat,arcmin
      integer   pixel,ilin,ierr
      character scndir*(*)
      logical   nstpix
*
* DECLARE LOCAL VARIABLES
*
      real      const   ! A constant value required in calculating
                        ! cloeset data trace
      real      dat     ! The unscaled data value of the trace nearest
                        ! to the cursor
      real      temp    ! temporary real storage
      integer   trace   ! trace count
      real      unsdev  ! The unscaled deviation from trace to cursor
      ierr=0
*
* CONVERT X POSITION TO DATA PIXEL LOCATION
*
      if(scndir.eq.'NORTH') then
         pixel=nint(samplo+(samphi-samplo)*(x-boxxlo)
     :              /(boxxhi-boxxlo))
         if(nstpix) then
            x=boxxlo+(pixel-samplo)*(boxxhi-boxxlo)/(samphi-samplo)
         endif
      else
         pixel=nint(samplo+(samphi-samplo)*(boxxhi-x)
     :              /(boxxhi-boxxlo))
         if(nstpix) then
            x=boxxhi-(pixel-samplo)*(boxxhi-boxxlo)/(samphi-samplo)
         endif
      endif
*
* CONVERT X POSITION TO ARCMINS NORTH OF THE FIELD CENTRE
*
      arcmin=xlim(1)+(xlim(2)-xlim(1))*(x-boxxlo)/(boxxhi-boxxlo)
*
* FIND WHICH DATA TRACE IS CLOSEST TO THE CURSOR POSITION
*
* FIRST SET UP CONSTANT
*
      const=offset(1)+unsmin+(unsmax-unsmin)*(y-boxylo)
     :      /(boxyhi-boxylo)
*
* INITIALIZE THE DEVIATION OF THE CURSOR FROM THE NEAREST TRACE TO A
* LARGE NUMBER
*
      unsdev=1.0e32
*
* LOOP THROUGH EACH TRACE TO FIND THE CLOSEST TRACE AND THE UNSCALED
* DATA VALUE OF THAT TRACE AT THE CURSOR X POSITION
*
      do trace=1,ndtout
         dat=data(pixel,dets(trace))
         if(dat.ne.inval) then
            temp=abs(const-offset(trace)-dat)
            if(temp.lt.unsdev) then
               unsdev=temp
               ilin=dets(trace)
               nstdet=trace
               unsdat=dat
            endif
         endif
      enddo
*
* IF NO VALID PIXELS LIE ABOVE OR BELOW THE CURSOR, GIVE MESSAGE
*
      if(unsdev.eq.1.0e32) then
         call wrerr('NODATA')
         ierr=1
         goto 999
      endif
*
* IF NEAREST PIXEL VALUES ARE REQUIRED, CALCULATE Y POSITION AT WHICH
* THIS PIXEL OCCURS
*
      if(nstpix) then
         y=boxylo+(unsdat-unsmin+offset(nstdet)
     :     -offset(1))*(boxyhi-boxylo)/(unsmax-unsmin)
*
* OTHERWISE CALCULATE THE UNSCALED DATA VALUE CORRESPONDING TO THE
* CURSOR Y POSITION
*
      else
         unsdat=const-offset(nstdet)
      endif
*
* FINISH
*
 999  continue

      end
