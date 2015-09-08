      subroutine histpc(data,npix,nlin,ilin,inval,pc,vpc,npc,hist,
     :                  hstsiz,minval,maxval,ierr)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To find the value in a line of an image corresponding to a
*       specific fraction of the image line histogram from 0.0 to 1.0
*
*SOURCE
*       HISTPC.FOR in UTILITIES.TLB
*
*METHOD
*       Form a histogram of the image values and scan up or down to
*       find the appropriate point. The resolution of the histogram
*       is determined by the size of the histogram array, hstsiz.
*
*ARGUMENTS
*   INPUTS:
*       data(npix,nlin) real     The image data
*       npix            integer  The no. of pixels in a line of input
*       nlin            integer  The no. of lines in input
*       ilin            integer  The line for which values are required
*       inval           integer  The flag for invalid pixels
*       pc(npc)         real     An array of fractional positions in the
*                                histogram. In range 0.0 to 1.0.
*       npc             integer  The size of arrays pc and vpc
*       hstsiz          integer  The size of the histogram array, hist.
*       maxval          real     The max value of the data in the line
*       minval          real     The min value of the data in the line
*   OUTPUTS:
*       vpc(npc)        real     An array of real results corresponding
*                                to the positions in array pc
*       hist(hstsiz)    integer  Workspace for the histogram.
*                                Contains the histogram on exit.
*       ierr            integer  Error status: 0 - Success
*                                              1 - No valid input pixels
*                                              2 - MAXVAL = MINVAL
*
*SUBROUTINES CALLED
*       none
*
*VAX SPECIFICS
*       enddo
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 22/9/87
*-----------------------------------------------------------------------
*
*

      integer npc,npix,nlin,ilin,hstsiz,hist(hstsiz)
      real data(npix,nlin),vpc(npc),pc(npc),minval,maxval
      logical allbad

*
* IF MAX AND MIN VALUES TO BE USED FOR HISTOGRAM ARE EQUAL, EXIT
* WITH IERR=2
*
      if(maxval.eq.minval) then
         ierr=2
         goto 999
      endif

*
* INITIALLISE HISTOGRAM
*
      ierr=0
      do i=1,hstsiz
         hist(i)=0
      enddo

*
* FORM A HISTOGRAM OF ALL THE VALID PIXELS
*
      binsiz=(maxval-minval)/hstsiz
      allbad=.true.
      npts=0
      do i=1,npix
         integr=int((data(i,ilin)-minval)/binsiz)+1
         if(data(i,ilin).ne.inval) then
            allbad=.false.
            if(integr.ge.1.and.integr.le.hstsiz) then
               hist(integr)=hist(integr)+1
               npts=npts+1
            endif
         endif
      enddo

*
* IF THERE ARE NO VALID PIXELS, EXIT WITH ERROR FLAG SET
*
      if(allbad) then
         ierr=1
         go to 999
      endif

*
* CONSIDER EACH PERCENTAGE HISTOGRAM POINT
*
      do i=1,npc

*
* CALCULATE THE NUMBER OF DATA POINTS CORRESPONDING TO THIS POINT
* COUNTING UP OR DOWN DEPENDING ON WHICH SIDE OF THE MEDIAN
*
         if(pc(i).le.0.5) then
            limit=nint(npts*pc(i))
            istart=1
            iend=hstsiz
            idirn=1
         else
            limit=nint(npts*(1.0-pc(i)))
            istart=hstsiz
            iend=1
            idirn=-1
         endif
         if(limit.eq.0) limit=1

*
* COUNT THROUGH HISTOGRAM TO FIND THIS POINT
*
         n=0
         do j=istart,iend,idirn
            n=n+hist(j)
            if(n.ge.limit) go to 8
         enddo
8        vpc(i)=(j-0.5)*binsiz+minval
      enddo

*
* FINISH
*
999   continue

      end
