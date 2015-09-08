      subroutine datara
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* PURPOSE
*       Calculates the following image statistics, writing them both to
*       the screen and the environment:
*          1) MAX and MIN values
*          2) Range of data values (MAX-MIN)
*          3) Mean data value
*          4) Standard deviation of data values
*          5) Number of valid pixels in image
*          6) Number of invalid pixels in image
*          7) Optionally, a set of data values corresponding to a set
*             of cumulative histogram points
*          8) Total valid data sum in image
*
*SOURCE
*       DATARA.FOR in DATARANGE.TLB
*
*METHOD
*       Get the input image and descriptors. Call IMSTAT to find
*       the required image statistics.
*       If user specifies any histogram points, calculate the
*       corresponding data values by calling PCHIST.
*       If ILEVEL is greater than 1, display the statistics.
*       Write these values to output parameters.
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              gt2dir,imstat,putpar,wrerr
*       EDRS:
*              gtdscr,getpar,lbgone,pchist,trcout,rngerr
*       INTERIM:
*              wruser,frdata,rdkeyr,cnpar
*
*STARLINK PARAMETERS
*       INPUT/read/     The input image
*       ILEVEL/read/    The user information level
*       HISTPC/read/    A list of histogram percentage points
*       MAX/write/      The maximum data value in the input
*       MIN/write/      The minimum data value in the input
*       RANGE/write/    The range of data values in the input
*       MEAN/write/     The mean data value in the input
*       DEVN/write/     The standard deviation of the data in the input
*       DATVALS/write/  A list of data values corresponding to HISTPC
*       DATASUM/write/  Total valid data sum in image
*       NOVALID/error/  Accessed if the input contains no valid data
*       TOOMANY/error/  Accessed if there are too many histogram data
*                       values to store in a single output parameter.
*       BADVALUE/error/ Accessed if a bad histogram percentage value is
*                       given
*       TOOBAD/error/   Accessed if too many bad percentage values are given
*
*VAX SPECIFICS
*       %val
*       do while
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 7/4/88
*-------------------------------------------------------------------
*
      parameter (minint=-32768,maxint=32767,maxbad=3)
      character cval*1,prbuf*80
      logical more
      integer hist(minint:maxint),ipc(20)
      real    histpc(20),rpc(20)
*
* GET INPUT IMAGE
*
      call gt2dir('INPUT',102,.false.,npix,nlin,ipin,ierr)
      if(ierr.eq..0) then
*
* GET IMAGE DESCRIPTORS
*
         inval=-100000
         scale=1.0
         zero=0.0
         call gtdscr('INPUT','BSCALE','REAL',ival,scale,cval,ierr)
         call gtdscr('INPUT','BZERO','REAL',ival,zero,cval,ierr)
         call gtdscr('INPUT','INVAL','INTEGER',inval,rval,cval,ierr)
*
* GET INTERACTION LEVEL
*
         ilevel=2
         call getpar('ILEVEL','INTEGER',1,1.0,3.0,.true.,ilevel,rval,
     :                ierr)
*
* CALL IMSTAT TO FIND THE IMAGE STATISTICS
*
         call imstat(%val(ipin),npix,nlin,inval,imax,imin,amean,adev,
     :               nval,ierr)
         if(ierr.eq.0) then
*
* SEE IF USER WANTS ANY HISTOGRAM POINTS CALCULATED
*
            nbad=0
            more=.true.
            do while(more)
               more=.false.
               call rdkeyr('HISTPC',.false.,20,histpc,nhist,ierr)
               if(ierr.eq.0) then
*
* CHECK THAT VALUES LIE BETWEEN 0 AND 100, AND THEN CONVERT TO
* FRACTIONAL VALUES (0.0 - 1.0)
*
                  do i=1,nhist
                     if(histpc(i).gt.100.0.or.histpc(i).lt.0.0) then
                        if(nbad.lt.maxbad) then
                           nbad=nbad+1
                           call wrerr('BADVALUE')
                           call rngerr('*** REAL VALUES','REAL',0.0,
     :                                 100.0)
                           call cnpar('HISTPC',ierr)
                           more=.true.
                        else
                           call wrerr('TOOBAD')
                           nhist=0
                        endif
                     else
                        histpc(i)=0.01*histpc(i)
                     endif
                  enddo
               else
                  nhist=0
               endif
            enddo
*
* IF REQUIRED CALL PCHIST TO PRODUCE A HISTOGRAM AND DETERMINE THE
* REQUIRED DATA VALUES
*
            if(nhist.gt.0) then
               call pchist(%val(ipin),npix,nlin,inval,histpc,ipc,nhist,
     :                     hist,minint,maxint,ierr)
               if(ierr.eq.0) then
*
* CONVERT THE UNSCALED INTEGER VALUES TO REAL DATA VALUES
*
                  do i=1,nhist
                     rpc(i)=scale*ipc(i)+zero
                  enddo
               endif
            endif
*
* CALCULATE MAX AND MIN DATA VALUES, DATA RANGE AND DATA SUM
*
            datmax=scale*imax+zero
            datmin=scale*imin+zero
            datran=datmax-datmin
            datmea=scale*amean+zero
            datdev=scale*adev
            ninval=npix*nlin-nval
            datasm=nval*datmea
*
* DISPLAY VALUES ON SCREEN
*
            if(ilevel.ge.2) then

               write(prbuf,10) datmax
   10          format('    Maximum data value: ',g13.6)
               call lbgone(prbuf(25:))
               call wruser(' ',ierr)
               call wruser(prbuf,ierr)

               write(prbuf,20) datmin
   20          format('    Minimum data value: ',g13.6)
               call lbgone(prbuf(25:))
               call wruser(' ',ierr)
               call wruser(prbuf,ierr)

               write(prbuf,30) datran
   30          format('    Data range: ',g13.6)
               call lbgone(prbuf(17:))
               call wruser(' ',ierr)
               call wruser(prbuf,ierr)

               write(prbuf,40) datmea
   40          format('    Mean data value: ',g13.6)
               call lbgone(prbuf(22:))
               call wruser(' ',ierr)
               call wruser(prbuf,ierr)

               write(prbuf,50) datdev
   50          format('    Standard deviation of data: ',g13.6)
               call lbgone(prbuf(33:))
               call wruser(' ',ierr)
               call wruser(prbuf,ierr)

               write(prbuf,60) nval
   60          format('    No. of valid pixels: ',I10)
               call lbgone(prbuf(26:))
               call wruser(' ',ierr)
               call wruser(prbuf,ierr)

               write(prbuf,70) ninval
   70          format('    No. of invalid pixels: ',I10)
               call lbgone(prbuf(28:))
               call wruser(' ',ierr)
               call wruser(prbuf,ierr)

               write(prbuf,80) datasm
   80          format('    Total valid data sum: ',G13.6)
               call lbgone(prbuf(27:))
               call wruser(' ',ierr)
               call wruser(prbuf,ierr)
*
* DISPLAY HISTOGRAM POINTS IF ANY WERE CALCULATED
*
               if(nhist.gt.0) then
                  do i=1,nhist
                     write(prbuf,90) 100*histpc(i),rpc(i)
   90                format('    ',F6.2,'% histogram point: ',
     :                      G13.6)
                     call lbgone(prbuf(30:))
                     call lbgone(prbuf(5:))
                     call wruser(' ',ierr)
                     call wruser(prbuf,ierr)
                  enddo
               endif

            endif
*
* WRITE THE OUTPUT PARAMETERS
*
            call putpar('MAX','REAL',ival,datmax,cval,ierr)
            call putpar('MIN','REAL',ival,datmin,cval,ierr)
            call putpar('RANGE','REAL',ival,datran,cval,ierr)
            call putpar('MEAN','REAL',ival,datmea,cval,ierr)
            call putpar('DEVN','REAL',ival,datdev,cval,ierr)
            call putpar('NVAL','INTEGER',nval,rval,cval,ierr)
            call putpar('NINVAL','INTEGER',ninval,rval,cval,ierr)
            call putpar('DATASUM','REAL',ival,datasm,cval,ierr)
*
* DUE TO LIMIT OF 63 CHARACTERS IN PARAMETERS, ONLY 6 HISTOGRAM DATA
* VALUES CAN BE STORED
*
            if(nhist.gt.0) then
               if(nhist.gt.6) call wrerr('TOOMANY')
               call trcout('DATVALS',rpc,min(6,nhist),ierr)
            endif
*
* IF NO VALID DATA IN INPUT, GIVE MESSAGE
*
         else
            call wrerr('NOVALID')
         endif
      endif
*
* FREE DATA AREAS
*
 999  call frdata(' ',ierr)

      end
