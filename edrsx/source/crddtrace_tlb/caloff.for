      subroutine caloff(offset,ndets,ndtout,unszer,inval,unsmax,unsmin,
     :                  spaces,avrges,scale,zero,dets)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To determine an offset for each trace to vertically seperate
*       the traces. The offset is given as an unscaled data value and
*       refers (after scaling) to the flux scale drawn on the right
*       hand edge of the data display. To determine an actual flux value
*       for a trace, the (scaled) offset must be subtracted from the
*       flux scale value displayed.
*
*SOURCE
*       CALOFF.FOR in CRDDTRACE.TLB
*
*METHOD
*       This version use one of three possible methods. The method used
*       is determined by the argument 'spaces':
*       1) spaces='CONSTANT'
*       This method returns offsets which are evenly spaced. If the
*       minimum data value in the displayed data is datmin, the offsets
*       arange that the positions of the datmin level for each trace are
*       evenly spaced between the upper and lower limits of the plotting
*       box. This results in any detector to detector striping being
*       apparent in the traces, which could result in the traces
*       becoming confused under high magnification.
*       2) spaces='AVERAGE'
*       This method uses an 'average' data value for each trace and
*       produces offsets which ensure that these 'average' data values
*       are equally spaced over the plotting area. Any detector to
*       detector striping is thus hidden and the amount of overlap of
*       adjacent traces is minimised. The 'average' unscaled data
*       values are calculated by routine averag and passed to this
*       routine in the array avrges.
*       3) spaces='FREE'
*       This method uses offsets specified by the user. The actual
*       values are aquired from the user in the parameter getting
*       and updating routines, GTPARS and GTPUPD as scaled data values
*       and this routine converts them to unscaled data values.
*
*ARGUMENTS
*   INPUTS:
*       ndets           integer The size of the offsets,dets and
*                               averages arrays
*       ndtout          integer The number of traces to be displayed
*       unszer          real    The unscaled data value corresponding
*                               to zero scaled data value
*       unsmax          real    The unscaled data value corresponding
*                               to the upper limit of the flux scale
*       unsmin          real    The unscaled data value corresponding
*                               to lowest data value to be displayed
*       spaces     character    The method to use for calculating the
*                               offsets (see METHOD above).
*       offset(ndets)   real    Contains either last times offsets or
*                               offsets specified by user if the method
*                               chosen was 'FREE'.
*       inval           integer The value stored as the 'average' value
*                               if the detector is to be disregarded
*       avrges(ndets)   real    The 'average' unscaled data values for
*                               each detector
*       scale           real    The scaling factor for producing scaled
*                               data values
*       zero            real    The zero offset for producing scaled
*                               data values
*       dets(ndets)     integer A list of detectors to be displayed
*   OUTPUTS:
*       offset(ndets)   real    The array to hold the offsets. Element
*                               1 is for the lowest trace, element 2 the
*                               next higher trace, etc.
*       spaces     character    If spaces='FREE' on entry, the values
*                               stored in offset are changed. This must
*                               onlt be done once, so spaces is changed
*                               to 'DEFAULT' to idicate that the offsets
*                               have already been calculated.
*
*SUBROUTINES CALLED
*       none
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 10/9/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer   ndets,ndtout,inval,dets(ndets)
      real      offset(ndets),unszer,unsmax,unsmin,avrges(ndets)
      real      scale,zero
      character spaces*(*)
*
* DECLARE LOCAL VARIABLES
*
      real      avebtm  ! The 'average' data value of the bottom trace
      integer   trace   ! Trace counter
*
* IF METHOD 'FREE' WAS SPECIFIED, CONVERT THE SUPPLIED OFFSETS TO
* UNSCALED DATA VALUES, AND CHANGE SPACES TO INDICATE THAT THE
* CONVERSION HAS BEEN DONE
*
      if(spaces.eq.'FREE') then
         do trace=1,ndets
            offset(trace)=(offset(trace)-zero)/scale
         enddo
         spaces='DEFAULT'
*
* IF METHOD WAS 'CONSTANT', CALCULATE EVENLY SPACED OFFSETS FOR EACH
* TRACE
*
      else if(spaces.eq.'CONSTANT') then
         do trace=1,ndtout
            offset(trace)=((trace-1)*(unsmax-unsmin)/ndtout)+unszer
         enddo
*
* IF METHOD WAS 'AVERAGE', CALCULATE OFFSETS WHICH CAUSE THE 'AVERAGE'
* TRACE DATA VALUES TO BE EVENLY SPACED.
*
      else if(spaces.eq.'AVERAGE') then
*
* LOOP TO CALCULATE A DIFFERENT OFFSET FOR EACH TRACE (INVALID DETECTORS
* HAVE SCALED OFFSET 0, UNSCALED OFFSET UNSZER)
*
         avebtm=avrges(dets(1))
         if(avebtm.eq.inval) avebtm=unszer
         do trace=1,ndtout
            if(avrges(dets(trace)).ne.inval) then

*
* CALCULATE THE OFFSET FOR THIS TRACE
*
               offset(trace)=-avrges(dets(trace))
     :                       +(unsmax-avebtm)*(trace-1)/ndtout
     :                       +avebtm+unszer
*
* IF THE LINE HAS NO VALID PIXELS, SET SCALED OFFSET TO ZERO
*
            else
               offset(trace)=unszer
            endif
         enddo
      endif
*
* FINISH
*
      end
