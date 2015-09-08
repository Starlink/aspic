      subroutine convw(in,nsin,ndet,sbegin,send,nsout,out,status)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Convert the input INTEGER*2 data to DOUBLE PRECISION, using only
*       samples SBEGIN to SEND from each detector.
*
*SOURCE
*       CONVW.FOR in NDFOUT.TLB
*
*ARGUMENTS       
*   INPUT:
*       in(nsin,ndet) integer*2 Input integer data.
*       nsin,ndet     integer   Dimensions of input data.
*       sbegin        integer   First input sample to copy to output.
*       send          integer   Last input sample to copy to output.
*       nsout         integer   Size of each line in output data.
*	status	integer		Inherited status
*
*   OUTPUTS:
*       out(nsout,ndet) double precision       Output data.
*       status        integer   Exit status
*
*SUBROUTINES CALLED
*       None
*              
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 10/2/91
*-------------------------------------------------------------------
      implicit none

*
* INCLUDE SAE PARAMETERS, ETC
*
      include 'SAE_PAR'

*
* DECLARE ARGUMENTS
*
      integer	nsin,ndet,sbegin,send,nsout,status
      double precision out(nsout,ndet)
      integer*2 in(nsin,ndet)

*
* DECLARE LOCAL VARIABLES
*
      integer	det	! Detector counter.
      integer	samp	! Sample counter.

*
* CHECK INHERITED STATUS
*
      if(status.ne.SAI__OK) return

*
* LOOP THROUGH EACH DETECTOR
*
      do det=1,ndet

*
* COPY THE REQUIRED RANGE OF SAMPLES FROM INPUT TO OUTPUT.
*
         do samp = sbegin,send
            out(samp,det) = dble( in(samp,det) )
         enddo

      enddo

*
* FINISH
*
      end
