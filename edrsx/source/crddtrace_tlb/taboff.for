      subroutine taboff(offset,noff,ndtout,dets,ndets,ballno,bscale,
     :                  bzero)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To display a table of scaled offsets for all displayed traces
*
*SOURCE
*       TABOFF.FOR in CRDDTRACE.TLB
*
*ARGUMENTS
*   INPUTS:
*       offset(noff)    real    The unscaled offsets
*       noff            integer The size of array offset
*       ndtout          integer The number of displayed traces
*       dets(ndets)     integer The cross scan position of each detector
*                               displayed (eg 1st, 3rd, 7th etc)
*       ndets           integer The size of array dets
*       ballno          integer The Ball no. of each detector in the
*                               current band in cross scan order
*       bscale          real    The scale factor for unscaled data
*       bzero           real    The zero level for unscaled data
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               strlen
*       EDRS:
*               lbgone
*       SGS:
*               sgs_stxj,sgs_shtx,sgs_sartx,sgs_tx,sgs_line,sgs_flush
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
      integer   noff,ndets,dets(ndets),ballno(ndets),ndtout
      real      offset(noff),bscale,bzero
*
* DECLARE LOCAL VARIABLES
*
      integer   detec   ! Detector counter
      real      flux    ! Scaled detector offset
      character idbuf*2 ! Buffer for Ball numbers
      character numbuf*10! Buffer for offset values
      integer   strlen  ! Function giving used length of a string
      real      yposn   ! Y position of detector info
*
* SET UP TEXT ATRRIBUTES ASSUMING CURRENT ZONE IS THE BASE ZONE FOR
* THE CURRENT DEVICE
*
      call sgs_stxj('CC')
      call sgs_shtx(0.011)
      call sgs_sartx(0.667)
*
* DISPLAY TABLE TITLES
*
      call sgs_tx(0.021,0.94,'DET#')
      call sgs_tx(0.087,0.94,'OFFSET')
      call sgs_line(0.003,0.93,0.038,0.93)
      call sgs_line(0.056,0.93,0.113,0.93)
*
* FOR EACH DETECTOR WRITE OUT ITS BALL NUMBER AND THE OFFSET OF ITS
* TRACE (WRITE OUT IN SAME ORDER AS DATA TRACES APPEAR)
*
      do detec=ndtout,1,-1
         yposn=0.9-(ndtout-detec)*0.025
         write(idbuf,'(I2)') ballno(dets(detec))
         call lbgone(idbuf)
         call sgs_tx(0.021,yposn,idbuf(:strlen(idbuf)))
         flux=bscale*offset(detec)+bzero
*
* IF OFFSET IS ZERO OUTPUT "0.0" RATHER THAN "0.000000E+00
*
         if(flux.ne.0) then
            write(numbuf,'(G10.4)') flux
            call lbgone(numbuf)
         else
            numbuf='0.0'
         endif
         call sgs_tx(0.087,yposn,numbuf(:strlen(numbuf)))
      enddo
      call sgs_flush
*
* FINISH
*
      end
