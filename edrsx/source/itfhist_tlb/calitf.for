      subroutine calitf(hista,chista,histb,chistb,itf,axlo,axhi,bxlo,
     :                  bxhi,bscale,bzero)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Calculates the Intensity Transfer Function which when applied
*       to an image with histogram given by hista, produces an image
*       with histogram given by histb.
*
*SOURCE
*       CALITF.FOR in ITFHIST.TLB
*
*METHOD
*       The cumulative histograms of the data image and the reference
*       image are formed. For each data value in the data image, the
*       fraction of the image with value less than or equal to
*       this value is thus found. The data value in the reference image
*       below which the same fraction of the reference image lies is
*       then found and this data value is taken as the ITF value at the
*       current data image value.
*
*ARGUMENTS
*   INPUTS:
*       axlo,axhi             integers  Limits of arrays hista and itf
*       hista(axlo:axhi)      integer   Input histogram
*       bxlo,bxhi             integers  Limits of array histb
*       histb(bxlo:bxhi)      integer   Histogram to be matched
*       chista(axlo-1:axhi+1) integer   Work space for one cumulative
*                                       histogram
*       chistb(bxlo-1:bxhi+1) integer   Work space for other cumulative
*                                       histogram
*       bscale                real      Scale factor for histb data
*       bzero                 real      Zero offset for histb data
*   OUTPUTS:
*       out(axlo:axhi)        real      Required ITF
*
*SUBROUTINES CALLED
*       none
*
*VAX SPECIFICS
*       implicit none
*       do while
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 26/5/88
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer   axlo,axhi,bxlo,bxhi
      integer   hista(axlo:axhi),histb(bxlo:bxhi)
      integer   chista(axlo-1:axhi+1),chistb(bxlo-1:bxhi+1)
      real      itf(axlo:axhi),bscale,bzero
*
* DECLARE LOCAL VARIABLES
*
      integer   i       ! Loop count
      real      delta   ! Fractional pixel shift required by interpolation
      real      norm    ! Factor which normalises hista to histb
      integer   refval  ! Pointer to histb entry
      integer   target  ! No. of pixel in ref image which must be equal
                        ! to or lower than the current hista data value
      integer   val     ! Pointer to hista entry
*
* CALCULATE CUMULATIVE HISTOGRAMS
*
      chista(axlo-1)=0
      do i=axlo,axhi
         chista(i)=chista(i-1)+hista(i)
      enddo
      chista(axhi+1)=chista(axhi)+1

      chistb(bxlo-1)=0
      do i=bxlo,bxhi
         chistb(i)=chistb(i-1)+histb(i)
      enddo
      chistb(bxhi+1)=chistb(bxhi)+1
*
* CALCULATE FACTOR WHICH WILL NORMALISE HISTOGRAM A VALUES TO HISTOGRAM
* B VALUES
*
      norm=chistb(bxhi)/chista(axhi)
*
* INITIALIZE POINTER TO HISTOGRAM B ENTRY
*
      refval=bxlo
*
* LOOP THROUGHT ALL DATA VALUES IN INPUT HISTOGRAM, HISTA
*
      do val=axlo,axhi
*
* FOR EACH INPUT DATA VALUE, SEE WHAT FRACTION OF THE INPUT IMAGE
* HAS DATA VALUE EQUAL OR LESS THAN THE GIVEN INPUT DATA VALUE
*
         target=chista(val)*norm
*
* MOVE THROUGH THE REFERENCE CUMULATIVE HISTOGRAM UNTIL A DATA VALUE IS
* FOUND WHICH HAS A GREATER FRACTION OF THE REFERENCE IMAGE EQUAL TO OR
* LESS THAN IT.
*
         do while(chistb(refval).le.target)
            refval=refval+1
         enddo
*
* DO LINEAR INTERPOLATION BETWEEN THIS REFERENCE DATA VALUE AND THE ONE
* BELOW IT TO FIND THE REFERENCE DATA VALUE WHICH WOULD HAVE EXACTLY THE
* SAME FRACTION OF THE REFERENCE IMAGE EQUAL OR LESS THAN IT. THIS IS
* THE FINAL ITF VALUE FOR THIS INPUT DATA VALUE
*
         delta=(target-chistb(refval-1))/(chistb(refval)-
     :          chistb(refval-1))
         itf(val)=bscale*(refval-1+delta)+bzero
*
* DO NEXT INPUT DATA VALUE
*
      enddo
*
* FINISH
*
      end
