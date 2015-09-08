      subroutine memcg3(ipin,size,file22,sdc,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Gets a hi-res image from file 1 of an analysis file and
*	writes it into file 1 of ME_st, adding on the background image.
*       stored in the analysis file.
*
*SOURCE
*       MEMCG3.FOR in MEMCRDD.TLB
*
*METHOD
*       
*ARGUMENTS       
*   INPUT:
*	ierr	integer		Inherited status: 0 - "OK so far"
*
*   OUTPUTS:
*       ierr    integer         Exit status: 0 - success
*	pnt	integer		Pointer to analysis file
*	sdc 	integer		Pointer to start of external SDC file
*
*COMMON USAGE
*   READ:
*	
*   WRITE:
*	/_COM/
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              
*       THIS PACKAGE (MEMCRDD.TLB):
*              
*       EDRS:
*              
*       INTERIM:
*              
*STARLINK PARAMETERS
*
*
*VAX SPECIFICS
*       implicit none
*       %val
*       do while
*       REAL*8
*       Trig functions in degrees
*       enddo
*       end of line comments
*       2 byte integer values
*       RTL routines
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) /10/89
*-------------------------------------------------------------------
      implicit none

*
* INCLUDE GLOBAL PARAMETER DECLARATIONS
*
      include '(PR_DEC)'

*
* INCLUDE IRAS MISSION PARAMETERS
*
      include 'UTILITIES(IR_PAR)'

*
* INCLUDE ALL COMMON BLOCKS
*
      include '(A5_COM)'
      include '(A6_COM)'
      include '(A7_COM)'
      include '(A8_COM)'
      include '(A9_COM)'
      include '(B0_COM)'
      include '(B1_COM)'
      include '(B2_COM)'
      include '(B5_COM)'
      include '(B6_COM)'
      include '(D3_COM)'
      include '(D5_COM)'
      include '(E0_COM)'
      include '(ME_COM)'
      include '(ZZ_COM)'

*
* DECLARE ARGUMENTS
*
      integer	ierr,ipin,size,file22,sdc

*
* DECLARE LOCAL VARIABLES
*
      real	acc	! Data sample accuracy from analysis file
      integer	bac	! Start of background image in analysis file
      integer	file1	! Start of file 1 in analysis file
      integer	istat	! Temporary status value
      integer	nlin	! No. of lines in the given BDF (should be 1)
      integer	offset	! Offset into internal files
      integer	start	! The location within the input BDF from which
			! the first element of the current data set is
			! to be read (updated by MEMCD7).

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* GET A POINTER TO THE INPUT BDF
*
      call gt2dir('ANALIN',104,.true.,size,nlin,ipin,ierr)

*
* CHECK FILE CONTAINS ONLY 1 LINE
*
      if(ierr.eq.0.and.nlin.ne.1) ierr=1
      if(ierr.ne.0) goto 999

*
* CALCULATE START POSITION OF FILE 22 AND SDC FILE IN ANALYSIS FILE
*
      file22=1+A5_sz+A6_sz+A7_sz+A8_sz+A9_sz+B0_sz+B1_sz+B2_sz+B5_sz+ 
     :       B6_sz+D3_sz+D5_sz+E0_sz+ME_sza+ME_szp+ME_szl+ZZ_sz+ME_mk

      sdc=file22+7*ME_mk

*
* CALCULATE THE START OF FILE 1
*
      file1=file22+11*ME_mk

*
* READ IN FILE 1
*
      call memcd7(ME_st(ME_kb(1)),ME_mj,%val(ipin),size,file1,ierr)
      if(ierr.ne.0) goto 999

*
* CALCULATE START OF BACKGROUND IMAGE IN ANALYSIS FILE
*
      bac=file1+6*ME_mj

*
* ADD THE BACKGROUND IMAGE ONTO FILE 1
*
      call bacoff(ME_st(ME_kb(1)),ME_mj,%val(ipin),size,bac,ierr)

*
* FINISH
*
  999 continue

      end



C---------------------------------------------------------------------
      subroutine bacoff(image,imsize,data,datsiz,start,ierr)
      implicit none
      integer	imsize,datsiz,ierr,start,index
      real	image(imsize),data(datsiz)

      if(ierr.ne.0) goto 999

      do index=1,imsize
         image(index)=image(index)+data(start+index-1)
      enddo

 999  continue

      end
