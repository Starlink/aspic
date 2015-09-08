      subroutine memce0(sky,file1,file2,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Copy the input trial sky into internal file 1, prior to
*	generating simulated data. Also set file 2 so that pixels
*	which are valid in the trial sky have the value 1.0, and 
*	invalid pixels have the value 0.0.
*
*SOURCE
*       MEMCE0.FOR in MEMCRDD.TLB
*
*METHOD
*	The integers in the input EDRS image are scaled, and invalid
*	pixels are set to zero. The mask image will be used to divide
*	out the effect on the generated data of these zero pixels.
*       
*ARGUMENTS       
*   INPUT:
*	sky(A9_nps,A9_nls)   I*2    Input unscaled trial sky image
*	ierr	          integer   Inherited status: 0 - "OK so far"
*   OUTPUTS:
*	file1(B0_nps,B0_nls) Real   File 1 holding internal image
*	file2(B0_nps,B0_nls) Real   File 2 holding invalid pixel mask
*       ierr              integer   Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*	/A9_COM/,/B0_COM/,/ME_COM/
*   WRITE:
*	/E0_COM/,
*		E0_val	The no. of valid pixels in the trial sky image
*
*SUBROUTINES CALLED
*       EDRS:
*              wrerr
*              
*STARLINK PARAMETERS
*	NOVALID(error) Accessed if input contains no valid data
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*       2 byte integer values
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 7/11/89
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
* INCLUDE COMMON BLOCKS HOLDING...

* ... TRIAL SKY EDRS DESCRIPTORS
      include '(A9_COM)'

* ... TRIAL SKY FITS DESCRIPTORS
      include '(B0_COM)'

* ... OUTPUT VALUES FROM THIS ROUTINE
      include '(E0_COM)'

* ... MEMSYS3 COMMON BLOCKS
      include '(ME_COM)'

*
* DECLARE ARGUMENTS
*
      integer	ierr
      integer*2	sky(A9_nps,A9_nls)
      real	file1(B0_nps,B0_nls),file2(B0_nps,B0_nls)

*
* DECLARE LOCAL VARIABLES
*
      integer	istat	! Temporary status value
      integer	line	! Line counter
      integer	offset	! Offset into an internal file
      integer	pixel	! Pixel counter
      integer	xint	! Pixel no. within internal image
      integer	xoff	! Offset in X between internal and external images
      integer	yint	! Line no. within internal image
      integer	yoff	! Offset in Y between internal and external images

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* INITIALISE FILES 1 AND 2 TO ALL ZEROS
*
      do line=1,B0_nls
         do pixel=1,B0_nps
            file1(pixel,line)=0.0
            file2(pixel,line)=0.0
         enddo
      enddo

*
* THE INTERNAL SKY IMAGE MAY BE BIGGER THAN THE DISK IMAGE DUE TO THE 
* REQUIREMENTS OF THE FFT ROUTINES. SET UP THE OFFSETS BETWEEN THE 
* INTERNAL AND EXTERNAL IMAGES
*
      xoff=(B0_nps-A9_nps)/2
      yoff=(B0_nls-A9_nls)/2

*
* LOOP ROUND THE IMAGE, COPYING THE SCALED DATA AND BUILDING THE 
* INVALID PIXEL MASK. 
*
      E0_val=0

      do line=1,A9_nls
         do pixel=1,A9_nps

            xint=pixel+xoff
            yint=line+yoff

            if(sky(pixel,line).ne.A9_inv) then
               file1(xint,yint)=A9_bsc*sky(pixel,line)+A9_bze
               file2(xint,yint)=1.0
               E0_val=E0_val+1
            else
               file1(xint,yint)=0.0
               file2(xint,yint)=0.0
            endif

         enddo
      enddo

*
* IF THE INPUT IMAGE CONTAINS NO VALID DATA, ABORT
*
      if(E0_val.eq.0) then
         call wrerr('NOVALID')
         ierr=1
         goto 999
      endif

*
* FINISH
*
  999 continue

      end
