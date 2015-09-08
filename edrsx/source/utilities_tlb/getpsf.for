      subroutine getpsf(name,isize,ippsf,npixps,nlinps,ndets,npixp,
     :                  nlinp,invalp,scalep,zerop,trdtfp,band,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Gets an IRAS PSF stack as produced by PSFSTACK
*
*SOURCE
*       GETPSF.FOR in UTILITIES.TLB
*
*ARGUMENTS
*   INPUTS:
*       name          character   Name of DSCL parameter to associate
*                                 with PSF stack
*       isize         integer     Max no of PSFs per stack allowed
*   OUTPUTS:
*       ippsf(isize)  integer     Pointers to I*2 image planes
*       npixps        integer     No. of pixels per line in each image
*                                 plane.
*       nlinps        integer     No. of lines in each image plane.
*       ndets         integer     No. of images planes in stack
*       npixp(isize)  integer     No. of pixels per line in each PSF
*       nlinp(isize)  integer     No. of lines in each PSF
*       invalp(isize) integer     Invalid pixel value for each PSF
*       scalep(isize) real        Scale factor for each PSF
*       zerop(isize)  real        Zero offset for each PSF
*       trdtfp(6,isize) real      Coefficients of linear transformation
*                                 from PSF pixel coords to focal plane
*                                 Z and Y coords, for each PSF
*       band          integer     IRAS band no. of PSF stack
*       ierr          integer     Error status: 0 - Success
*                                               101 - Wrong no. of PSFs
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              wrerr
*       EDRS:
*              gtdscr,gtdscn,gt3dir
*
*STARLINK PARAMETERS
*       name/read/      Argument 'name' contains name of parameter
*                       to be associated with the PSF stack
*       WRONGDTS/error/ Access if PSF stack given by user does not have
*                       the correct no. of detectors for requested band
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 19/12/88
*-------------------------------------------------------------------
*
      implicit none

*
* INCLUDE IRAS MISSION PARAMETERS AND FOCAL PLANE INFORMATION
*
      include 'UTILITIES(IR_PAR)'
      include 'UTILITIES(DT_DAT)'

*
* DECLARE ARGUMENTS
*
      integer   band,isize,npixps,nlinps,ndets,ierr
      integer   ippsf(isize),npixp(isize),nlinp(isize),invalp(isize)
      real      scalep(isize),zerop(isize),trdtfp(6,isize)
      character name*(*)

*
* DECLARE LOCAL VARIABLES
*
      character   cval*1  ! Dummy character argument
      integer     icoef   ! Coefficient counter
      integer     idet    ! PSF counter
      integer     ival    ! Dummy integer argument
      real        rval    ! Dummy real argument
      real        temp(IR_dts,6) ! Temporary storage for transformation
                          ! coefficients

*
* GET 3D PSF IMAGE STACK
*
      call gt3dir(name,102,.true.,npixps,nlinps,ndets,ippsf,ierr)
      if(ierr.ne.0) goto 999

*
* GET IRAS BAND NUMBER
*
      call gtdscr(name,'BAND','INTEGER',band,rval,cval,ierr)

*
* CHECK THERE ARE THE RIGHT NUMBER OF PSFS IN THE STACK FOR THIS BAND
*
      if(ndets.ne.DT_bns(band)) then
         call wrerr('WRONGDTS')
         ierr=101
         goto 999
      endif

*
* GET PSF DESCRIPTOR ARRAYS. THESE ARRAYS HOLD THE VALUE OF EACH
* DESCRIPTOR FOR EACH PSF IMAGE. FIRST GET THE NUMBER OF LINES AND
* PIXELS ACTUALLY USED FOR THE PSF OUT OF EACH IMAGE PLANE.
*
      call gtdscn(name,'NPIX','INTEGER',1,ndets,npixp,rval,
     :                cval,ierr)
      call gtdscn(name,'NLIN','INTEGER',1,ndets,nlinp,rval,
     :                cval,ierr)

*
* NEXT GET THE DESCRIPTORS BSCALE, BZERO AND INVAL FOR EACH PSF. THESE
* ARE USED TO CONVERT THE STORED INTEGERS INTO REAL PSF VALUES
*
      call gtdscn(name,'INVAL','INTEGER',1,ndets,invalp,rval,
     :                cval,ierr)
      call gtdscn(name,'BSCALE','REAL',1,ndets,ival,scalep,
     :                cval,ierr)
      call gtdscn(name,'BZERO','REAL',1,ndets,ival,zerop,cval,
     :                ierr)

*
* NEXT GET THE COEFFICIENTS WHICH DESCRIBE THE TRANSFORMATION FROM
* PSF COLUMN AND ROW NUMBERS TO IRAS FOCAL PLANE Z AND Y COORDINATES.
*
      call gtdscn(name,'TRDTFP1','REAL',1,ndets,ival,temp(1,1),
     :                cval,ierr)
      call gtdscn(name,'TRDTFP2','REAL',1,ndets,ival,temp(1,2),
     :                cval,ierr)
      call gtdscn(name,'TRDTFP3','REAL',1,ndets,ival,temp(1,3),
     :                cval,ierr)
      call gtdscn(name,'TRDTFP4','REAL',1,ndets,ival,temp(1,4),
     :                cval,ierr)
      call gtdscn(name,'TRDTFP5','REAL',1,ndets,ival,temp(1,5),
     :                cval,ierr)
      call gtdscn(name,'TRDTFP6','REAL',1,ndets,ival,temp(1,6),
     :                cval,ierr)

*
* REORDER THESE COEEFICIENTS
*
      do idet=1,ndets
         do icoef=1,6
            trdtfp(icoef,idet)=temp(idet,icoef)
         enddo
      enddo

*
* FINISH
*
  999 continue
      end
