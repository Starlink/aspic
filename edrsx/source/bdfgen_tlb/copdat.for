      subroutine copdat(data,nfield,npt,out,npix,nlin,c,nxf,nyf,ndf,
     :                  scale,zero,inval,hilim,lolim)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Copies data from a structure read from a text file into a
*       BDF image.
*
*SOURCE
*       COPDAT.FOR in BDFGEN.TLB
*
*ARGUMENTS
*   INPUTS:
*       npt               integer       No. of records read from file
*       nfield            integer       No. of fields in each record
*       data(npt,nfield)  real          Input data
*       npix,nlin         integers      Size of output image
*       c(6)              real          Co-efficients of transformation
*                                       to be applied to values in file
*                                       to get actual pixel co-ords
*       nxf               integer       Field no. of X data
*       nyf               integer       Field no. of Y data
*       ndf               integer       Field no. of pixel data
*                                       (0 = use all fields)
*       hilim(nfield)     real          Max value of each field
*       lolim(nfield)     real          Min value of each field
*       inval             integer       Value to store in image if input
*                                       file specified no value
*   OUTPUTS:
*       scale             real          Value of BSCALE used for output
*       zero              real          Value of BZERO used for output
*       out(npix,nlin)    integer*2     Output image
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               wruser
*       EDRS:
*               lbgone
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*       2 byte integer values
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 8/6/88
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer   nfield,npt,npix,nlin,nxf,nyf,ndf,inval
      real      data(npt,nfield),hilim(nfield),lolim(nfield),scale,zero
      real      c(6)
      integer*2 out(npix,nlin)
*
* DECLARE LOCAL VARIABLES
*
      real      dmax
      real      dmin
      integer   i
      integer   ierr
      integer   irec
      integer   ix
      integer   iy
      integer   j
      integer   maxint
      integer   minint
      character prbuf*80
      real      rscl

      parameter (maxint=32767,minint=-32767)
*
* CALCULATE THE SCALE AND ZERO FACTORS
*
      if(ndf.gt.0) then

         dmax=hilim(ndf)
         dmin=lolim(ndf)

      else

         dmax=-1.0e32
         dmin=1.0e32
         do i=1,nfield
            dmax=max(dmax,hilim(i))
            dmin=min(dmin,lolim(i))
         enddo

      endif

      if((dmax-dmin).gt.1.0e-20) then
         scale=(dmax-dmin)/(0.75*(maxint-minint))
         zero=((dmax+dmin)-(maxint+minint)*scale)*0.5
         rscl=1.0/scale
      else if(dmax.ge.dmin) then
         scale=1.0
         zero=dmin
         rscl=1.0
      else
         scale=1.0
         zero=0.0
         rscl=1.0
      endif
*
* INITIALISE OUTPUT ARRAY TO INVALID
*
      do j=1,nlin
         do i=1,npix
            out(i,j)=inval
         enddo
      enddo
*
* IF PIXEL CO-ORDS ARE TO BE READ FROM INPUT FILE...
*
      if(nxf.gt.0.and.nyf.gt.0) then
*
* GO THROUGH ALL THE DATA
*
         do irec=1,npt
            ix=c(1)+c(2)*data(irec,nxf)+c(3)*data(irec,nyf)
            iy=c(4)+c(5)*data(irec,nxf)+c(6)*data(irec,nyf)
            if(ix.ge.1.and.ix.le.npix.and.iy.ge.1.and.iy.le.nlin) then
               if(out(ix,iy).ne.inval) then
                  write(prbuf,10) ix,iy,irec
  10              format(' *** Multiple assignment to pixel (',I6,
     :                   ',',I6,') at record ',I6)
                  call lbgone(prbuf(61:))
                  call lbgone(prbuf(43:))
                  call lbgone(prbuf(36:))
                  call wruser(prbuf,ierr)
               endif
               out(ix,iy)=nint((data(irec,ndf)-zero)*rscl)
            endif
         enddo
*
* OTHERWISE GENERATE PIXEL CO-ORDS AUTOMATICALLY. FIRST DEAL WITH
* CASE CREATING AN IMAGE FROM A SINGLE COLUMN  OF DATA
*
      else
         if(ndf.gt.0) then
            ix=0
            iy=1
            do irec=1,npt
               ix=ix+1
               if(ix.gt.npix) then
                  ix=1
                  iy=iy+1
                  if(iy.gt.nlin) then
                     call wruser('*** Output image too small to hold'//
     :                           ' all the data',ierr)
                     write(prbuf,20) irec-1
  20                 format('    Stopped at record ',I6)
                     call lbgone(prbuf(23:))
                     call wruser(prbuf,ierr)
                     goto 999
                  endif
               endif
               out(ix,iy)=nint((data(irec,ndf)-zero)*rscl)
            enddo
*
* NEXT DEAL WITH CASE WHERE ALL COLUMNDS ARE USED IN IMAGE
*
         else
            do iy=1,nlin
               do ix=1,npix
                  out(ix,iy)=nint((data(iy,ix)-zero)*rscl)
               enddo
            enddo
         endif

      endif

*
* FINISH
*
  999 continue

      end
