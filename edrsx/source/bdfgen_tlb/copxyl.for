      subroutine copxyl(data,nfield,nrec,id,x,y,d,nxf,nyf,ndf,idstrt)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Copies data from a structure read from a text file into a
*       BDF XY list.
*
*SOURCE
*       COPXYL.FOR in BDFGEN.TLB
*
*ARGUMENTS
*   INPUTS:
*       nrec              integer       No. of records read from file
*       nfield            integer       No. of fields in each record
*       data(nrec,nfield) real          Input data
*       nxf               integer       Field no. of X data
*       nyf               integer       Field no. of Y data
*       ndf               integer       Field no. of pixel data
*                                       (0 = no data values wanted)
*	idstrt		  integer	Integer to start counting 
*				        identifiers from.
*   OUTPUTS:
*	id(20,nrec)	  byte		List of 20 byte ASCII identifiers
*	x(nrec)		  real		X values
*	y(nrec)	 	  real		Y values
*	d(nrec)		  real		Data values
*
*SUBROUTINES CALLED
*       EDRS:
*               lbgone
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*	Byte arrays
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 27/11/89
*-------------------------------------------------------------------
*
      implicit none

*
* DECLARE ARGUMENTS
*
      integer   nfield,nrec,nxf,nyf,ndf,idstrt
      real      data(nrec,nfield),x(nrec),y(nrec),d(nrec)
      byte	id(20,nrec)

*
* DECLARE LOCAL VARIABLES
*
      integer   i	! Character counter within an identifier
      character idbuf*20! Buffer for a complete identifier
      integer   ierr	! Local error status
      integer   irec	! Record counter

*
* LOOP ROUND ALL RECORDS
*
      do irec=1,nrec

*
* GENERATE THE IDENTIFIER IN THE FORM "#n"
*
         write(idbuf,'(i20)') irec+idstrt-1
         idbuf(1:1)='#'
         call lbgone(idbuf(2:))

*
* PUT ID INTO IDENTIFIER LIST
*
         do i=1,20
            id(i,irec)=ichar(idbuf(i:i))
         enddo

*
* COPY THE X,Y AND DATA VALUES TO THE XY LIST
*
         x(irec)=data(irec,nxf)
         y(irec)=data(irec,nyf)
         if(ndf.gt.0) d(irec)=data(irec,ndf)

*
* DO NEXT RECORD
*
      enddo

*
* FINISH
*
  999 continue

      end
