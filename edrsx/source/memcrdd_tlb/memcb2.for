      subroutine memcb2(ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Copies AO and SURVEY descriptors to a common location.
*
*SOURCE
*       MEMCB2.FOR in MEMCRDD.TLB
*
*METHOD
*	Various items are copied as listed in "COMMON USAGE". The
*	scale and zero values are modified to produce data values
*	in Jy rather than W/m**2 using the Oct. 1984 IPAC calibration
*       correction.
*
*ARGUMENTS       
*   INPUT:
*	ierr	integer		Inherited status: 0 - "OK so far"
*
*   OUTPUTS:
*       ierr    integer         Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*	/A5_COM/,/A6_COM/,/ZZ_COM/
*   WRITE:
*	/B2_COM/,
*		B2_bnd	IRAS band no. (1-4)
*		B2_ncf	No. of CRDD files given as input
*		B2_frm  Descriptor frame identifiers
*		B2_nam	Names of BDF files containing data
*		B2_pin	Pointers to start of files
*		B2_bze	Zero offsets giving data values in Jy
*		B2_bsc	scale factors giving data values in Jy
*		B2_bpx	Flag for blank data values (invalid)
*		B2_nde	No. of detector data streams in each file
*		B2_nys  No. of data samples per detector
*		B2_spd	Scan speeds in arcmins per second
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 29/9/89
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
* INCLUDE FOCAL PLANE DATA, ETC
*
      include 'UTILITIES(DT_DAT)'      

*
* INCLUDE COMMON BLOCKS HOLDING....

* ... AO DESCRIPTORS
      include 'UTILITIES(AO_COM)'

* ... SURVEY DESCRIPTORS
      include 'UTILITIES(DS_COM)'

* ... AO CRDD INFO
      include '(A5_COM)'

* ... SURVEY CRDD INFO
      include '(A6_COM)'

* ... OUTPUT VALUES FROM THIS ROUTINE 
      include '(B2_COM)'

* ... USER SUPPLIED PARAMETERS
      include '(ZZ_COM)'

*
* DECALRE ARGUMENTS
*
      integer	ierr

*
* DECLARE LOCAL VARIABLES
*
      integer	crddf	! Pointer to current CRDD file
      integer	frmid	! Current descriptor frame identifier

*
* CHECK INHERITED STATUS VALUE
*
      if(ierr.ne.0) goto 999

*
* IF USER GAVE AO DATA, COPY VALUES FROM /A5_COM/ AND /AO_COM/
*
      if(ZZ_typ.eq.'AO') then

         B2_bnd=A5_bnd
         B2_ncf=A5_ncf

         do crddf=1,A5_ncf

            frmid=A5_frm(crddf)
            B2_frm(crddf)=frmid
            B2_nam(crddf)=A5_nam(crddf)
            B2_pin(crddf)=A5_pin(crddf)
            B2_bze(crddf)=AO_bze(frmid)*DT_cvf(B2_bnd)*DT_o84(B2_bnd)
            B2_bsc(crddf)=AO_bsc(frmid)*DT_cvf(B2_bnd)*DT_o84(B2_bnd)
            B2_bpx(crddf)=AO_bpx(frmid)
            B2_nde(crddf)=AO_nde(frmid)
            B2_nys(crddf)=AO_nys(frmid)
            B2_spd(crddf)=A5_spd(crddf)

         enddo

*
* ELSE IF USER GAVE SURVEY DATA, COPY VALUES FROM /A6_COM/ AND /DS_COM/
*
      else

         B2_bnd=A6_bnd
         B2_ncf=A6_ncf

         do crddf=1,A6_ncf

            frmid=A6_frm(crddf)
            B2_frm(crddf)=frmid
            B2_nam(crddf)=A6_nam(crddf)
            B2_pin(crddf)=A6_pin(crddf)
            B2_bze(crddf)=DS_bze(frmid)*DT_cvf(B2_bnd)*DT_o84(B2_bnd)
            B2_bsc(crddf)=DS_bsc(frmid)*DT_cvf(B2_bnd)*DT_o84(B2_bnd)
            B2_bpx(crddf)=DS_bpx(frmid)
            B2_nde(crddf)=DS_nde(frmid)
            B2_nys(crddf)=DS_nys(frmid)
            B2_spd(crddf)=IR_scr

         enddo

      endif

*
* FINISH
*
  999 continue

      end
