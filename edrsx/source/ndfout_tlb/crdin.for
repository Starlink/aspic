      subroutine crdin(name,band,frmid,ierr)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Copy the descriptors from an input CRDD file into common
*       block /DESCR/ (see module DS_COM), returning a pointer to 
*       the 'level' at which the frame descriptors are stored in 
*       /DESCR/. The waveband of the data in the file is extracted 
*       from descriptor 'CONTENT' and returned.
*
*SOURCE
*       CRDIN.FOR in NDFOUT.TLB
*
*ARGUMENTS
*
*   INPUTS:
*       name    character       Cotains the name of the Starlink
*                               parameter used to access the input.
*   OUTPUT:
*       band    integer         The IRAS band no. of the data (1-4).
*       frmid   integer         The 'level' at which the descriptors
*                               are stored in common block /DESCR/
*       ierr    integer         Status return: 0 - success
*                                             11 - Not a CRDD file.
*                                              (etc)
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               wrerr,ptderr
*       EDRS:
*               gtdscr
*       INTERIM:
*               ctoi,rddscr,ctod,ctor
*
*STARLINK PARAMETERS
*       'name'/read/    The string 'name' holds the parameter associated
*                       with the input CRDD file
*       TOOBAD/error/   Accessed if too many bad parameter values are
*                       given by user.
*       DESCFULL/error/ Accessed if there is nor room for more
*                       descriptors in the common arrays
*       NOTCRDD/error/  Accessed if the input file does not contain CRDD
*                       data
*       INVBAND/error/  Accessed if the IRAS band no. specified is
*                       outside range 1 to 4
*       BSSOVFLOW/error/        Accessed if the arrays for the boresight
*                       data samples are full
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 4/2/91
*-------------------------------------------------------------------
*
      implicit none

*
* INCLUDE COMMON DESCRIPTOR DECLARATIONS (VARIABLES CALLED DS_xxx)
*
      include 'UTILITIES(DS_COM)'

*
* DECLARE ARGUMENTS
*
      character*(*)     name
      integer           band,ierr,frmid

*
* DECLARE LOCAL VARIABLES
*
      integer           ap1     ! Offset to first apostrophy in string.
      integer           ap2     ! Offset to second apostrophy in string.
      character*80      bvalue(DS_mxb)  !Buffer array for boresight data
      character*80      ctemp   !Temporary storage for character descriptors
      character*1       cval    !Dummy character argument
      real*8            dtemp   !Temporary starage for double precision
      real*8            dutcx   !Abs. boresight crossing time
      integer           itemp   !Temporary storage for integers
      integer           isamp   !Loop count for boresight samples
      integer           ival    !Dummy integer argument
      real              rtemp   !Temporary starage for reals
      real              rval    !Dummy real argument
      real*8            utcs    !Temporary storage for UTCS values

*
* SET DESCRIPTOR LEVEL TO ONE HIGHER THAN PREVIOUS TOP LEVEL
*
      frmid=DS_top+1
      ierr=0
      if(frmid.gt.DS_mxf) then
         call wrerr('DESCFULL')
         ierr=18
      else
         DS_top=frmid
      endif
      if(ierr.eq.0) then

*
* GET THE IMAGE SIZE DESCRIPTORS
*
         call gtdscr(name,'NAXIS1','INTEGER',DS_nys(frmid),rval,cval,
     :               ierr)
         if(ierr.eq.0) call gtdscr(name,'NAXIS2','INTEGER',
     :                             DS_nde(frmid),rval,cval,ierr)

*
* IF ALL IS OK...
*
         if(ierr.eq.0) then

*
* GET THE VALUE OF THE 'CONTENT' DESCRIPTOR
*
            call gtdscr(name,'CONTENT','CHARACTER',ival,rval,ctemp,ierr)
            if(ierr.eq.0) then
               DS_con(frmid)=ctemp(2:)
            else
               DS_con(frmid)=' '
            endif

*
* CHECK THAT THE FILE CONTAINS CRDD DATA
*
            if(DS_con(frmid)(1:4).ne.'BAND'.or.
     :         DS_con(frmid)(8:13).ne.'SIGNAL') then
               ierr=11
               goto 999
            endif

*
* EXTRACT THE BAND NO. FROM THE CONTENT DESCRIPTOR AND CHECK IT
*
            call ctoi(DS_con(frmid)(5:6),band,ierr)
            if((ierr.eq.0.and.(band.lt.1.or.band.gt.4)).or.
     :         ierr.eq.5) then
               call wrerr('INVBAND')
               ierr=12
               goto 999
            endif

*
* GET THE VALUE OF THE 'BSCALE' DESCRIPTOR
*
            call gtdscr(name,'BSCALE','REAL',ival,DS_bsc(frmid),
     :                  cval,ierr)
            if(ierr.ne.0) then
               call ptderr('BSCALE')
               ierr=10
               goto 999
            endif

*
* GET THE VALUE OF THE 'BZERO' DESCRIPTOR
*
            call gtdscr(name,'BZERO','REAL',ival,DS_bze(frmid),cval,
     :                     ierr)
            if(ierr.ne.0) then
               call ptderr('BZERO')
               ierr=10
               goto 999
            endif

*
* GET THE VALUE OF THE 'BLANK' DESCRIPTOR
*
            call gtdscr(name,'BLANK','INTEGER',DS_bpx(frmid),rval,
     :                     cval,ierr)
            if(ierr.ne.0) then
               call ptderr('BLANK')
               ierr=10
               goto 999
            endif

*
* GET THE VALUE OF THE 'OBJECT' DESCRIPTOR
*
            call gtdscr(name,'OBJECT','CHARACTER',ival,rval,ctemp,ierr)
            if(ierr.ne.0) then
               call ptderr('OBJECT')
               ierr=10
               goto 999
            endif
            DS_obj(frmid)=ctemp(2:13)

*
* GET THE VALUE OF THE 'RA-REQ' DESCRIPTOR
*
            call gtdscr(name,'RA-REQ','CHARACTER',ival,rval,ctemp,ierr)
            if(ierr.ne.0) then
               call ptderr('RA-REQ')
               ierr=10
               goto 999
            endif
*
* EXTRACT THE VALUE FROM BETWEEN ANY MATCHED APOSTROPHIES.            
*
            ap1=index(ctemp,'''')
            if(ap1.ne.0) then
               ap2=index(ctemp(ap1+1:),'''')+ap1
               if(ap2.eq.0) then               
                  call wruser('*** Illegal format for descriptor '//
     :                        'RA-REQ: '//ctemp,ierr)
                  ierr=100
                  goto 999
               else
                  DS_rar(frmid)=ctemp(ap1+1:ap2-1)
               endif
            else                  
               DS_rar(frmid)=ctemp
            endif

*
* GET THE VALUE OF THE 'DEC-REQ' DESCRIPTOR
*
            call gtdscr(name,'DEC-REQ','CHARACTER',ival,rval,ctemp,ierr)
            if(ierr.ne.0) then
               call ptderr('DEC-REQ')
               ierr=10
               goto 999
            endif
*
* EXTRACT THE VALUE FROM BETWEEN ANY MATCHED APOSTROPHIES.            
*
            ap1=index(ctemp,'''')
            if(ap1.ne.0) then
               ap2=index(ctemp(ap1+1:),'''')+ap1
               if(ap2.eq.0) then               
                  call wruser('*** Illegal format for descriptor '//
     :                        'DEC-REQ: '//ctemp,ierr)
                  ierr=100
                  goto 999
               else
                  DS_dcr(frmid)=ctemp(ap1+1:ap2-1)
               endif
            else                  
               DS_dcr(frmid)=ctemp
            endif

*
* GET THE VALUE OF THE 'SOP' DESCRIPTOR
*
            call gtdscr(name,'SOP','INTEGER',DS_sop(frmid),rval,
     :                     cval,ierr)
            if(ierr.ne.0) then
               call ptderr('SOP')
               ierr=10
               goto 999
            endif

*
* GET THE VALUE OF THE 'UTC-EXP' DESCRIPTOR
*
            call rddscr(name,'UTC-EXP',1,ctemp,itemp,ierr)
            if(ierr.eq.0) call ctod(ctemp,DS_uex(frmid),ierr)
            if(ierr.ne.0) then
               call ptderr('UTC-EXP')
               ierr=10
               goto 999
            endif

*
* GET THE VALUE OF THE 'XSC-EXP' DESCRIPTOR
*
            call gtdscr(name,'XSC-EXP','REAL',ival,DS_xex(frmid),
     :                  cval,ierr)
            if(ierr.ne.0) then
               call ptderr('XSC-EXP')
               ierr=10
               goto 999
            endif

*
* GET THE VALUE OF THE 'UTC-1ST' DESCRIPTOR
*
            call rddscr(name,'UTC-1ST',1,ctemp,itemp,ierr)
            if(ierr.eq.0) call ctod(ctemp,DS_ust(frmid),ierr)
            if(ierr.ne.0) then
               call ptderr('UTC-1ST')
               ierr=10
               goto 999
            endif

*
* CALCULATE MODIFIED JULIAN DATE (JD-2400000.5) AT TIME OF FIRST SAMPLE
*
            DS_mjd(frmid)=4.46045D+4+DS_ust(frmid)/8.64D+4

*
* GET THE VALUE OF THE 'SAMPLES' DESCRIPTOR
*
            call gtdscr(name,'SAMPLES','INTEGER',DS_bsa(frmid),rval,
     :                  cval,ierr)
            if(ierr.ne.0) then
               call ptderr('SAMPLES')
               ierr=10
               goto 999
            endif

*
* CHECK THAT THE SIZE OF THE ARRAYS HOLDING THE BORESIGHT SAMPLES IS NOT
* LESS THAN THE NO. OF SAMPLES IN THE CRDD FILE
*
            if(DS_bsa(frmid).gt.DS_mxb) THEN
               call wrerr('BSSOVFLOW')
               ierr=35
               goto 999
            endif

*
* GET THE VALUE OF THE 'BUNIT' DESCRIPTOR
*
            call gtdscr(name,'BUNIT','CHARACTER',ival,rval,
     :                 DS_bru(frmid),ierr)
            if(ierr.ne.0) then
               call ptderr('BUNIT')
               ierr=10
               goto 999
            endif

*
* GET THE VALUES OF THE 'UTCS' DESCRIPTOR
*
            call rddscr(name,'UTCS',DS_bsa(frmid),bvalue,ival,ierr)
            if(ierr.eq.0) then

*
* CONVERT TO DOUBLE PRECISION VALUES
*
               do isamp = 1,DS_bsa(frmid)
                  call ctod(bvalue(isamp)(8:),utcs,ierr)

*
* CONVERT TO TIME OFFSET
*
                  DS_but(isamp,frmid) = sngl(utcs -
     :                                          DS_ust(frmid))
               end do
            endif
            if(ierr.ne.0) then
               call ptderr('UTCS')
               ierr=10
               goto 999
            endif

*
* GET THE VALUES OF THE 'PSI' DESCRIPTOR
*
            call rddscr(name,'PSI',DS_bsa(frmid),bvalue,ival,ierr)
            if(ierr.eq.0) then

*
* CONVERT TO REAL VALUES
*
               do isamp = 1,DS_bsa(frmid)
                  call ctor(bvalue(isamp)(8:),DS_psi(isamp,frmid),ierr)
               end do
            endif
            if(ierr.ne.0) then
               call ptderr('PSI')
               ierr=10
               goto 999
            endif

*
* GET THE VALUES OF THE 'NU' DESCRIPTOR
*
            call rddscr(name,'NU',DS_bsa(frmid),bvalue,ival,ierr)
            if(ierr.eq.0) then

*
* CONVERT TO REAL VALUES
*
               do isamp = 1,DS_bsa(frmid)
                  call ctor(bvalue(isamp)(8:),rval,ierr)

*
* CONVERT TO SUN ANGLE, THETA
*
                  DS_the(isamp,frmid) = rval+90.0
               end do
            endif
            if(ierr.ne.0) then
               call ptderr('NU')
               ierr=10
               goto 999
            endif

*
* GET THE VALUES OF THE 'SOLONG' DESCRIPTOR
*
            call rddscr(name,'SOLONG',DS_bsa(frmid),bvalue,ival,
     :                  ierr)
            if(ierr.eq.0) then

*
* CONVERT TO REAL VALUES
*
               do isamp = 1,DS_bsa(frmid)
                  call ctor(bvalue(isamp)(8:),DS_sol(isamp,frmid),
     :                      ierr)
               end do
            endif
            if(ierr.ne.0) then
               call ptderr('SOLONG')
               ierr=10
               goto 999
            endif

*
* GET THE VALUES OF THE 'LAMBDA' DESCRIPTOR
*
            call rddscr(name,'LAMBDA',DS_bsa(frmid),bvalue,ival,
     :                  ierr)
            if(ierr.eq.0) then

*
* CONVERT TO REAL VALUES
*
               do isamp = 1,DS_bsa(frmid)
                  call ctor(bvalue(isamp)(8:),DS_lam(isamp,frmid),
     :                      ierr)
               end do
            endif
            if(ierr.ne.0) then
               call ptderr('LAMBDA')
               ierr=10
               goto 999
            endif

*
* GET THE VALUES OF THE 'BETA' DESCRIPTOR
*
            call rddscr(name,'BETA',DS_bsa(frmid),bvalue,ival,ierr)
            if(ierr.eq.0) then

*
* CONVERT TO REAL VALUES
*
               do isamp = 1,DS_bsa(frmid)
                  call ctor(bvalue(isamp)(8:),DS_bet(isamp,frmid),
     :                      ierr)
               end do
            endif
            if(ierr.ne.0) then
               call ptderr('BETA')
               ierr=10
               goto 999
            endif

*
* FINISH
*
         endif
      endif

 999  continue

      end
