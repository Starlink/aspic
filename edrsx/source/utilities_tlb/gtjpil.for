      integer function gtjpil(codeid,signal,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Returns information about the current job or process by calling
*       system service SYS$GETJPI. This routine can only get information
*       which will fit in a 4 byte integer variable.
*
*SOURCE
*       GTJPIL.FOR in UTILITIES.TLB
*
*METHOD
*       Identify the required JPI code and call system service
*       SYS$GETJPI if the required code is valid.
*
*ARGUMENTS
*   INPUTS:
*       codeid  character       JPI code for the required information
*                               ( see info on lexical function F$GETJPI
*                                 in VMS manual DCL dictionary for an
*                                 explanation)
*       signal  integer         If 1 then any error from SYS$GETJPI is
*                               communicated to the user.
*   OUTPUTS:
*       ierr    integer         Error status: 0 - Success
*                                             1 - Invalid info code
*                                             2 - Error returned by
*                                                 SYS$GETJPI
*       (function value) integer The required information
*
*USED BY
*       PSFSTACK
*
*SUBROUTINES CALLED
*       VAX SYSTEM SERVICES:
*               SYS$GETJPI
*       VAX RUN TIME LIBRARY:
*               lib$signal
*       VAX FORTRAN
*               mvbits
*
*VAX SPECIFICS
*       implicit none
*       %val
*       end of line comments
*       2 byte integer values
*       uses RTL and system services
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 22/11/87
*-------------------------------------------------------------------
*
      implicit none
*
* INCLUDE SYMBOLIC DEFINITIONS OF $GETJPI CODES AND CONDITION CODE
* FIELD NAMES
*
      include '($jpidef)'
      include '($stsdef)'
*
* DECLARE ARGUMENTS
*
      character codeid*(*)
      integer   ierr,signal
*
* DECLARE LOCAL VARIABLES
*
      integer*2 blen
      integer   bufad
      integer   buffer
      integer*2 code
      integer   lstend
      integer   retad
      integer   sys$getjpi
      common /itemlist/ blen,code,bufad,retad,lstend
*
* IDENTIFY CODE
*
      if(codeid.eq.'APTCNT') then
         code=jpi$_aptcnt
      else if(codeid.eq.'ASTCNT') then
         code=jpi$_astcnt
      else if(codeid.eq.'ASTLM') then
         code=jpi$_astlm
      else if(codeid.eq.'AUTHPRI') then
         code=jpi$_authpri
      else if(codeid.eq.'BIOCNT') then
         code=jpi$_biocnt
      else if(codeid.eq.'BIOLM') then
         code=jpi$_biolm
      else if(codeid.eq.'BUFIO') then
         code=jpi$_bufio
      else if(codeid.eq.'BYTCNT') then
         code=jpi$_bytcnt
      else if(codeid.eq.'BYTLM') then
         code=jpi$_bytlm
      else if(codeid.eq.'CPULIM') then
         code=jpi$_cpulim
      else if(codeid.eq.'CPUTIM') then
         code=jpi$_cputim
      else if(codeid.eq.'DFPFC') then
         code=jpi$_dfpfc
      else if(codeid.eq.'DFWSCNT') then
         code=jpi$_dfwscnt
      else if(codeid.eq.'DIOCNT') then
         code=jpi$_diocnt
      else if(codeid.eq.'DIOLM') then
         code=jpi$_diolm
      else if(codeid.eq.'DIRIO') then
         code=jpi$_dirio
      else if(codeid.eq.'ENQCNT') then
         code=jpi$_enqcnt
      else if(codeid.eq.'ENQLM') then
         code=jpi$_enqlm
      else if(codeid.eq.'FILCNT') then
         code=jpi$_filcnt
      else if(codeid.eq.'FILLM') then
         code=jpi$_fillm
      else if(codeid.eq.'FREPTECNT') then
         code=jpi$_freptecnt
      else if(codeid.eq.'GPGCNT') then
         code=jpi$_gpgcnt
      else if(codeid.eq.'GRP') then
         code=jpi$_grp
      else if(codeid.eq.'IMAGECOUNT') then
         code=jpi$_imagecount
      else if(codeid.eq.'JOBPRCCNT') then
         code=jpi$_jobprccnt
      else if(codeid.eq.'JOBTYPE') then
         code=jpi$_jobtype
      else if(codeid.eq.'MASTER_PID') then
         code=jpi$_master_pid
      else if(codeid.eq.'MEM') then
         code=jpi$_mem
      else if(codeid.eq.'MODE') then
         code=jpi$_mode
      else if(codeid.eq.'OWNER') then
         code=jpi$_owner
      else if(codeid.eq.'PAGEFLTS') then
         code=jpi$_pageflts
      else if(codeid.eq.'PAGFILCNT') then
         code=jpi$_pagfilcnt
      else if(codeid.eq.'PGFLQUOTA') then
         code=jpi$_pgflquota
      else if(codeid.eq.'PID') then
         code=jpi$_pid
      else if(codeid.eq.'PPGCNT') then
         code=jpi$_ppgcnt
      else if(codeid.eq.'PRCCNT') then
         code=jpi$_prccnt
      else if(codeid.eq.'PRCLM') then
         code=jpi$_prclm
      else if(codeid.eq.'PRI') then
         code=jpi$_pri
      else if(codeid.eq.'PRIB') then
         code=jpi$_prib
      else if(codeid.eq.'PROC_INDEX') then
         code=jpi$_proc_index
      else if(codeid.eq.'SIRESPEC') then
         code=jpi$_sitespec
      else if(codeid.eq.'STATE') then
         code=jpi$_state
      else if(codeid.eq.'SWPFILLOC') then
         code=jpi$_swpfilloc
      else if(codeid.eq.'TMBU') then
         code=jpi$_tmbu
      else if(codeid.eq.'TQCNT') then
         code=jpi$_tqcnt
      else if(codeid.eq.'TQLM') then
         code=jpi$_tqlm
      else if(codeid.eq.'UIC') then
         code=jpi$_uic
      else if(codeid.eq.'VIRTPEAK') then
         code=jpi$_virtpeak
      else if(codeid.eq.'VOLUMES') then
         code=jpi$_volumes
      else if(codeid.eq.'WSAUTH') then
         code=jpi$_wsauth
      else if(codeid.eq.'WSAUTHEXT') then
         code=jpi$_wsauthext
      else if(codeid.eq.'WSEXTENT') then
         code=jpi$_wsextent
      else if(codeid.eq.'WSPEAK') then
         code=jpi$_wspeak
      else if(codeid.eq.'WSQUOTA') then
         code=jpi$_wsquota
      else if(codeid.eq.'WSSIZE') then
         code=jpi$_wssize
      else
         ierr=1
         goto 999
      endif
*
* SET UP REST OF ITEMLIST FOR $GETJPI
*
      blen=4
      bufad=%loc(buffer)
      retad=0
      lstend=0
*
* CALL SYSTEM SERVICE $GETJPI TO GET THE INFORMATION
*
      ierr=sys$getjpi(,,,blen,,,)
*
* IF NOT SUCCESSFUL, MODIFY THE CONDITION CODE TO BE AN INFORMATIONAL
* MESSAGE SO THAT PROGRAM EXECUTION WILL CONTINUE AND SIGNAL THE
* CONDITION TO THE USER.
*
      if(.not.ierr) then
         if(signal.ne.0) then
            call mvbits(STS$K_INFO,0,3,ierr,0)
            call lib$signal(%val(ierr))
         endif
         ierr=2
      else
         ierr=0
      endif
*
* RETURN BUFFER VALUE AND FINISH
*
 999  gtjpil=buffer

      end
