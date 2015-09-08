      subroutine grupdb(wktype,wkcon,dtype,v1,v2,v3,v4,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Updates the IRAS EDRS extension graphics database file
*
*
*
*
*SOURCE
*       GRUPDB.FOR in UTILITIES.TLB
*
*METHOD
*
*
*
*ARGUMENTS
*   INPUTS:
*
*   OUTPUTS:
*
*
*USED BY
*
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*
*       THIS PACKAGE (.TLB):
*
*       EDRS:
*
*       INTERIM:
*
*
*STARLINK PARAMETERS
*       /read/
*       /write/
*       /error/
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       %val
*       end of line comments
*       do while
*       2 byte integer values
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) //87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer   wktype,wkcon,dtype,ierr
      real      v1,v2,v3,v4
*
* DECLARE LOCAL VARIABLES
*
      integer   con     ! Temporary store for workstation type
      integer   dt      ! Temporary store for display type
      logical   more    ! True if database record for this device has
                        ! not been found yet
      integer   rec     ! Record index in database text file
      integer   type    ! temporary store for workstation type
      real      v1t     ! Temporary store for database variable 1
      real      v2t     ! Temporary store for database variable 2
      real      v3t     ! Temporary store for database variable 3
      real      v4t     ! Temporary store for database variable 4
*
* OPEN DATABASE TEXT FILE "IRASGRDB.DAT" IN USERS LOGIN DIRECTORY
*
      open(10,file='SYS$LOGIN:IRASGRDB.DAT',status='UNKNOWN',
     :     access='DIRECT',form='FORMATTED',recl=80,
     :     organization='RELATIVE',iostat=ierr)
      if(ierr.ne.0) goto 999
*
* LOOP THROUGH ALL RECORDS IN DATABASE FILE LOOKING FOR A RECORD
* WHICH REFERS TO THE CURRENT GRAPHICS DEVICE
*
      rec=0
      more=.true.
      do while(more)
         rec=rec+1
         read(10,rec=rec,fmt=10,iostat=ierr) type,con,dt,v1t,v2t,v3t,v4t
  10     format(3I5,4G13.6)
         if(ierr.eq.0) then
            if(type.eq.wktype.and.con.eq.wkcon) more=.false.
         else
            more=.false.
         endif
      enddo
*
* WRITE THE NEW VALUES OUT OVERWRITING ANY OLD RECORD FOR THIS DEVICE
*
      write(10,rec=rec,fmt=10,iostat=ierr) wktype,wkcon,dtype,v1,v2,v3,
     :                                     v4
*
* CLOSE FILE AND FINISH
*
 999  close(10)

      end
