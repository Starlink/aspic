      subroutine getdvn(name,commnt,ival,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To get a list of the available SGS device types.
*       This routine is called by SGS routine sgs_wname once for every
*       valid workstation type recognized by the node on which the
*       program is running (See SUN85 p33).
*
*SOURCE
*       GETDVN.FOR in UTILITIES.TLB
*
*METHOD
*       The list is stored in string cmdlist which is in common block
*       /getdev/. Workstation names are seperated by commas and the
*       list is terminated by a full stop.
*
*ARGUMENTS
*   INPUTS:
*       name    character       Workstation name
*       commnt  character       Descriptive comment string
*       ival    integer         Not used
*   OUTPUTS:
*       ierr    integer         Status value
*
*USED BY
*       CRDDTRACE
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               ustrln
*
*VAX SPECIFICS
*       implicit none
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 4/9/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      character*(*)     name,commnt
      integer           ival,ierr

*
* DECLARE LOCAL VARIABLES
*
      character*2000    cmdlst
      common /getdev/   cmdlst
      integer           ustrln,i,j
*
* ADD NEW DEVICE NAME TO END OF LIST OF LEGAL DEVICE NAMES
*
      i=ustrln(cmdlst)
      j=ustrln(name)

      cmdlst=cmdlst(:i)//name(:j)//','
*
* FINISH
*
      ierr=0

      end
