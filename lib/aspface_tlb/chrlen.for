*+   CHRLEN - Find length of string.
      SUBROUTINE CHRLEN (STRING,LENGTH)
*    Description :
*     Find length of string according to the Fortran 77 convention
*     that trailing blanks are ignored.
*    Invocation :
*     CALL CHRLEN (STRING;LENGTH)
*    Parameters :
*     STRING = CHARACTER*(*) (READ)
*           String to be measured.
*     LENGTH = INTGER (WRITE)
*           Length of the string ignoring trailing blanks.
*    Method :
*     Jack's original method :
*      Start from string size (CHR_SIZE) and work back to start until
*      first non-blank character is found.
*     This is rather slow so a single call to a Vax utility is used.
*     The utility copies the given string to a local string, removing
*     trailing blank and tab characters in the process, and returns
*     the number of characters copied into the local string. The local
*     string, of course, has to be able to hold anythiing up to the
*     largest string possible on the Vax. This may be rather expensive
*     in its use of space. If it is, a shorter local string could be used
*     and the status returned from the vax utility could be used to check
*     for truncation and if truncation has occured then Jack's method used.
*    Deficiencies :
*     Can only properly handle strings up to a predefined length.
*    Authors :
*     Jack Giddings   (UCL::JRG)
*     Dave Baines     (ROE::ASOC5)
*     A C Davenhall   (ROE::ACD)
*    History :
*     03/06/1983 : Original version                        (UCL::JRG)
*     14/06/1984 : Optimised version                       (ROE::ASOC5)
*     27/8/84    : Version for the ASPIC interface library (ROE::ACD)
*                  The following changes were made;
*                  1. Converted to a subroutine.
*                  2. All references to the SSE include 
*                     files removed.
*    Type Definitions :
      IMPLICIT NONE
*    Import :
      CHARACTER*(*)
     :  STRING      ! string to be measured.
*    Export :
      INTEGER
     :  LENGTH      ! Length found for the string (exc. trail. blanks).
*    External references :
!      INTEGER CHR_SIZE ! returns character string size used in Jack's method
*    Local constants :
      INTEGER
     :  MAXLEN      ! maximum string length that can be handled.
      PARAMETER
     : (MAXLEN=65535)
*    Local variables :
      CHARACTER*( MAXLEN )
     :  LOCAL       ! Contains STRING without trailing blanks and tabs.
      INTEGER
     :  DECLEN      ! declared length of the imported string.
*-

! Jack's original code :      
!      chr_len = chr_size(str)
!      dowhile (chr_len .gt. 0)
!         if (str(chr_len:chr_len) .ne. ' ') then
!            goto 1
!         endif
!         chr_len = chr_len - 1
!      enddo
! 1    continue

*
*    get the declared length of the string to be truncated

      DECLEN = LEN(STRING)

*
*    copies STRING to LOCAL removing trailing blanks and tabs and
*    returns the number of characters copied in LENGTH. If truncation
*    occurs LENGTH will correspond to the maximum number of characters
*    that can be held in LOCAL.

      CALL STR$TRIM (LOCAL(1:DECLEN),STRING,LENGTH)

      END
