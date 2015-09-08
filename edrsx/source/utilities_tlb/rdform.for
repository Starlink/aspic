      Subroutine rdform(name,format,status)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Returns a flag for each recognized data format set true
*       if the frame associated with parameter 'name' has an
*       incarnation of that data format.
*
*SOURCE
*       RDFORM.FOR in UTILITIES.TLB
*
*METHOD
*       The method is copied from the interim routine RDDATA
*
*ARGUMENTS
*   INPUTS:
*       NAME    character       Parameter name of data frame
*   OUTPUTS:
*       FORMAT  logical array:  Each element is set .TRUE. if there
*                               exists an incarnation with the
*                               corresponding data type:
*                               1: signed byte
*                               2: signed word
*                               3: signed longword
*                               4: real
*                               5: double precision
*                               6: unsigned byte
*                               7: unsigned word
*       STATUS  integer         Status return value
*
*USED BY
*       All programs that use GTFMTS
*
*SUBROUTINES CALLED
*       EDRS:
*               STLERR
*       INTERIM:
*               STL_ACCFRM
*               STL_RVB
*               STL_WVB
*       VMS
*               SYS$ASCTIM
*
*STARLINK PARAMETERS
*       'name'/read/    Character variable name contains name of parameter
*                       used for accessing the data frame
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 26/8/87
*       (based on INTERIM routine RDDATA)
*-------------------------------------------------------------------
*
      IMPLICIT      INTEGER(A-Z)
*
      CHARACTER*(*) NAME
      INTEGER*4     STATUS,FMT_CODES(7)
      LOGICAL FORMAT(7)
*
      CHARACTER*1   DUMMY
*
      INCLUDE 'INTERIM(PCTCOM)'
      INCLUDE 'INTERIM(FCBCOM)'
      INCLUDE 'INTERIM(LDBCOM)'
      INCLUDE 'INTERIM(ERRPAR)'
*
      DATA FMT_CODES/101,102,104,204,208,301,302/
*
*
*     .....access bulk data frame
      CALL STL_ACCFRM(NAME,ENTRY,STATUS)
      IF (STATUS.NE.ERR_NORMAL) GOTO 60
*
*     .....read Frame Control and first Local Descriptor Block
      CALL STL_RVB(PCT_IOCHAN(ENTRY),FCB,1,IOSTAT)
      CALL STL_RVB(PCT_IOCHAN(ENTRY),LDB,FCB_PTRLDB,IOSTAT)
*
*     .....update Frame Control Block (if allowed)
      IF (.NOT.PCT_RDONLY(ENTRY)) THEN
         CALL SYS$ASCTIM(,FCB_ACCESS,,)
         CALL STL_WVB(PCT_IOCHAN(ENTRY),FCB,1,IOSTAT)
      ENDIF
*
*
*     .....read list of data types
      DO I=1,7
         FORMAT(I)=.FALSE.
      ENDDO
      DO I=1,FCB_INCARN
         DO J=1,7
            IF(FCB_INCSEG(2,I).EQ.FMT_CODES(J)) FORMAT(J)=.TRUE.
         ENDDO
      ENDDO
*
*     .....check for error status
   60 IF (STATUS.NE.ERR_NORMAL) CALL STLERR('RDFORM',STATUS)
*
*
   70 RETURN
      END
