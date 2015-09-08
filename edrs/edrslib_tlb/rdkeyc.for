      subroutine rddata(name,format,type,size,pntr,status)
 
*++
*     RDDATA - Read Data frame
*
*     This routine is used to map an existing bulk data frame into
*     memory and return an address pointer to it. If the format of
*     the data is not that requested by the user, then automatic
*     conversion will occur. If the user wishes to be informed that
*     a conversion has taken place, he can specify the format code
*     as its corresponding negative value and the status return will
*     be set accordingly.
*
*     A frame that has been accessed by RDDATA must be released,
*     after processing, by a call to FRDATA.
*
*     (NOTE that access violation will occur if any attempt is made
*     to modify frame data that is marked for input; the memory
*     space allocated to the frame data is read-only protected).
*
*     Applications programs that read 'IMAGE' type frames should
*     use subroutine RDIMAG and not RDDATA.
*
*     CALL RDDATA(NAME,FORMAT,TYPE,SIZE,PNTR,STATUS)
*
*     Input arguments:
*     ----------------
*     NAME:    CHARACTER expression:    Parameter name of data frame
*     FORMAT:  INTEGER expression:      Data format required
*
*     Output arguments:
*     -----------------
*     TYPE:    CHARACTER variable:      Bulk data type
*     SIZE:    INTEGER variable:        Number of data values
*     PNTR:    INTEGER variable:        Pointer to data in memory
*     STATUS:  INTEGER variable:        Status return value
*
*
*     D.PEARCE  29/JUN/80  VERSION #2
*--
*
      implicit      integer(a-z)
 
*
      character*(*) name,type
      integer*4     format,size,pntr,status
 
*
      include 'interim(errpar)'
 
*
*
*     .....access bulk data frame
      call stl_accfrm(name,entry,status)
 
*
*     .....read frame if accessible
 
      if (status.eq.err_normal)call stl_rdfrm(entry,format,type,size
     : ,pntr,status)
 
*
*     .....check error status
 
      if (status.ne.err_normal) call stlerr('RDDATA',status)
 
*
*
      return
 
      end
 
 
 
