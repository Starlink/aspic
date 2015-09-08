      subroutine args_putp (id,parid,info,status)

*+  Put 'info' to record 'id'. Position of info is determined by
*   'parid'. If 'parid' is illegal (ie greater than DVPARB) then nothing
*   is done. If 'id' is out of range then a new record is written.
*   Status returns are those from 'args_wrdb', ie 0 for success and 1
*   or 2 for failure.

      include 'ASPIC(dbparms)'

      integer id,parid,status
      character info*(*),line*(RECLEN)

      if (1.le.parid.and.parid.le.DVPARB) then
        call args_rddb (id,line,status)
        line(offset(parid):min(RECLEN,offset(parid)+
     :    length(parid)-1)) = info
        call args_wrdb (id,line,status)
      endif

      end
