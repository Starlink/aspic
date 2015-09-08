      subroutine args_getp (id,parid,info,status)

*+  Read 'info' from record 'id'. Position of 'info' is determined by
*   'parid'. If 'parid' is illegal (ie greater than DVPARB) then nothing
*   is done. Status returns are those from 'args_rddb', ie 0 for
*   success and 1 or 2 for failure.

      include 'ASPIC(dbparms)'

      integer id,parid,status
      character info*(*),line*(RECLEN)

      if (1.le.parid.and.parid.le.DVPARB) then
          call args_rddb (id,line,status)
          if (status.eq.0) then
              info = line(offset(parid):min(RECLEN,offset(parid)+
     :               length(parid)-1))
          else
              info = ' '
          endif
      endif

      end
