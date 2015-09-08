      subroutine args_rdim (acx,acy,asx,asy,psx,psy,status)

*+  Read latest record of type 'IMAG' from ARGS database: image has been drawn
*   on ARGS centre '(acx,acy)' size  '(asx,asy)' in ARGS units, size
*   '(px,py)' in pixel units.
*   Status returns are 0 for success, 1 for "no such record", 2 for "error
*   opening database file".

      include 'ASPIC(dbparms)'
      include 'ASPIC(dbtype)'
      include 'ASPIC(dblnam)'

      integer acx,acy,asx,asy,psx,psy,status,idmax,id
      character type*(TYPLEN)

*   open database file
      call args_dopdb (ALGNAM,status)
      if (status.eq.0) then

*       determine number of records in the file and read last one
          call args_qidmx (idmax)
          if (idmax.eq.0) then
              status = 1
          else
              do id = idmax,id,-1
                  call args_qdb7p (id,type,acx,acy,asx,asy,psx,psy,
     :              status)
                  if (type.eq.'IMAG') then
                      return
                  endif
              enddo
              status = 1
          endif

      endif

*   don't need to close database as have only read from it

      end
