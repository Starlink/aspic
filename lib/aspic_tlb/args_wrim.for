      subroutine args_wrim (acx,acy,asx,asy,psx,psy,status)

*+  Write new record to ARGS database: image type 'IMAG' has been drawn
*   on ARGS centre '(acx,acy)' size  '(asx,asy)' in ARGS units, size
*   '(psx,psy)' in pixel units. Any obscured images will be deleted with
*   consequent renumbering. Status returns are 0 for success, 2 for "error
*   opening database file" and 6 for "illegal size".

      include 'ASPIC(dbparms)'
      include 'ASPIC(dblnam)'

      integer acx,acy,asx,asy,psx,psy,status,args_loc,idmax,tstat
      real ptou(6)
      data ptou/1.0,0.0,0.0,1.0,0.0,0.0/

*   check that all sizes are positive
      if (asx.le.0.or.asy.le.0.or.psx.le.0.or.psy.le.0) then
          status = 6
      else

*       open database file
          call args_dopdb (ALGNAM,status)
          if (status.eq.0) then

*           determine number of records in the file and write next one
              call args_qidmx (idmax)
              id = idmax + 1
              call args_tdb7p (id,'IMAG',acx,acy,asx,asy,psx,psy,
     :          status)
              if (status.eq.0) then
 
*               ensure pixel -> user transformation is the identity
                  call args_tdbf (id,'PTOU',ptou,6,status)
                  if (status.eq.0) then

*                   remove any images which are obscured by the new one
                      call args_tidy (id,status)

                  endif

              endif

          endif

      endif

*   close database file (don't report status)
      call args_cldb (tstat)

      end
