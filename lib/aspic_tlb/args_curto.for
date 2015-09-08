      subroutine args_curto (crtype,id,ib,ux,uy)

*+  Read cursor from ARGS and return position in '(ux,uy)' in user units.
*   There is a timeout if the tracker ball is not moved in a set time
*   'crtype' is the type of image being looked for. Type 1 is a match-all
*   type (assumed if 'crtype' is illegal).
*   'id' is id of image on which cursor is located (0 if not found).
*   'ib' identifies the button which was pressed (0 if error reading cursor or
*   if there are no images displayed). The ARGS is assumed to have been
*   assigned by the caller. (If not, ....****!) NB This routine assumes a
*   knowledge of the various image types.

      include 'ASPIC(dbparms)'
      include 'ASPIC(dbtype)'
      include 'ASPIC(dblnam)'

      integer id,ax,ay,ib,status,idmax,rtype,i,args_loc
      real ux,uy
      logical args_insid
      character crtype*(*),uptype*(TYPLEN),type*(TYPLEN)

      ib = 0

*   open database file
      call args_dopdb (ALGNAM,status)
      if (status.ne.0) then
          id = 0
      else

*       determine number of records in file (must be > 0)
          call args_qidmx (idmax)
          if (idmax.eq.0) then
                id = 0
          else

*           now do the actual ARGS interaction ('ib' = 0 indicates error)
              call args_lrdcto (ib,ax,ay)
              if (ib.eq.0) then
                id = 0
              else

*               validate requested type (ensure upper case)
                  call str$upcase (uptype,crtype)
                  rtype = max (args_loc (types,NTYPES,uptype),1)

*               go through images in reverse order
                  do id = idmax,1,-1
                      if (args_insid (id,ax,ay)) then
*                       check image type (if not ok, treat as being
*                       transparent and continue the search)
                          call args_qdbc (id,'TYPE',1,type,i,status)
                          i = args_loc (types,NTYPES,type)
                          if ((rtype.ne.1.and.(i.eq.1.or.i.eq.rtype))
     :                    .or.(rtype.eq.1.and.i.ne.0)) then
*                           force loop to terminate (image located)
                              goto 10
                          endif
                      endif
                  enddo
*               ensure id is 0 (image not found)
                  id = 0

*               if have located image, convert to user units
   10             if (id.eq.0) then
                      ux = 0.0
                      uy = 0.0
                  else
                      call args_atou (id,ax,ay,ux,uy,status)
*                   if error converting, indicate with 'ib'
                      if (status.ne.0) then
                          ib = 0
                      endif
                  endif

              endif
 
          endif

      endif

*   don't need to close database as have only read from it

      end
