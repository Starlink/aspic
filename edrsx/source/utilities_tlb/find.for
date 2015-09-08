      subroutine find(file,type,diry,status)

*
*+      SUBROUTINE FIND
*
*SOURCE
*       FIND.FOR IN UTILITIES.TLB
*
*       PART OF THE DCL PSEUDO-STARLINK ENVIRONMENT
*
*       ATTEMPTS TO LOCATE A FILE BY SEARCHING:-
*               1       CURRENT DIRECTORY
*               2       DSCL_LOCDIR: (FOR LOCALLY MODIFIED PROGS)
*               3       DSCL_PAKDIR: (FOR PACKAGES)
*               4       DSCL_'TYPE'DIR: (FOR GENERAL PROGS)
*               5       LOGICAL NAME TABLES
*
*       ARGUMENTS
*               FILE    CHARACTER       FILE NAME
*               TYPE    CHARACTER       FILE TYPE
*               DIRY    CHARACTER       LOCATED DIRECTORY
*               STATUS  INTEGER         ERROR CODE
*
*       IF A LOGICAL NAME IS TRANSLATED, THEN FILE MAY
*       BE OVERWRITTEN WITH A VALUE EXTRACTED FROM THE EQUIVALENCE
*       NAME - BEWARE! (only if required file actually exists, 26/8/87)
*
*       STATUS VALUES USED ARE 0 FOR SUCCESS AND 1 FOR FAILURE
*
*       W F LUPTON RGO FEB 1981
*
*       MODIFIED BY D.S. BERRY (26/8/87) TO RETURN FAIL STATUS IF
*       A LOGICAL NAME TRANSLATION POINTS TO A NON-EXISTANT FILE
*
      implicit integer (a-z)
      character*(*) file,type,diry
      character*30 tmpfil

      if (file.eq.' ') then
         status=1
         goto 999

      endif


*
*       CURRENT DIRECTORY
*
      call exists(file//'.'//type,status)

      if (status.eq.0) then
         diry=' '
         goto 999

      endif


*
*       DSCL_LOCDIR
*
      call exists('DSCL_LOCDIR:'//file//'.'//type,status)

      if (status.eq.0) then
         diry='DSCL_LOCDIR:'
         goto 999

      endif


*
*       DSCL_PAKDIR
*
      call exists('DSCL_PAKDIR:'//file//'.'//type,status)

      if (status.eq.0) then
         diry='DSCL_PAKDIR:'
         goto 999

      endif


*
*       DSCL_'TYPE'DIR
*
      call exists('DSCL_'//type//'DIR:'//file//'.'//type,status)

      if (status.eq.0) then
         diry='DSCL_'//type//'DIR:'
         goto 999

      endif


*
*       TRANSLATE LOGICAL NAME
*
      call translog(file,diry,status)

      if (status.eq.0) then

*
*       EXTRACT DISK/DIRY INTO DIRY, FILE NAME INTO FILE
*
         end_dir=index(diry,']')
         end_log=index(diry,':')
         end_dir_log=max(end_dir,end_log)
         end_nam=index(diry(end_dir_log+1:)//'.','.')+end_dir_log

         if (end_nam.gt.len(diry).or.diry(end_nam+1:).eq.type) then
            tmpfil=diry(end_dir_log+1:end_nam-1)

            if (end_dir_log.eq.0) then
               diry=' '

            else
               diry=diry(:end_dir_log)
            endif
            call exists(diry//tmpfil//'.'//type,status)
            if(status.eq.0) then
               file=tmpfil
            else
               diry=' '
               status=1
            endif

         else
            diry=' '
            status=1
         endif


      else
         diry=' '
      endif


*
*       AND RETURN
*
999   return

      end



