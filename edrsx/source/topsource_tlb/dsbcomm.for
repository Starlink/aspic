      program dsbcomm

*
* +      PROGRAM DSCLCOMM (RENAMED DSBCOMM TEMPORARILY)
*
*        PART OF THE DCL PSEUDO-STARLINK ENVIRONMENT
*
*        PROGRAM TO ANALYZE AND PERFORM A DSCL COMMAND
*
*        W F LUPTON RGO JUNE 1981
*
*        MODIFIED BY DS BERRY TO USE HIS GETFOR ROUTINE FOR GETTING
*        COMMANDS FROM THE USER INSTEAD OF LIB$GET_FOREIGNB. GETFOR
*        PROVIDES DCL-LIKE RECALL FACILITIES (30/9/88)
*
      implicit integer (a-z)
      logical echo
      include 'scllib(dsclchans.inc)'
      character incomm*255,command*255,commname*80,commchar*1,par(20)
     : *8,val(20)*80,prog_dir*80,qual(20)*40,ex*4
      character*40 outext

*    Modified by KFH 22-12-82 to pick up a logical name
*    to be used as the prompt string
      character*8 prompt
      data ex,incomm/'EXIT',' '/

*
*        RETRIEVE THE PROGRAM LINE USING GETFOR (LIKE LIB$GET_FOREIGN
*        EXCEPT THAT IT PROVIDES RECALL FACILITIES)
*

      do while (incomm.eq.' ')
         call lib$sys_trnlog('DSCL_PROMPT',,prompt,,,)
         call getfor(incomm,prompt)
      enddo


*
*        REMOVE LEADING AND TRAILING SPACES, TABS ETC AND CONVERT TO UPPER CASE
*
      call strtid(incomm,command)

*
*        DO THE INITIALISATION THAT IS REQUIRED ON EACH COMMAND
*
      call comminit(command,.true.,commname,qual,qual_num,commchar,par
     : ,val,par_num,status)

      if (status.ne.0) then
         status=40

      else

*
*                 CHECK WHETHER IT IS A DSCL PROCEDURE
*

         if (commchar.eq.'@') then
            call prcrun(commname,qual(1),par,val,par_num,status)

            if (status.ne.0) then
               commchar='*'
            endif

         endif


*
*                 CHECK WHETHER IT IS A STARLINK PROGRAM
*

         if (commchar.eq.'*') then
            call find(commname,'EXE',prog_dir,status)

            if (status.eq.0) then
               call gen_setsym('DSCL_PROGRAM',prog_dir(:leng(prog_dir))
     :          //commname(:leng(commname))//'.EXE')

*
*                 Output message to terminal saying which
*                 program is being loaded.
*

               if (index(prog_dir,'EXEDIR').eq.0) then

                  if (leng(prog_dir).ne.0) then

                     if (index(prog_dir,'LOCDIR').ne.0) then
                        outext='* LOCAL PROGRAM *'

                     else if (index(prog_dir,'PAKDIR').eq.0) then
                        outext='* PRIVATE PROGRAM *'
                     end if


                  else
                     outext='* PRIVATE PROGRAM *'
                  end if

                  call wruser(outext,istat)
               end if

               call stlrun(commname,par,val,par_num,status)

               if (status.ne.0) then
                  call wruser('Invalid STARLINK program call',status)
                  status=40
               endif


            else
               commchar='$'
            endif

         endif


*
*                 TRY AS DCL COMMAND (LAST RESORT)
*

         if (commchar.eq.'$') then

            if (command(:leng(command)).eq.ex(:leng(command))) then
               command='STOP'
            endif

            status=lib$do_command('$'//command(:leng(command)))

            if (.not.status) then
               call wruser('Invalid DCL command',status)
               status=40
            end if

         endif

      endif


*
*        RETURN ERROR STATUS (40 JUST GIVES A REASONABLY INNOCUOUS MESSAGE)
*
      call exit(status)

      end



