$!
$! REORGANISE.COM - Reorganises the files in EDRSX into the correct 
$!		    directory structure (See the header of file 
$!		    DEVSYS.COM for a description of the correct
$!                  structure). 
$!----------------------------------------------------------------------
$!
$   on warning then exit
$!
$! Ensure correct default directory
$!
$   top=f$trnlnm("EDRSX")
$   if top.eqs."" then goto no_edrsx
$   set def 'top'
$!
$! Move all text libraries to source subdirectory
$!
$   write sys$output "   Moving files to [.SOURCE] subdirectory"
$   rename *.tlb [.source]*.*
$!
$! Also move template source files to source subdirectory
$!
$   rename template.con [.source]*.*
$   rename template.for [.source]*.*
$   rename template.hlp [.source]*.*
$!
$! Now move all object libraries to object subdirectory
$!
$   write sys$output "   Moving files to [.OBJECT] subdirectory"
$   rename *.olb [.object]*.*
$!
$! Move all command procedures to procs subdirectory
$!
$   write sys$output "   Moving files to [.PROCS] subdirectory"
$   rename devsys.com;6 devsys.tmp;6
$   rename *.com [.procs]*.*
$!
$! Now move back the command procedures not associated with software
$! developement (i.e. the ones a normal user may want to use).
$!
$   rename [.procs]batch.com []*.*
$   rename [.procs]edrsxtidy.com []*.*
$   rename [.procs]rebuild.com []*.*
$   rename devsys.tmp;6 devsys.com;6
$!
$! Now move all build (link) files to build subdirectory
$!
$   write sys$output "   Moving files to [.BUILD] subdirectory"
$   rename *.bld [.build]*.*
$!
$! Purge all directories
$!
$   purge
$   purge [.build]
$   purge [.source]
$   purge [.object]
$   purge [.procs]
$!
$! Finished
$!
$   write sys$output "   FINISHED"
$   exit
$!
$no_edrsx:
$!
$! If log name EDRSX is not defined stop
$!
$   write sys$output "Logical name EDRSX not defined"
$   exit
