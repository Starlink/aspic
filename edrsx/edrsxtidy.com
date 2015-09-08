$!  
$!  -------------------------------------------------------
$!  
$!   This command procedure clears up the logical names and
$!   the global symbols that were used when running the EDRSX
$!   package of programs via DSCL. It is a copy of the DSCL
$!   PAKTIDY procedure with the EDRSX specific tidying
$!   "hard wired". While in EDRSX, DSCL_PAKDIR points to the
$!   directory containing EDRS, (not EDRSX) thus the TIDY procedure
$!   from EDRS would normally be called by PAKTIDY when shutting down
$!   EDRSX. To enable EDRSX to shut down neatly, the symbol PAKTIDY
$!   is made to point to this procedure rather than the version of 
$!   PAKTIDY in DSCLDIR.
$!  
$!  -------------------------------------------------------
$!  
$!   Deassign logical names associated with programs and procedures
$!
$   on error then continue
$   deassign bdfgen
$   deassign bigconv
$   deassign convolve 
$   deassign crddblur
$   deassign crddsample
$   deassign crddtrace
$   deassign destripe
$   deassign datarange
$   deassign drawscan 
$   deassign edrsin 
$   deassign fixinval
$   deassign fourier
$   deassign histogram 
$   deassign imgedit
$   deassign imgstack
$   deassign irasback
$   deassign irascoef
$   deassign irascorr
$   deassign irasdscr
$   deassign irasin
$   deassign irasshift
$   deassign itfhist
$   deassign maskgen
$   deassign matchback
$   deassign memask
$   deassign memcrdd
$   deassign radec
$   deassign scatter
$   deassign simcrdd 
$   deassign blink
$   deassign irasstack
$   deassign skysub
$   deassign sourcefit
$   deassign weightgen
$   deassign xyplot
$   deassign ndfout
$!
$!   Delete global symbols
$!
$   delete/symbol/global batch
$!  
$!   Reset the help library searching to the normal ASPIC system
$!  
$   deassign edrsxhelp
$   deassign hlp$library
$   deassign edrshelp
$   deassign hlp$library_1
$   deassign aspichelp
$   deassign hlp$library_2
$   deassign ipmafhelp
$   deassign hlp$library_3
$   deassign hlp$library_4
$   deass pakhelp
$   define hlp$library asp
$   define hlp$library_1 vms
$   h*elp:==help/nolibrary/page
$!  
$!   Deassign the logical name dscl_pakdir to stop searching
$!   of the package directory
$!  
$   deassign dscl_pakdir
$!  
$!   Deassign the pak> prompt and reinstate the Dscl> prompt
$!  
$   deassign dscl_prompt
$   define dscl_prompt "Dscl>"
$!
$! Make the symbol PAKTIDY point to the version in DSCLDIR
$!
$   paktidy:==@dscl_comdir:paktidy
$   exit
