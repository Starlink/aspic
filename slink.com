$!  =============================================================================
$!        Link command file for linking software using the distributed
$!        ASPIC libraries.
$!  =============================================================================
$!       N.B. The FINGS and RGOLIB libraries both contain entries for CURSOR
$!            The RGOLIB CURSOR entry is for SIMPLPLOT programs and therefore
$!            when linking these programs the libraries required for linking
$!            should be explicitly named.
$!  =============================================================================
$!  
$!    Set up error control
$!  
$   set noon
$   set message/nofacility/noseverity/noident/notext
$   on control_y then goto clean_up
$!  
$!    Set up logical names to the library directories
$!  
$   define libs asplib
$!  
$!    Check the module to be linked was specified
$!  
$   if p1 .eqs. "" then inquire p1 "Main program object file ?"
$!  
$!    Check if all libraries are to be used in the link
$!  
$   if "''P2'" .eqs. "" then goto link_all
$!  
$!    Set up the link list for this program
$!  
$   lib = ""
$   p2length = f$length(p2)
$   if f$locate("ROEASP",p2) .ne. p2length then lib = "''LIB',LIBS:ROEASP/LIB"
$   if f$locate("ASPFACE",p2) .ne. p2length then lib = "''LIB',LIBS:ASPFACE/LIB"
$   if f$locate("FINGS",p2) .ne. p2length then lib = "''LIB',LIBDIR:FINGS/LIB"
$   if f$locate("ROEARGS",p2) .ne. p2length then lib = "''LIB',LIBS:ROEARGS/LIB"
$   if f$locate("GRASP",p2) .ne. p2length then lib = "''LIB',LIBS:GRASP/LIB"
$   if f$locate("RGOLIB",p2) .ne. p2length then lib = "''LIB',LIBS:RGOLIB/LIB"
$   if f$locate("ASPIC",p2) .ne. p2length then lib = "''LIB',LIBS:ASPIC/LIB"
$   if f$locate("EDRSLIB",p2) .ne. p2length then lib = "''LIB',LIBS:EDRSLIB/LIB"
$   if f$locate("NAG",p2) .ne. p2length then lib = "''LIB',NAG_LIB/LIB"
$   if f$locate("INTERIM",p2) .ne. p2length then lib = "''LIB',INTERIM/OPT"
$   if f$locate("TAPEIO",p2) .ne. p2length then lib = "''LIB',LIBDIR:TAPEIO/LIB"
$   if f$locate("SGS",p2) .ne. p2length then lib = "''LIB',LIBDIR:SGS/LIB"
$   if f$locate("HIGR",p2) .ne. p2length then -
       lib = "''LIB',LIBDIR:HIGR/LIB/INC=(HIGR_DRDATA,HIGR_CNDATA,HIGR_H3DATA)"
$   if f$locate("GKS",p2) .ne. p2length then lib = "''LIB',GKSOPT/OPT"
$   if f$locate("ARGS",p2) .ne. p2length .and. -
       f$locate("GKS",p2) .eq. p2length then lib = "''LIB',ARGSOPT/OPT"
$no_args:
$   goto perform_link
$!  
$!    Set up a link list including all libraries
$!  
$link_all:
$!  
$   lib = ""
$   lib = "''LIB',LIBS:ROEASP/LIB"
$   lib = "''LIB',LIBS:ASPFACE/LIB"
$   lib = "''LIB',LIBDIR:FINGS/LIB"
$   lib = "''LIB',LIBS:ROEARGS/LIB"
$   lib = "''LIB',LIBS:GRASP/LIB"
$   lib = "''LIB',LIBS:RGOLIB/LIB"
$   lib = "''LIB',LIBS:ASPIC/LIB"
$   lib = "''LIB',LIBS:EDRSLIB/LIB"
$   lib = "''LIB',NAG_LIB/LIB"
$   lib = "''LIB',INTERIM/OPT"
$   lib = "''LIB',LIBDIR:TAPEIO/LIB"
$   lib = "''LIB',LIBDIR:SGS/LIB"
$   lib = "''LIB',LIBDIR:HIGR/LIB/INC=(HIGR_DRDATA,HIGR_CNDATA,HIGR_H3DATA)"
$   lib = "''LIB',GKSOPT/OPT"
$   p2length=0
$!  
$!    Link the program
$!  
$perform_link:
$   set on
$   set message/facility/severity/ident/text
$   if f$locate("FINGS",p2) .eq. p2length .and. -
       f$locate("ASPIC",p2) .eq. p2length then goto no_cargs1
$   link/nomap/notrace  'p1''lib',sys$input:/opt
    psect=cargs1,noshr
$   goto clean_up
$no_cargs1:
$   link/nomap/notrace  'p1''lib'
$clean_up:
$   deassign libs
$   set on
$   set message/facility/severity/ident/text
$   exit
