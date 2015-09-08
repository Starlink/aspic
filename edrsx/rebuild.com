$!
$!  Re-links all executable images in EDRSX. Assumes logical name
$!  edrsx has been set up pointing to the top level EDRSX directory.
$!
$write sys$output " "
$write sys$output " THIS PROCEDURE RE-LINKS ALL .EXE FILES IN EDRSX"
$write sys$output " "
$@edrsx:devsys
$build all
