$	if p1.eqs."" then inquire p1 "Search for?"
$	if p1.eqs."" then exit
$lbl1:
$	lib=f$search("source:*.tlb")
$	if lib.eqs."" then exit
$	   write sys$output "Searching ",lib
$	   string==p1
$	   proclib 'lib' search for n y
$	   goto lbl1

