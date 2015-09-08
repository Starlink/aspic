$on warning then goto error_trap
$type sys$input

      Reads AO CRDD from an IPAC footprints tape into SDF files. Then 
      converts the data into BDF format and destripes it. Output BDF
      files are called <prefix>_BiLj.BDF (undestriped), and 
      <prefix>_BiLj_DS.BDF (destriped), where "i" is the IRAS band
      number (1-4), and "j" is the AO "leg" number.

$adamstart
$!
$! GET OBJECT NAME
$!
$inquire prefix "Give prefix for file names"
$!
$! MOUNT THE TAPE
$!
$inquire drive "Use which tape drive?"
$alloc 'drive'
$write sys$output "Please mount IPAC AO footprints tape on drive ",drive
$mount/for 'drive'
$!
$! GET A LIST OF FILES TO READ FROM THE TAPE.
$!
$inquire files "Read which files from the tape?"
$!
$! DEFINE THE FOREIGN COMMAND TO RUN THE IPAC-SPECIFIC VERSION OF KAPPA
$! FITSIN
$!
$fitsin:==$edrsx:fitsin auto=true loghdr=true nocon=true prefix='prefix' logfile=@fitsin.log mtdeck=@'drive' label=false fmtcnv=false files='files' more=false
$!
$! READ THE DATA OF THE IPAC TAPE INTO ZILLIONS OF SDF FILES
$!
$fitsin
$!
$! DISMOUNT THE TAPE
$!
$dismount 'drive'
$dealloc 'drive'
$!
$! CONVERT THE ZILLION SDF FILES TO A FEW BDF FILES
$!
$write sys$output "Converting SDF files to BDFs"
$runstar edrsx:aocrdd/prefix='prefix'
$!
$! DESTRIPE THE BDF CRDD FILES
$!
$leg=1
$LOOP1:
$band=1
$LOOP2:
$file="''prefix'_b''band'l''leg'"
$if f$search("''file'.bdf").eqs."" then goto end
$write sys$output "Destriping ",file
$runstar edrsx:aodestripe/input='file'/output='file'_DS
$band=band+1
$if band.le.4 then goto loop2
$leg=leg+1
$goto loop1
$END:
$!
$!  DELETE THE SDF FILES.
$!
$if f$search("''prefix'*.sdf").nes."" then delete/noconf 'prefix'*.sdf;*
$write sys$output ">>> Finished"
$exit
$!
$! IF AN ERROR HAPPENS, ARRIVE HERE TO TIDY UP
$!
$ERROR_TRAP:
$dismount 'drive'
$if f$search("''prefix'*.sdf").nes."" then delete/noconf 'prefix'*.sdf;*
$exit
