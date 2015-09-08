$   if f$logical("edrs").eqs."" then -
       assign disk$star:[aspic.edrs] edrs
$   if f$logical("per").eqs."" then -
       assign disk$star:[aspic.per] per
$   if f$logical("chart").eqs."" then -
       assign stardisk:[starlink.utility.chart] chart
