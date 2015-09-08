!                                                           
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!
!
!             ********************* 
!             *                   * 
!             * Procedure   DATIM * 
!             *                   * 
!             ********************* 
!
!
!
!  CALLING SEQUENCE:- 
!       DATIM [ 'Text colour' 'X-coordinate' 'Y-coordinate' ] 
!
!
!  FUNCTION:- 
!       The procedure displays the current date  and  time  on  the
!       ARGS screen. 
!
!
!  USE:- 
!       It can be used when taking photographs or video  recordings
!       of the ARGS to make a permanent record of the date and time
!       when the display was produced. The  text  string  will,  by
!       default,  be  centred  immediately  above  the  last  image
!       displayed and be white in colour.  The colour and centre of
!       the  text  string  can be specified by optional parameters.
!       Note that the centre of the text string is defined in  ARGS
!       screen  cordinates. An alternative method of specifying the
!       coordinates is to call the ASPIC routine TBXY and  position
!       the  cursor  at  the point on the screen where you want the
!       centre of the text string to be. Then call DATIM as follows
!
!                 DATIM W 'TBXY_AX' 'TBXY_AY' 
!
!
! NORMALLY DEFAULTED PARAMETERS:- 
!
! Text colour     W (white)           This is the colour  in  which
!                                     the  text  will  be  written.
!                                     (Colours available  are  
!                                     W - white  R  -  red B - blue
!                                     G - green Y - yellow
!                                     M - magenta C - cyan ) 
!
! X-coordinate    Above last image    This is the  X-coordinate  of
!                                     the   centre   of   the  text
!                                     string. 
!
! Y-coordinate    Above last image    This is the  Y-coordinate  of
!                                     the   centre   of   the  text
!                                     string. 
!
!
!
!
!
!                                                                           
!                                                                           
! D.J.King                 RGO                            27-JAN-82
!
!
!-----------------------------------------------------------------
!
$ date:=='f$time()'
$ pos='f$locate(" ",date)+1
$ time:=='f$extract(pos,5,date)
$ date:='f$extract(0,pos,date)
$ datim:="''date'   ''time'"
$ col:=='p1'
$ if col.eqs."" then col:=="w"
$ xpos:=='p2'
$ if xpos.eqs."" then xpos:==""
$ ypos:=='p3'
$ if ypos.eqs."" then ypos:==""
LET ATEXT_TEXT='DATIM'
atext 'col' SIZE= 'xpos' 'ypos' DIR=  quiet=true 
CLEAR ATEXT_TEXT
