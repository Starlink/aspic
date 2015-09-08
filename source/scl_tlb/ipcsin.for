!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!
!
!
!                      ********************** 
!                      *                    * 
!                      * Procedure   IPCSIN * 
!                      *                    * 
!                      ********************** 
!
!
!
!           CALLING SEQUENCE: 
!                IPCSIN 
!
!
!           FUNCTION: 
!                To read IPCS format magnetic tapes. 
!
!
!           USE: 
!                The program converts IPCS data held  on  magnetic  tape  to
!                Starlink bulk data frame files held on disk. (See SUN 3) 
!
!
!
!          USER PARAMETERS: 
!
!          INPUT                               Specifies the device on which
!                                              the  input  tape  is  loaded.
!                                              This  will  generally  be  an
!                                              explicit device name, such as
!                                              MTA1:,  although  users   can
!                                              specify  any VMS logical name
!                                              which has  been  pre-assigned
!                                              to  the device. The tape must
!                                              be mounted by the DCL command
!                                              using the /FOREIGN qualifier,
!                                              e.g. $MOUNT/FOREIGN MTA1: 
!
!          OUTPUT                              Specifies the data  files  on
!                                              the input tape that are to be
!                                              processed. Single files or  a
!                                              set  of adjacent files may be
!                                              specified.              (e.g.
!                                              FILES=3,7-10,20 will read the
!                                              files 3,7,8,9,10,20) 
!
!          FILES           YES                 Specifies  the  name  of  the
!                                              output  'bulk  data frame' on
!                                              disk. If only one file is  to
!                                              be  processed  the  parameter
!                                              can  be  specified   on   the
!                                              command  line.  However,  for
!                                              multi-file  processing,   the
!                                              user  will  be  prompted each
!                                              time a new output frame  file
!                                              specification is required. 
!
!
!          NORMALLY DEFAULTED PARAMETERS: 
!
!          COREDUMP        YES                 If     COREDUMP=YES      this
!                                              indicates that the data files
!                                              are preceded  by  a  coredump
!                                              file and a dummy run file. 
!
!
!
!          D.J.Pearce               RAL                            14-MAR-83
!
!*************************************************************************

STARDIR:IPCSIN
