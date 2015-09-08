*       module  DT_DAT
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Declares variables to hold data about the detectors,
*       and calibration, and sets up their correct values.
*
*SOURCE
*       DT_DAT.INC in UTILITIES.TLB
*
*METHOD
*       The data in this module is taken from the IRAS data sets
*       explanatory supplement, and IPMAF.
*       1) Module 'IR_PAR' must be included before this module
*       to define the required IRAS mission parameters.
*       2) Null values are filled with DT_ign
*       3) Values within each band are held in the order in which
*       the detectors are stored in a CRDD file
*
*VAX SPECIFICS
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 28/8/87
*-------------------------------------------------------------------
*
* DECLARE VARIABLES
*
      integer   DT_ign
      parameter (DT_ign=-9999)

      integer   DT_bns(IR_bns)          ! No. of detectors in each band.
                                        ! 1st index: band no.
      integer   DT_bal(IR_dts,IR_bns)   ! Ball no. for each detector.
                                        ! 1st index: detector position
                                        ! in order of hi z to lo z
                                        ! 2nd index: band no.
      real      DT_ypo(IR_dts,IR_bns)   ! Y position of centre of
                                        ! detector in arcmins.
                                        ! Indecices as for bal
      real      DT_zpo(IR_dts,IR_bns)   ! Z position of centre of
                                        ! detector in arcmins.
                                        ! Indecices as for bal
      integer   DT_srt(IR_bns)          ! The sampling frequency
                                        ! for each detector.
                                        ! 1st index: band no.
      real      DT_cvf(IR_bns)          ! A factor for converting data
                                        ! from W/M**2 to Janskys
                                        ! 1st index: band no.
                                        ! These values are 10**26/bwidth
                                        ! (bwidth is effective bandwidth
                                        !  from last para. of Exp. Supp.
                                        !  page X-13)
      real      DT_o84(IR_BNS)          ! Correction factors to apply to
                                        ! CRDD to get oct 1984 calibration
                                        ! (Obtained from module IRASDATA
                                        !  from IPMAF program CRDIMAGE)
*
* ASSIGN VALUES TO VARIABLES
*
      data DT_bns/16,15,16,15/

      data DT_bal/47,27,51,23,48,28,52,24,49,29,53,25,50,30,54,26,
     :            39,19,43,16,40,20,44,17,41,21,45,18,42,22,46,-9999,
     :            31,12,35, 8,32,13,36, 9,33,14,37,10,34,15,38,11,
     :            55, 4,59, 1,56, 5,60, 2,57, 6,61, 3,58, 7,62,-9999/

      data DT_ypo/-5.67, 7.71,-7.42,  9.47,-5.67,  7.71,-7.43,  9.46,
     :            -5.67, 7.70,-7.43,  9.47,-5.66,  7.71,-7.42,  9.48,
     :            -1.16,12.24, -2.92,14.01, -1.16,12.27, -2.92,14.04,
     :            -1.16,12.26, -2.93,14.04, -1.14,12.27, -2.92,-9999,
     :             4.56,17.20, 2.06, 19.64, 4.59, 17.19, 2.06, 19.72,
     :             4.58,17.20, 2.11, 19.74, 4.59, 17.20, 2.10, 19.70,
     :           -11.33,23.83,-15.34,27.87,-11.42,24.04,-15.49,27.80,
     :           -11.51,23.65,-15.40,27.86,-11.41,23.78,-15.38,-9999/

      data DT_zpo/14.64,13.55,11.98, 9.81, 7.65,  5.47,  3.32,  1.14,
     :            -1.02,-3.19,-5.35,-7.52,-9.68,-11.86,-13.41,-14.50,
     :            14.05,12.96,10.88, 8.71,  6.55,  4.37,  2.22, 0.04,
     :            -2.12,-4.29,-6.45,-8.62,-10.78,-12.88,-13.95,-9999,
     :            14.55,13.49,11.94, 9.80, 7.61,  5.47,  3.27,  1.14,
     :            -1.06,-3.20,-5.40,-7.53,-9.73,-11.86,-13.41,-14.46,
     :            13.95,12.86,10.88, 8.71,  6.55,  4.37,  2.21, 0.04,
     :            -2.12,-4.29,-6.46,-8.62,-10.79,-12.77,-13.84,-9999/

      data DT_srt/16,16,8,4/

      data DT_cvf/7.42e12,19.38e12,38.76e12,100.0e12/

      data DT_o84/0.97,0.98,0.93,0.74/
