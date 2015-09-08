*+  Details of 'extra' parameters pertaining to image scaling on display
*   and on zoom, which are held in user parameter DISPZOOM

*   no of extra parameters
      integer NXPAR
      parameter (NXPAR=8)

*   names of extra parameters
      character xpars(NXPAR)*(PARLEN)
      data xpars/'TRIM','LOG','PVLO','PVHI','ZXC','ZYC','ZXF','ZYF'/

*   length of new logical parameter type
      integer LOGLEN
      parameter (LOGLEN=1)

*   offsets within the user parameter
      integer OTRIM,OLOG,OPVLO,OPVHI,OZXC,OZYC,OZXF,OZYF
      parameter (OTRIM=1,OLOG=OTRIM+LOGLEN,OPVLO=OLOG+LOGLEN,
     :    OPVHI=OPVLO+FLTLEN,OZXC=OPVHI+FLTLEN,OZYC=OZXC+INTLEN,
     :    OZXF=OZYC+INTLEN,OZYF=OZXF+INTLEN)

*   array of offsets
      integer xoffst(NXPAR)
      data xoffst/OTRIM,OLOG,OPVLO,OPVHI,OZXC,OZYC,OZXF,OZYF/

*   array of lengths
      integer xleng(NXPAR)
      data xleng/2*LOGLEN,2*FLTLEN,4*INTLEN/

