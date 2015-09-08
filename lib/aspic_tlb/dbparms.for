*+  Parameter definitions for args database retrieval suite

*   nos of standard, user and derived parameters and offsets to
*   starts of user and derived ones (also max no of values in a
*   derived parameter)
      integer NSTPAR,NUSPAR,NDVPAR,STPARB,USPARB,DVPARB,MAXPAR
      parameter (NSTPAR=18,NUSPAR=5,NDVPAR=4)
      parameter (STPARB=0,USPARB=NSTPAR,DVPARB=NSTPAR+NUSPAR)
      parameter (MAXPAR=6)

*   no of image types
      integer NTYPES
      parameter (NTYPES=3)

*   lengths of items in data record (and maximum length)
      integer TYPLEN,INTLEN,FLTLEN,LABLEN,UNILEN,PARLEN,USVLEN,MAXLEN
      parameter (TYPLEN=4,INTLEN=11,FLTLEN=15,LABLEN=8,UNILEN=8,
     :    PARLEN=8,USVLEN=80)
      parameter (MAXLEN=MAX(TYPLEN,INTLEN,FLTLEN,LABLEN,PARLEN,USVLEN))

*   file unit no, record length, max no of records and undefined integer
      integer DBCHAN,RECLEN,MAXREC,IUNDEF
      parameter (DBCHAN=42,IUNDEF=-1)
      parameter (RECLEN=TYPLEN+7*INTLEN+6*FLTLEN+2*LABLEN+2*UNILEN+
     :    NUSPAR*(PARLEN+USVLEN),MAXREC=RECLEN/INTLEN-1)

*   offsets to fields in data record
      integer OTYP,OACENX,OACENY,OASIZX,OASIZY,OPSIZX,OPSIZY,
     :    OPTOU1,OPTOU2,OPTOU3,OPTOU4,OPTOU5,OPTOU6,OLABX,OLABY,
     :    OUNITX,OUNITY,ONUSP,OUSP1,OUSV1,OUSV2,OUSV3,OUSV4,OUSV5
      parameter (OTYP=1,OACENX=OTYP+TYPLEN,OACENY=OACENX+INTLEN,
     :    OASIZX=OACENY+INTLEN,OASIZY=OASIZX+INTLEN,OPSIZX=OASIZY+
     :    INTLEN,OPSIZY=OPSIZX+INTLEN,OPTOU1=OPSIZY+INTLEN,OPTOU2=
     :    OPTOU1+FLTLEN,OPTOU3=OPTOU2+FLTLEN,OPTOU4=OPTOU3+FLTLEN,
     :    OPTOU5=OPTOU4+FLTLEN,OPTOU6=OPTOU5+FLTLEN,OLABX=OPTOU6+FLTLEN,
     :    OLABY=OLABX+LABLEN,OUNITX=OLABY+LABLEN,OUNITY=OUNITX+UNILEN,
     :    ONUSP=OUNITY+LABLEN,OUSP1=ONUSP+INTLEN,OUSV1=OUSP1+PARLEN,
     :    OUSV2=OUSV1+USVLEN+PARLEN,OUSV3=OUSV2+USVLEN+PARLEN,
     :    OUSV4=OUSV3+USVLEN+PARLEN,OUSV5=OUSV4+USVLEN+PARLEN)

*   array of offsets
      integer offset(DVPARB)
      data offset/OTYP,OACENX,OACENY,OASIZX,OASIZY,OPSIZX,OPSIZY,
     :    OPTOU1,OPTOU2,OPTOU3,OPTOU4,OPTOU5,OPTOU6,OLABX,OLABY,
     :    OUNITX,OUNITY,ONUSP,OUSV1,OUSV2,OUSV3,OUSV4,OUSV5/

*   array of lengths
      integer length(DVPARB)
      data length/TYPLEN,6*INTLEN,6*FLTLEN,2*LABLEN,2*UNILEN,INTLEN,
     :    5*USVLEN/
