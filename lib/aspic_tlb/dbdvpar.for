*+  Names of derived parameters
      character dvpars(NDVPAR)*(PARLEN)
      data dvpars/'ACENT','ASIZE','PSIZE','PTOU'/

*   array of parameter id's for first items of derived parameters
      integer dvprid(NDVPAR)
      data dvprid/2,4,6,8/

*   array of numbers of standard parameters comprising derived parameters
      integer dvnstp(NDVPAR)
      data dvnstp/2,2,2,6/
