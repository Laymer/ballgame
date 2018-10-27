<!--
nc = 12
lmbcs =  5242880
smbcs  = 262144 + 12*(5242880-262144)/
If the value of parameter sbct is larger than the value of parameter lmbcs,
the allocator may have to create multiblock carriers that are
larger than the value of parameter lmbcs, though.
Singleblock carriers allocated through mseg_alloc are sized to whole pages.
smbcs+nc*(lmbcs-smbcs)/mbcgs
    when nc <= mbcgs
        smbcs+nc*(lmbcs-smbcs)/mbcgs
    and when nc > mbcgs
        lmbcs
{atags,false},
 {ramv,false},
 {sbct,524288},
 {asbcst,4145152},
 {rsbcst,50},
 {rsbcmt,80},
 {rmbcmt,50},
 {mmbcs,131072},
 {mmmbc,18446744073709551615},
 {mmsbc,256},
 {lmbcs,5242880},
 {smbcs,262144},
 {mbcgs,10},
 {acul,45},
 {acnl,1000},
 {acfml,0},
{as,aoffcbf}]},
https://github.com/yrashk/erlang/blob/master/erts/emulator/beam/erl_alloc_util.h -->
 <!-- #else /* if SMALL_MEMORY */ -->

 <!-- #define ERTS_DEFAULT_ALCU_INIT {                                           \ -->
     <!-- 128*1024,		/* (bytes)  ycs:    sys_alloc carrier size       */\ -->
     <!-- 1024      		/* (amount) mmc:    max mseg carriers            */\ -->
 <!-- } -->

 <!-- #define ERTS_DEFAULT_ALLCTR_INIT {                                         \ -->
     <!-- NULL,                                                                  \ -->
     <!-- ERTS_ALC_A_INVALID,	/* (number) alloc_no: allocator number           */\ -->
     <!-- 1,			/* (bool)   ts:     thread safe                  */\ -->
     <!-- 0,			/* (bool)   tspec:  thread specific              */\ -->
     <!-- 0,			/* (bool)   tpref:  thread preferred             */\ -->
     <!-- 0,			/* (bool)   ramv:   realloc always moves         */\ -->
     <!-- 64*1024,		/* (bytes)  sbct:   sbc threshold                */\ -->
     <!-- 2*1024*2024,	/* (amount) asbcst: abs sbc shrink threshold     */\ -->
     <!-- 20,			/* (%)      rsbcst: rel sbc shrink threshold     */\ -->
     <!-- 80,			/* (%)      rsbcmt: rel sbc move threshold       */\ -->
     <!-- 128*1024,		/* (bytes)  mmbcs:  main multiblock carrier size */\ -->
     <!-- 256,		/* (amount) mmsbc:  max mseg sbcs                */\ -->
     <!-- 10,			/* (amount) mmmbc:  max mseg mbcs                */\ -->
     <!-- 1024*1024,		/* (bytes)  lmbcs:  largest mbc size             */\ -->
     <!-- 128*1024,		/* (bytes)  smbcs:  smallest mbc size            */\ -->
     <!-- 10			/* (amount) mbcgs:  mbc growth stages            */\ -->
 <!-- } -->
 <!--
 nc = 12
 lmbcs =  5242880
 smbcs  = 262144 + 12*(5242880-262144)/
 If the value of parameter sbct is larger than the value of parameter lmbcs,
 the allocator may have to create multiblock carriers that are
 larger than the value of parameter lmbcs, though.
 Singleblock carriers allocated through mseg_alloc are sized to whole pages.
 smbcs+nc*(lmbcs-smbcs)/mbcgs
     when nc <= mbcgs
         smbcs+nc*(lmbcs-smbcs)/mbcgs
     and when nc > mbcgs
         lmbcs
 {atags,false},
  {ramv,false},
  {sbct,524288},
  {asbcst,4145152},
  {rsbcst,50},
  {rsbcmt,80},
  {rmbcmt,50},
  {mmbcs,131072},
  {mmmbc,18446744073709551615},
  {mmsbc,256},
  {lmbcs,5242880},
  {smbcs,262144},
  {mbcgs,10},
  {acul,45},
  {acnl,1000},
  {acfml,0},
 {as,aoffcbf}]},
 https://github.com/yrashk/erlang/blob/master/erts/emulator/beam/erl_alloc_util.h -->
  <!-- #else /* if SMALL_MEMORY */ -->

  <!-- #define ERTS_DEFAULT_ALCU_INIT {                                           \ -->
      <!-- 128*1024,		/* (bytes)  ycs:    sys_alloc carrier size       */\ -->
      <!-- 1024      		/* (amount) mmc:    max mseg carriers            */\ -->
  <!-- } -->

  <!-- #define ERTS_DEFAULT_ALLCTR_INIT {                                         \ -->
      <!-- NULL,                                                                  \ -->
      <!-- ERTS_ALC_A_INVALID,	/* (number) alloc_no: allocator number           */\ -->
      <!-- 1,			/* (bool)   ts:     thread safe                  */\ -->
      <!-- 0,			/* (bool)   tspec:  thread specific              */\ -->
      <!-- 0,			/* (bool)   tpref:  thread preferred             */\ -->
      <!-- 0,			/* (bool)   ramv:   realloc always moves         */\ -->
      <!-- 64*1024,		/* (bytes)  sbct:   sbc threshold                */\ -->
      <!-- 2*1024*2024,	/* (amount) asbcst: abs sbc shrink threshold     */\ -->
      <!-- 20,			/* (%)      rsbcst: rel sbc shrink threshold     */\ -->
      <!-- 80,			/* (%)      rsbcmt: rel sbc move threshold       */\ -->
      <!-- 128*1024,		/* (bytes)  mmbcs:  main multiblock carrier size */\ -->
      <!-- 256,		/* (amount) mmsbc:  max mseg sbcs                */\ -->
      <!-- 10,			/* (amount) mmmbc:  max mseg mbcs                */\ -->
      <!-- 1024*1024,		/* (bytes)  lmbcs:  largest mbc size             */\ -->
      <!-- 128*1024,		/* (bytes)  smbcs:  smallest mbc size            */\ -->
      <!-- 10			/* (amount) mbcgs:  mbc growth stages            */\ -->
  <!-- } -->
