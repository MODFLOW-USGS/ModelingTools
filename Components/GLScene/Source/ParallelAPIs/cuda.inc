{$IFDEF MSWINDOWS}
  {$DEFINE CUDA_STDCALL}
  {$UNDEF CUDA_CDECL}
{$ELSE}
  {$DEFINE CUDA_CDECL}
  {$UNDEF CUDA_STDCALL}
{$ENDIF}
{$DEFINE CUDA_NO_SM_13_DOUBLE_INTRINSICS}
{$MINENUMSIZE 4}

{$DEFINE USE_LOGGING}

{$DEFINE USE_CUDA_DEBUG_MODE}
