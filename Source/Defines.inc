{$IFDEF FMX}
  //FMX
  {$IF CompilerVersion >= 26} //XE5
    {$DEFINE USE_NEW_UNITS}
  {$ENDIF}
  {$IF CompilerVersion >= 27} //XE6
    {$DEFINE USE_NEW_ENUMS}
  {$ENDIF}
  {$IF CompilerVersion >= 29} //XE8
    {$DEFINE USE_IMGLST}
  {$ELSE}
    {$DEFINE USE_FMX_OLD_ENV}
  {$ENDIF}
{$ELSE}
  //VCL
  {$DEFINE VCL}
  {$DEFINE USE_IMGLST}
  {$IFDEF DCC}
    {$IF CompilerVersion >= 30} //Delphi 10 Seattle
      {$DEFINE USE_DPICHANGE}
    {$ENDIF}
  {$ENDIF}
  {$IFDEF MSWINDOWS}
    {$DEFINE VCL_WIN}
  {$ENDIF}
{$ENDIF}

