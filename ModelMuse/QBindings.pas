unit QBindings;
{ QBindings must be the very first unit in the main program source file.
  This unit load the libborqt.so / libqtintf.so from the same directory where
  the executable is. (No start script necessary.) }

interface
uses
  SysUtils;

implementation

{$IFDEF LINUX}
uses
  Libc;

procedure LoadQtLib;
const
  QtShareNames: array[Boolean] of string = (
    'libqtintf-6.9-qt2.3.so', 'libborqt-6.9-qt2.3.so');
var
  StaticallyBound: Boolean;
  UseBorQt: Boolean;
  QtLibPath: string;
begin

 // Get the path where the shared object could be found.
  QtLibPath := ExtractFileDir(ParamStr(0));


  if (QtLibPath <> '') and (QtLibPath[1] <> PathDelim) then
    QtLibPath := IncludeTrailingPathDelimiter(GetCurrentDir) + QtLibPath;
  UseBorQt := GetModuleHandle(PChar(QtShareNames[True])) <> 0;
  StaticallyBound := UseBorQt or (GetModuleHandle(PChar(QtShareNames[False])) <> 0);
  if not StaticallyBound then
  begin
    UseBorQt := GetEnvironmentVariable('CLX_USE_LIBQT') = '';
    if dlopen(PChar(QtLibPath + PathDelim + QtShareNames[UseBorQt]), RTLD_LAZY or RTLD_GLOBAL) = nil then
    begin
      // The BindHelp.pas unit will catch this error when it tries loading the
      // shared object
    end;
  end;
end;

initialization
  LoadQtLib;

{$ENDIF LINUX}

end.
