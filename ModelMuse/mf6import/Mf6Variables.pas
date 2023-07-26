unit Mf6Variables;

interface

uses
  Winapi.Windows, Vcl.Dialogs, System.SysUtils;

type
  cint32                 = LongInt;
  cint                   = cint32;
  TMf6Constants = record
    BMI_LENCOMPONENTNAME: cint;
    BMI_LENERRMESSAGE: cint;
    BMI_LENGRIDTYPE: cint;
    BMI_LENVARADDRESS: cint;
    BMI_LENVARTYPE: cint;
    BMI_LENVERSION: cint;
  end;
  PMf6Constants = ^TMf6Constants;

implementation

var
  FMappingHandle : THandle = 0;
  FRecordInfo : PMf6Constants= nil;
  FMappingName : String = '';

  procedure GetConstants;
begin
{ To Open the Memory Map File }
  FMappingName := 'C:\ModelingTools\ModelMuse\mf6import\libmf6.dll';
  FMappingHandle := OpenFileMapping(FILE_MAP_ALL_ACCESS, false, PChar(FMappingName));

  { To Read the File Contents}
  FRecordInfo := MapViewOfFile(FMappingHandle, FILE_MAP_ALL_ACCESS, 0, 0, SizeOf(TMf6Constants));

//  ShowMessage('BMI_LENCOMPONENTNAME: '  + IntToStr(FRecordInfo^.BMI_LENCOMPONENTNAME));
end;

initialization
  GetConstants;

end.
