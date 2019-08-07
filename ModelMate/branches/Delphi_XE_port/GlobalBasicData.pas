unit GlobalBasicData;

interface

  uses Classes, Dialogs, SysUtils, Windows;

  const
    NumParAttributes: integer = 20;
    NumDepAttributes: integer = 16;
    NumPriAttributes: integer = 10;
    MaxLenParName: integer = 12;
    MaxLenDepName: integer = 20;
    MaxLenGpName: integer = 12;

  var

//    TGrpUse = (guParGroup, guObsGroup, guPredGroup, guPriGroup, guUnknown);


    // Intrinsic types

    { Strings }
    FileToBeOpened: string = '';
    ProjectDirectory: string;
    LocalComputerName: string;

    { Booleans }
    ObservationsChanged: boolean;
    PredictionsChanged: boolean;
    PriorChanged: boolean;
    ParameterSetupChanged: boolean;
    ProjChanged: boolean;
    StartNewProject: boolean = True;
    UCChanged: boolean;
    UcodeSetWinOpen: boolean;
    UCParEstSetWinOpen: boolean;
    OpenMainForm: boolean;

    { Integers }
    Len255: LongInt = 255;
    Len20: LongInt = 20;
    Len12: LongInt = 12;
    Idum: LongInt;

    // Objects

    { String lists }
    slYesNo: TStringList;

    { Dialogs }
    odModflowGlobal: TOpenDialog;

  procedure FreeGlobalBasicData;
  procedure InitializeGlobalBasicData;
  function ComputerName: string;

implementation

procedure FreeGlobalBasicData;
begin
  FreeAndNil(slYesNo);
  odModflowGlobal.Free;
end;

procedure InitializeGlobalBasicData;
begin
  //
  LocalComputerName := ComputerName;
  ProjectDirectory := '\';
  slYesNo := TStringList.Create;
  slYesNo.Add('Yes');
  slYesNo.Add('No');
  odModflowGlobal := TOpenDialog.Create(nil);
  odModflowGlobal.Title := 'Select MODFLOW-2005 Name File';
  odModflowGlobal.DefaultExt := '.nam';
  odModflowGlobal.Filter := 'MODFLOW Name Files (*.nam)|*.nam|All Files (*.*)|*.*';
  //
end;

function ComputerName: String;
var
  buffer: array[0..Max_ComputerName_Length-1] of char;
  size: dword;
begin
  size := Max_ComputerName_Length;
  if GetComputerName(buffer, size) then
    Result := buffer
  else
    Result := ''
end;


initialization
  InitializeGlobalBasicData;

finalization
  FreeGlobalBasicData;

end.
