unit frmProgramLocationsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, Buttons, ExtCtrls, Mask, JvExMask,
  JvToolEdit, JvExControls, JvExStdCtrls, JvHtControls, UndoItems,
  PhastModelUnit, JvExExtCtrls, JvExtComponent, JvRollOut, ImgList,
  System.ImageList;

type
  TUndoChangeProgramLocations = class(TCustomUndo)
  private
    FOldLocations: TProgramLocations;
    FNewLocations: TProgramLocations;
  protected
    function Description: string; override;
  public
    Constructor Create(var NewLocations: TProgramLocations);
    Destructor Destroy; override;
    procedure DoCommand;  override;
    procedure Undo; override;
  end;

  TfrmProgramLocations = class(TfrmCustomGoPhast)
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    Panel1: TPanel;
    ilShowHide: TImageList;
    jvrltModflow2005: TJvRollOut;
    htlblModflow: TJvHTLabel;
    fedModflow: TJvFilenameEdit;
    jvrltModflowLGR: TJvRollOut;
    JvHTLabel1: TJvHTLabel;
    fedModflowLgr: TJvFilenameEdit;
    jvrltModflowLgr2: TJvRollOut;
    jvhtlblMfLgr2: TJvHTLabel;
    fedModflowLgr2: TJvFilenameEdit;
    jvrltModflowNWT: TJvRollOut;
    htlblModflowNWT: TJvHTLabel;
    fedModflowNWT: TJvFilenameEdit;
    jvrltModpath: TJvRollOut;
    htlblModPath: TJvHTLabel;
    fedModpath: TJvFilenameEdit;
    jvrltZoneBudget: TJvRollOut;
    htlblZoneBudget: TJvHTLabel;
    fedZonebudget: TJvFilenameEdit;
    jvrltMt3dms: TJvRollOut;
    htlblMt3dms: TJvHTLabel;
    fedMt3dms: TJvFilenameEdit;
    jvrltModelMate: TJvRollOut;
    htlblModelMate: TJvHTLabel;
    fedModelMate: TJvFilenameEdit;
    jvrltTextEditor: TJvRollOut;
    fedTextEditor: TJvFilenameEdit;
    jvrltModelMonitor: TJvRollOut;
    fedModelMonitor: TJvFilenameEdit;
    jvrltModflowFmp: TJvRollOut;
    fedModflowFmp: TJvFilenameEdit;
    htlblModflowFmp: TJvHTLabel;
    jvrltModflowCFP: TJvRollOut;
    htlblModflowCFP: TJvHTLabel;
    fedModflowCFP: TJvFilenameEdit;
    jvrltModflow6: TJvRollOut;
    htlblModflow6: TJvHTLabel;
    fedModflow6: TJvFilenameEdit;
    htlbl1: TJvHTLabel;
    jvrltMt3dUsgs: TJvRollOut;
    htlblMt3dUSGS: TJvHTLabel;
    fedMt3dUsgs: TJvFilenameEdit;
    jvrltZonebudget6: TJvRollOut;
    htlblZoneBudget6: TJvHTLabel;
    fedZonebudget6: TJvFilenameEdit;
    jvrltModflowOwhmV2: TJvRollOut;
    htlblModflowOwhmV2: TJvHTLabel;
    fedModflowOwhm2: TJvFilenameEdit;
    procedure fedModflowChange(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure jvrltExpand(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FModel: TCustomModel;
    procedure SetData;
    procedure HighlightControls;
    procedure AdjustHeight;
    { Private declarations }
  public
    Procedure GetData(Model: TCustomModel);
    { Public declarations }
  end;

implementation

uses frmGoPhastUnit, GoPhastTypes, ModflowPackageSelectionUnit;

resourcestring
  StrThisFileDoesNotE = 'This file does not exist';
  StrTheNameOfThisPro = 'The name of this program should not be "ModelMuse."';
  StrChangeProgramLocat = 'change program locations';

{$R *.dfm}

{ TfrmProgramLocations }

procedure TfrmProgramLocations.AdjustHeight;
begin
  ClientHeight := jvrltModelMonitor.Top + jvrltModelMonitor.Height
    + pnlBottom.Height;
end;

procedure TfrmProgramLocations.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmProgramLocations.fedModflowChange(Sender: TObject);
begin
  inherited;
  HighlightControls;
end;

procedure TfrmProgramLocations.FormShow(Sender: TObject);
begin
  inherited;
  AdjustHeight;
end;

procedure TfrmProgramLocations.GetData(Model: TCustomModel);
var
  Locations: TProgramLocations;
  function LinkString(const Url: string): string;
  begin
    result := Format('<a href="%0:s">%0:s</a>', [Url]);
  end;
begin
  FModel := Model;
  frmGoPhast.ReadIniFile;
  Locations := frmGoPhast.PhastModel.ProgramLocations;

  try
    fedModflow.FileName := Locations.ModflowLocation;
  except on EComboEditError do
    fedModflow.FileName := '';
  end;

  try
    fedTextEditor.FileName := Locations.TextEditorLocation;
  except on EComboEditError do
    fedTextEditor.FileName := '';
  end;

  if Model.ModflowPackages.ModPath.MpathVersion = mp5 then
  begin
    try
      fedModpath.FileName := Locations.ModPathLocation;
    except on EComboEditError do
      fedModpath.FileName := '';
    end;
    jvrltModpath.Caption := 'MODPATH v5';
    htlblModPath.Caption := LinkString(
      'https://water.usgs.gov/nrp/gwsoftware/modpath5/modpath5.html');
  end
  else if Model.ModflowPackages.ModPath.MpathVersion = mp6 then
  begin
    try
      fedModpath.FileName := Locations.ModPathLocationVersion6;
    except on EComboEditError do
      fedModpath.FileName := '';
    end;
    jvrltModpath.Caption := 'MODPATH v6';
    htlblModPath.Caption := LinkString('https://www.usgs.gov/software/modpath-particle-tracking-model-modflow');
  end
  else
  begin
    try
      fedModpath.FileName := Locations.ModPathLocationVersion7;
    except on EComboEditError do
      fedModpath.FileName := '';
    end;
    jvrltModpath.Caption := 'MODPATH v7';
    htlblModPath.Caption := LinkString('https://www.usgs.gov/software/modpath-particle-tracking-model-modflow');
  end;

  try
    fedModelMonitor.FileName := Locations.ModelMonitorLocation;
  except on EComboEditError do
    fedModelMonitor.FileName := ''
  end;

  try
    fedZonebudget.FileName := Locations.ZoneBudgetLocation;
  except on EComboEditError do
    fedZonebudget.FileName := '';
  end;

  try
    fedZonebudget6.FileName := Locations.ZoneBudgetLocationMf6;
  except on EComboEditError do
    fedZonebudget6.FileName := '';
  end;

  try
    fedModelMate.FileName := Locations.ModelMateLocation;
  except on EComboEditError do
    fedModelMate.FileName := '';
  end;

  try
    fedModflowLgr.FileName := Locations.ModflowLgrLocation;
  except on EComboEditError do
    fedModflowLgr.FileName := '';
  end;

  try
    fedModflowLgr2.FileName := Locations.ModflowLgr2Location;
  except on EComboEditError do
    fedModflowLgr2.FileName := '';
  end;

  try
    fedModflowNwt.FileName := Locations.ModflowNwtLocation;
  except on EComboEditError do
    fedModflowNwt.FileName := '';
  end;

  try
    fedMt3dms.FileName := Locations.Mt3dmsLocation;
  except on EComboEditError do
    fedMt3dms.FileName := '';
  end;

  try
    fedMt3dUsgs.FileName := Locations.Mt3dUsgsLocation;
  except on EComboEditError do
    fedMt3dUsgs.FileName := '';
  end;

  try
    fedModflowFmp.FileName := Locations.ModflowOwhmLocation;
  except on EComboEditError do
    fedModflowFmp.FileName := '';
  end;

  try
    fedModflowCfp.FileName := Locations.ModflowCfpLocation;
  except on EComboEditError do
    fedModflowCfp.FileName := '';
  end;

  try
    fedModflow6.FileName := Locations.Modflow6Location;
  except on EComboEditError do
    fedModflow6.FileName := '';
  end;

  try
    fedModflowOwhm2.FileName := Locations.ModflowOwhmV2Location;
  except on EComboEditError do
    fedModflowOwhm2.FileName := '';
  end;

  HighlightControls;
end;

procedure TfrmProgramLocations.SetData;
var
  Locations: TProgramLocations;
  Undo: TUndoChangeProgramLocations;
begin
  Locations := TProgramLocations.Create;
  try
    Locations.Assign(frmGoPhast.PhastModel.ProgramLocations);
    Locations.ModflowLocation := fedModflow.FileName;
    Locations.TextEditorLocation := fedTextEditor.FileName;
    if FModel.ModflowPackages.ModPath.MpathVersion = mp5 then
    begin
      Locations.ModPathLocation := fedModpath.FileName;
    end
    else if FModel.ModflowPackages.ModPath.MpathVersion = mp6 then
    begin
      Locations.ModPathLocationVersion6 := fedModpath.FileName;
    end
    else
    begin
      Locations.ModPathLocationVersion7 := fedModpath.FileName;
    end;
    Locations.ModelMonitorLocation := fedModelMonitor.FileName;
    Locations.ZoneBudgetLocation := fedZonebudget.FileName;
    Locations.ZoneBudgetLocationMf6 := fedZonebudget6.FileName;
    Locations.ModelMateLocation := fedModelMate.FileName;
    Locations.ModflowLgrLocation := fedModflowLgr.FileName;
    Locations.ModflowLgr2Location := fedModflowLgr2.FileName;
    Locations.ModflowNwtLocation := fedModflowNwt.FileName;
    Locations.ModflowOwhmLocation := fedModflowFmp.FileName;
    Locations.ModflowCfpLocation := fedModflowCfp.FileName;
    Locations.Mt3dmsLocation := fedMt3dms.FileName;
    Locations.Mt3dUsgsLocation := fedMt3dUsgs.FileName;
    Locations.Modflow6Location := fedModflow6.FileName;
    Locations.ModflowOwhmV2Location := fedModflowOwhm2.FileName;
    Undo := TUndoChangeProgramLocations.Create(Locations);
    frmGoPhast.UndoStack.Submit(Undo);
  finally
    Locations.Free
  end;
end;

procedure TfrmProgramLocations.HighlightControls;
var
  ModflowOK: Boolean;
  ModflowLgrOK: Boolean;
  ModflowLgr2OK: Boolean;
  ModpathOK: Boolean;
  ZoneBudgetOK: Boolean;
  FileEditorOK: Boolean;
  ModflowNwtOK: Boolean;
  ModflowFmpOK: Boolean;
  ModflowCfpOK: Boolean;
  Modflow6OK: Boolean;
  ZoneBudget6OK: Boolean;
  ModflowOwhmV2OK: Boolean;
  function CheckControl(Edit: TJvFilenameEdit): boolean;
  var
    LocalFileName: string;
  begin
    LocalFileName := ExtractFileName(Edit.FileName);
    result := LocalFileName <> 'ModelMuse.exe';
    if result then
    begin
      if Edit = fedTextEditor then
      begin
        result := FileExists(Edit.FileName) or (Edit.FileName = '')
          or (LowerCase(Edit.FileName) = 'notepad.exe');
      end
      else if Edit = fedModelMonitor then
      begin
        if Edit.FileName = '' then
        begin
          result := True;
        end
        else
        begin
          result := FileExists(Edit.FileName)
            and (LocalFileName = 'ModelMonitor.exe');
        end;
      end
      else
      begin
        result := FileExists(Edit.FileName);
      end;
      if not result then
      begin
        Edit.Hint := StrThisFileDoesNotE;
        Edit.ShowHint := True;
      end;
    end
    else
    begin
      Edit.Hint := StrTheNameOfThisPro;
      Edit.ShowHint := True;
    end;
    if result then
    begin
      Edit.Color := clWindow;
      Edit.ShowHint := False;
    end
    else
    begin
      Edit.Color := clRed;
    end;
  end;
begin
  CheckControl(fedModelMonitor);
  CheckControl(fedModelMate);

  jvrltModflow2005.Collapsed :=
    (frmGoPhast.PhastModel.ModelSelection  <> msModflow);
  ModflowOK := CheckControl(fedModflow)
    or (frmGoPhast.PhastModel.ModelSelection  <> msModflow);

  jvrltModflowLGR.Collapsed :=
    (frmGoPhast.PhastModel.ModelSelection  <> msModflowLGR);
  ModflowLgrOK := CheckControl(fedModflowLgr)
    or (frmGoPhast.PhastModel.ModelSelection  <> msModflowLGR);

  jvrltModflowLgr2.Collapsed :=
    (frmGoPhast.PhastModel.ModelSelection  <> msModflowLGR2);
  ModflowLgr2OK := CheckControl(fedModflowLgr2)
    or (frmGoPhast.PhastModel.ModelSelection  <> msModflowLGR2);

  jvrltModflowNWT.Collapsed :=
    (frmGoPhast.PhastModel.ModelSelection  <> msModflowNWT);
  ModflowNwtOK := CheckControl(fedModflowNWT)
    or (frmGoPhast.PhastModel.ModelSelection  <> msModflowNWT);

  jvrltModflowOwhmV2.Collapsed :=
    (frmGoPhast.PhastModel.ModelSelection  <> msModflowOwhm2);
  ModflowOwhmV2OK := CheckControl(fedModflowOwhm2)
    or (frmGoPhast.PhastModel.ModelSelection  <> msModflowOwhm2);

  jvrltModflowFmp.Collapsed :=
    (frmGoPhast.PhastModel.ModelSelection  <> msModflowFMP);
  ModflowFmpOK := CheckControl(fedModflowFmp)
    or (frmGoPhast.PhastModel.ModelSelection  <> msModflowFmp);

  jvrltModflowCFP.Collapsed :=
    (frmGoPhast.PhastModel.ModelSelection  <> msModflowCFP);
  ModflowCfpOK := CheckControl(fedModflowCfp)
    or (frmGoPhast.PhastModel.ModelSelection  <> msModflowCFP);

  jvrltModflow6.Collapsed :=
    (frmGoPhast.PhastModel.ModelSelection  <> msModflow2015);
  Modflow6OK := CheckControl(fedModflow6)
    or (frmGoPhast.PhastModel.ModelSelection  <> msModflow2015);

  jvrltModpath.Collapsed := not frmGoPhast.PhastModel.ModPathIsSelected;
  ModpathOK := CheckControl(fedModpath)
    or not frmGoPhast.PhastModel.ModPathIsSelected;

  jvrltZoneBudget.Collapsed :=
    (not frmGoPhast.PhastModel.ZoneBudgetIsSelected)
    or (frmGoPhast.PhastModel.ModelSelection = msModflow2015);
  ZoneBudgetOK := CheckControl(fedZonebudget)
    or not frmGoPhast.PhastModel.ZoneBudgetIsSelected
    or (frmGoPhast.PhastModel.ModelSelection = msModflow2015);

  jvrltZoneBudget6.Collapsed :=
    (not frmGoPhast.PhastModel.ZoneBudgetIsSelected)
    or (frmGoPhast.PhastModel.ModelSelection <> msModflow2015);
  ZoneBudget6OK := CheckControl(fedZonebudget6)
    or (not frmGoPhast.PhastModel.ZoneBudgetIsSelected)
    or (frmGoPhast.PhastModel.ModelSelection <> msModflow2015);

  jvrltMt3dms.Collapsed :=
    not frmGoPhast.PhastModel.Mt3dIsSelected
    or (frmGoPhast.PhastModel.ModflowPackages.Mt3dBasic.Mt3dVersion <> mvMS);
  CheckControl(fedMt3dms);

  jvrltMt3dUsgs.Collapsed :=
    not frmGoPhast.PhastModel.Mt3dIsSelected
    or (frmGoPhast.PhastModel.ModflowPackages.Mt3dBasic.Mt3dVersion <> mvUSGS);
  CheckControl(fedMt3dms);

  jvrltModelMate.Collapsed :=
    frmGoPhast.PhastModel.ModelMateProjectFileName  = '';

  FileEditorOK := CheckControl(fedTextEditor);

  btnOK.Enabled := ModflowOK and ModflowLgrOK and ModflowLgr2OK
    and ModflowNwtOK and ModflowFmpOK and ModflowOwhmV2OK
    and ModflowCfpOK and ModpathOK and ZoneBudgetOK and ZoneBudget6OK
    and FileEditorOK and Modflow6OK;
end;

procedure TfrmProgramLocations.jvrltExpand(Sender: TObject);
begin
  inherited;
  AdjustHeight;
end;

{ TUndoChangeProgramLocations }

constructor TUndoChangeProgramLocations.Create(
  var NewLocations: TProgramLocations);
begin
  Assert(NewLocations <> nil);
  FOldLocations := TProgramLocations.Create;
  FOldLocations.Assign(frmGoPhast.PhastModel.ProgramLocations);
  FNewLocations := NewLocations;
  NewLocations := nil;
end;

function TUndoChangeProgramLocations.Description: string;
begin
  result := StrChangeProgramLocat;
end;

destructor TUndoChangeProgramLocations.Destroy;
begin
  FNewLocations.Free;
  FOldLocations.Free;
  inherited;
end;

procedure TUndoChangeProgramLocations.DoCommand;
begin
  inherited;
  frmGoPhast.PhastModel.ProgramLocations.Assign(FNewLocations);
  frmGoPhast.WriteIniFile;
end;

procedure TUndoChangeProgramLocations.Undo;
begin
  inherited;
  frmGoPhast.PhastModel.ProgramLocations.Assign(FOldLocations);
  frmGoPhast.WriteIniFile;
end;

end.
