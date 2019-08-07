unit frmModflowNameFileUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, Buttons, UndoItems, ExtCtrls,
  PhastModelUnit;

type
  TNameFileSettings = class(TCollectionItem)
  private
    FLines: TStrings;
    FModel: TCustomModel;
    procedure SetLines(const Value: TStrings);
  public
    AlternateSolver: boolean;
    AlternateFlowPackage: boolean;
    property Lines: TStrings read FLines write SetLines;
    Constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;
    procedure AssignPropertiesToModel;
  end;

  TNameFileSettingsCollection = class(TCollection)
  private
    function GetItems(Index: integer): TNameFileSettings;
    procedure SetItems(Index: integer; const Value: TNameFileSettings);
  public
    Constructor Create(Model: TPhastModel);
    property Items[Index: integer]: TNameFileSettings read GetItems
      write SetItems; default;
    function Add: TNameFileSettings;
    procedure AssignPropertiesToModel;
  end;

  TfrmModflowNameFile = class(TfrmCustomGoPhast)
    pnlMain: TPanel;
    lblLines: TLabel;
    memoLines: TMemo;
    cbFlowPackage: TCheckBox;
    cbSolvers: TCheckBox;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    pnlModel: TPanel;
    comboModel: TComboBox;
    lblModel: TLabel;
    btnFileTypes: TButton;
    procedure FormCreate(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
    procedure comboModelChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject); override;
    procedure btnFileTypesClick(Sender: TObject);
  private
    FSettings: TNameFileSettingsCollection;
    FCurrentSettings: TNameFileSettings;
    FCreating: Boolean;
    procedure SetCurrentSettings(const Value: TNameFileSettings);
    property CurrentSettings: TNameFileSettings read FCurrentSettings
      write SetCurrentSettings;
    procedure GetData;
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

  TUndoLgrNameFileSettings = class(TCustomUndo)
  private
    FNewSettings: TNameFileSettingsCollection;
    FOldSettings: TNameFileSettingsCollection;
  protected
    function Description: string; override;
  public
    procedure DoCommand; override;
    // @name undoes the command.
    procedure Undo; override;
    Constructor Create(var Settings: TNameFileSettingsCollection);
    Destructor Destroy; override;
  end;

implementation

uses
  frmGoPhastUnit, frmFileTypesUnit;

resourcestring
  StrChangeMODFLOWName = 'change MODFLOW name file';

{$R *.dfm}

{ TfrmModflowNameFile }

procedure TfrmModflowNameFile.btnFileTypesClick(Sender: TObject);
begin
  inherited;
  DisplayAForm(TfrmFileTypes);
end;

procedure TfrmModflowNameFile.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmModflowNameFile.comboModelChange(Sender: TObject);
begin
  inherited;
  if FCreating then
  begin
    Exit;
  end;
  CurrentSettings := comboModel.Items.Objects[comboModel.ItemIndex]
    as TNameFileSettings;
end;

procedure TfrmModflowNameFile.FormCreate(Sender: TObject);
begin
  inherited;
  FCreating := True;
  try
    FillComboWithModelNames(comboModel);
    pnlModel.Visible := frmGoPhast.PhastModel.LgrUsed;
  finally
    FCreating := False;
  end;
  GetData;
end;

procedure TfrmModflowNameFile.FormDestroy(Sender: TObject);
begin
  inherited;
  FSettings.Free;
end;

procedure TfrmModflowNameFile.GetData;
var
  Index: Integer;
begin
  FSettings:= TNameFileSettingsCollection.Create(frmGoPhast.PhastModel);
  Assert(FSettings.Count = comboModel.Items.Count);
  for Index := 0 to comboModel.Items.Count - 1 do
  begin
    comboModel.Items.Objects[Index] := FSettings[Index];
  end;
  comboModelChange(nil);
end;

procedure TfrmModflowNameFile.SetCurrentSettings(
  const Value: TNameFileSettings);
begin
  if FCurrentSettings <> Value then
  begin
    if FCurrentSettings <> nil then
    begin
      FCurrentSettings.Lines := memoLines.Lines;
      FCurrentSettings.AlternateFlowPackage := cbFlowPackage.Checked;
      FCurrentSettings.AlternateSolver := cbSolvers.Checked;
    end;
    FCurrentSettings := Value;
    if FCurrentSettings <> nil then
    begin
      memoLines.Lines := FCurrentSettings.Lines;
      cbFlowPackage.Checked := FCurrentSettings.AlternateFlowPackage;
      cbSolvers.Checked := FCurrentSettings.AlternateSolver;
    end;
  end;
end;

procedure TfrmModflowNameFile.SetData;
begin
  CurrentSettings:= nil;
  frmGoPhast.UndoStack.Submit(TUndoLgrNameFileSettings.Create(FSettings));
end;

{ TNameFileSettings }

procedure TNameFileSettings.AssignPropertiesToModel;
begin
  FModel.ModflowNameFileLines := Lines;
  FModel.AlternateSolver := AlternateSolver;
  FModel.AlternateFlowPackage := AlternateFlowPackage;
end;

constructor TNameFileSettings.Create(Collection: TCollection);
begin
  inherited;
  FLines := TStringList.Create;
end;

destructor TNameFileSettings.Destroy;
begin
  FLines.Free;
  inherited;
end;

procedure TNameFileSettings.SetLines(const Value: TStrings);
begin
  FLines.Assign(Value);
end;

{ TNameFileSettingsCollection }

function TNameFileSettingsCollection.Add: TNameFileSettings;
begin
  result := inherited Add as TNameFileSettings;
end;

procedure TNameFileSettingsCollection.AssignPropertiesToModel;
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Items[Index].AssignPropertiesToModel;
  end;
end;

constructor TNameFileSettingsCollection.Create(Model: TPhastModel);
var
  Item: TNameFileSettings;
  Index: Integer;
  ChildModel: TChildModel;
begin
  inherited Create(TNameFileSettings);
  Item := Add;
  Item.FModel := Model;
  Item.Lines := Model.ModflowNameFileLines;
  Item.AlternateSolver := Model.AlternateSolver;
  Item.AlternateFlowPackage := Model.AlternateFlowPackage;
  if Model.LgrUsed then
  begin
    for Index := 0 to Model.ChildModels.Count - 1 do
    begin
      ChildModel := Model.ChildModels[Index].ChildModel;
      Item := Add;
      Item.FModel := ChildModel;
      Item.Lines := ChildModel.ModflowNameFileLines;
      Item.AlternateSolver := ChildModel.AlternateSolver;
      Item.AlternateFlowPackage := ChildModel.AlternateFlowPackage;
    end;
  end;
end;

function TNameFileSettingsCollection.GetItems(
  Index: integer): TNameFileSettings;
begin
  result := Inherited Items[Index] as TNameFileSettings
end;

procedure TNameFileSettingsCollection.SetItems(Index: integer;
  const Value: TNameFileSettings);
begin
  Inherited Items[Index] := Value;
end;

{ TUndoLgrNameFileSettings }

constructor TUndoLgrNameFileSettings.Create(
  var Settings: TNameFileSettingsCollection);
begin
  FNewSettings := Settings;
  Settings := nil;
  FOldSettings := TNameFileSettingsCollection.Create(frmGoPhast.PhastModel);
end;

function TUndoLgrNameFileSettings.Description: string;
begin
  result := StrChangeMODFLOWName;
end;

destructor TUndoLgrNameFileSettings.Destroy;
begin
  FNewSettings.Free;
  FOldSettings.Free;
  inherited;
end;

procedure TUndoLgrNameFileSettings.DoCommand;
begin
  inherited;
  FNewSettings.AssignPropertiesToModel;
end;

procedure TUndoLgrNameFileSettings.Undo;
begin
  inherited;
  FOldSettings.AssignPropertiesToModel;
end;

end.
