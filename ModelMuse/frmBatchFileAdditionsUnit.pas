unit frmBatchFileAdditionsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, ExtCtrls, frameBatchFileLinesUnit, StdCtrls,
  Buttons, UndoItems, PhastModelUnit;

type
  TModelLinesAssociation = class(TObject)
    public
    FModel: TCustomModel;
    BeforeLines: TStringList;
    AfterLines: TStringList;
    Constructor Create(Model: TCustomModel);
    Destructor Destroy; override;
  end;

  TModelAssociations = class(TObject)
  private
    FAssociations: TList;
    FCached: TModelLinesAssociation;
    function GetAssoc(Model: TCustomModel): TModelLinesAssociation;
  public
    Constructor Create;
    Destructor Destroy; override;
    property Assoc[Model: TCustomModel]: TModelLinesAssociation
      read GetAssoc; default;
  end;

  TfrmBatchFileAdditions = class(TfrmCustomGoPhast)
    frameBatchFileBefore: TframeBatchFileLines;
    Splitter1: TSplitter;
    frameBatchFileAfter: TframeBatchFileLines;
    Panel1: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    Panel2: TPanel;
    pnlModel: TPanel;
    lblModel: TLabel;
    comboModel: TComboBox;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure comboModelChange(Sender: TObject);
  private
    FAssociations: TModelAssociations;
    FCurrentModelLines: TModelLinesAssociation;
    procedure SetCurrentModelLines(const Value: TModelLinesAssociation);
    property CurrentModelLines: TModelLinesAssociation read FCurrentModelLines
      write SetCurrentModelLines;
    procedure GetData;
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

  TBatchFileAdditionsUndo = class(TCustomUndo)
  private
    FOldBeforeAdditions: TStrings;
    FOldAfterAdditions: TStrings;
    FNewBeforeAdditions: TStrings;
    FNewAfterAdditions: TStrings;
    FModel: TCustomModel;
  protected
    function Description: string; override;
  public
    Constructor Create(Model: TCustomModel; NewBeforeAdditions,
      NewAfterAdditions: TStrings);
    Destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

  TLgrBatchFileAdditionsUndo = class(TCustomUndo)
  private
    FNewAssoc: TModelAssociations;
    FOldAssoc: TModelAssociations;
    procedure Apply(Associations: TModelAssociations);
  protected
    function Description: string; override;
  public
    Constructor Create(var NewAssoc: TModelAssociations);
    Destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

var
  frmBatchFileAdditions: TfrmBatchFileAdditions;

implementation

uses
  frmGoPhastUnit, contnrs, GoPhastTypes;

resourcestring
  StrChangeBatchFileAd = 'change batch file additions';

{$R *.dfm}

{ TfrmBatchFileAdditions }

procedure TfrmBatchFileAdditions.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmBatchFileAdditions.comboModelChange(Sender: TObject);
var
  Assoc: TModelLinesAssociation;
begin
  inherited;
  Assoc := comboModel.Items.Objects[comboModel.ItemIndex] as TModelLinesAssociation;
  CurrentModelLines := Assoc;
end;

procedure TfrmBatchFileAdditions.FormCreate(Sender: TObject);
begin
  inherited;
  FAssociations := TModelAssociations.Create;
  GetData;
end;

procedure TfrmBatchFileAdditions.FormDestroy(Sender: TObject);
begin
  inherited;
  FAssociations.Free;
end;

procedure TfrmBatchFileAdditions.GetData;
var
  Assoc: TModelLinesAssociation;
  Index: Integer;
  ChildModel: TChildModel;
begin
  Assoc := FAssociations.Assoc[frmGoPhast.PhastModel];
  comboModel.AddItem(StrParentModel, Assoc);
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for Index := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := frmGoPhast.PhastModel.ChildModels[Index].ChildModel;
      if ChildModel <> nil then
      begin
        Assoc := FAssociations.Assoc[ChildModel];
        comboModel.AddItem(ChildModel.ModelName, Assoc);
      end;
    end;
  end
  else
  begin
    pnlModel.Visible := False;
  end;
  comboModel.ItemIndex := 0;
  comboModelChange(nil);
  frameBatchFileBefore.Height := (frameBatchFileBefore.Height
    + frameBatchFileAfter.Height) div 2;

//  frameBatchFileBefore.memoLines.Lines := frmGoPhast.PhastModel.BatchFileAdditionsBeforeModel;
//  frameBatchFileAfter.memoLines.Lines := frmGoPhast.PhastModel.BatchFileAdditionsAfterModel;
end;

procedure TfrmBatchFileAdditions.SetCurrentModelLines(
  const Value: TModelLinesAssociation);
begin
  if FCurrentModelLines <> Value then
  begin
    if FCurrentModelLines <> nil then
    begin
      FCurrentModelLines.BeforeLines.Assign(frameBatchFileBefore.memoLines.Lines);
      FCurrentModelLines.AfterLines.Assign(frameBatchFileAfter.memoLines.Lines);
    end;

    FCurrentModelLines := Value;

    if FCurrentModelLines <> nil then
    begin
      frameBatchFileBefore.memoLines.Lines := FCurrentModelLines.BeforeLines;
      frameBatchFileAfter.memoLines.Lines := FCurrentModelLines.AfterLines;
    end;
  end;
end;

procedure TfrmBatchFileAdditions.SetData;
var
  Undo: TLgrBatchFileAdditionsUndo;
begin
  CurrentModelLines := nil;
  Undo := TLgrBatchFileAdditionsUndo.Create(FAssociations);
  frmGoPhast.UndoStack.Submit(Undo);
end;

{ TBatchFileAdditionsUndo }

constructor TBatchFileAdditionsUndo.Create(Model: TCustomModel; NewBeforeAdditions,
  NewAfterAdditions: TStrings);
begin
  inherited Create;
  FModel := Model;
  FOldBeforeAdditions := TStringList.Create;
  FOldBeforeAdditions.Assign(FModel.BatchFileAdditionsBeforeModel);

  FOldAfterAdditions := TStringList.Create;
  FOldAfterAdditions.Assign(FModel.BatchFileAdditionsAfterModel);

  FNewBeforeAdditions := TStringList.Create;
  FNewBeforeAdditions.Assign(NewBeforeAdditions);

  FNewAfterAdditions := TStringList.Create;
  FNewAfterAdditions.Assign(NewAfterAdditions);
end;

function TBatchFileAdditionsUndo.Description: string;
begin
  result := StrChangeBatchFileAd;
end;

destructor TBatchFileAdditionsUndo.Destroy;
begin
  FNewAfterAdditions.Free;
  FNewBeforeAdditions.Free;
  FOldAfterAdditions.Free;
  FOldBeforeAdditions.Free;
  inherited;
end;

procedure TBatchFileAdditionsUndo.DoCommand;
begin
  FModel.BatchFileAdditionsBeforeModel := FNewBeforeAdditions;
  FModel.BatchFileAdditionsAfterModel := FNewAfterAdditions;
end;

procedure TBatchFileAdditionsUndo.Undo;
begin
  FModel.BatchFileAdditionsBeforeModel := FOldBeforeAdditions;
  FModel.BatchFileAdditionsAfterModel := FOldAfterAdditions;
end;

{ TModelLinesAssociation }

constructor TModelLinesAssociation.Create(Model: TCustomModel);
begin
  Assert(Model <> nil);
  FModel := Model;
  BeforeLines := TStringList.Create;
  AfterLines := TStringList.Create;
  BeforeLines.Assign(Model.BatchFileAdditionsBeforeModel);
  AfterLines.Assign(Model.BatchFileAdditionsAfterModel);
end;

destructor TModelLinesAssociation.Destroy;
begin
  AfterLines.Free;
  BeforeLines.Free;
  inherited;
end;

{ TModelAssociations }

constructor TModelAssociations.Create;
begin
  FAssociations := TObjectList.Create;
  FCached := nil;
end;

destructor TModelAssociations.Destroy;
begin
  FAssociations.Free;
  inherited;
end;

function TModelAssociations.GetAssoc(
  Model: TCustomModel): TModelLinesAssociation;
var
  Index: Integer;
  AnItem: TModelLinesAssociation;
begin
  if (FCached <> nil) and (FCached.FModel = Model) then
  begin
    result := FCached;
  end
  else
  begin
    result := nil;
    for Index := 0 to FAssociations.Count - 1 do
    begin
      AnItem := FAssociations[Index];
      if (AnItem.FModel = Model) then
      begin
        result := AnItem;
        break;
      end;
    end;
    if result = nil then
    begin
      result := TModelLinesAssociation.Create(Model);
      FAssociations.Add(result)
    end;
    FCached := result;
  end;
end;

{ TLgrBatchFileAdditionsUndo }

constructor TLgrBatchFileAdditionsUndo.Create(var NewAssoc: TModelAssociations);
var
  Index: Integer;
  ChildModel: TChildModel;
begin
  // Take ownership of NewAssoc
  FNewAssoc := NewAssoc;
  NewAssoc := nil;

  FOldAssoc := TModelAssociations.Create;
  FOldAssoc.Assoc[frmGoPhast.PhastModel];
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for Index := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := frmGoPhast.PhastModel.ChildModels[Index].ChildModel;
      if ChildModel <> nil then
      begin
        FOldAssoc.Assoc[ChildModel];
      end;
    end;
  end;
end;

function TLgrBatchFileAdditionsUndo.Description: string;
begin
  result := StrChangeBatchFileAd;
end;

destructor TLgrBatchFileAdditionsUndo.Destroy;
begin
  FNewAssoc.Free;
  FOldAssoc.Free;
  inherited;
end;

procedure TLgrBatchFileAdditionsUndo.DoCommand;
begin
  inherited;
  Apply(FNewAssoc);
end;

procedure TLgrBatchFileAdditionsUndo.Undo;
begin
  inherited;
  Apply(FOldAssoc);
end;

procedure TLgrBatchFileAdditionsUndo.Apply(Associations: TModelAssociations);
var
  ChildModel: TChildModel;
  Assoc: TModelLinesAssociation;
  Index: Integer;
begin
  Assoc := Associations.Assoc[frmGoPhast.PhastModel];
  frmGoPhast.PhastModel.BatchFileAdditionsBeforeModel := Assoc.BeforeLines;
  frmGoPhast.PhastModel.BatchFileAdditionsAfterModel := Assoc.AfterLines;
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for Index := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := frmGoPhast.PhastModel.ChildModels[Index].ChildModel;
      if ChildModel <> nil then
      begin
        Assoc := Associations.Assoc[ChildModel];
        ChildModel.BatchFileAdditionsBeforeModel := Assoc.BeforeLines;
        ChildModel.BatchFileAdditionsAfterModel := Assoc.AfterLines;
      end;
    end;
  end;
end;

end.
