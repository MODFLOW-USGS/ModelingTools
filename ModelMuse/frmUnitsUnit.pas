{@abstract(The main purpose of @name is to define @link(TfrmUnits)
  which is used to edit
  frmGoPhast.Model.@link(TPhastModel.Title) and
  frmGoPhast.Model.@link(TPhastModel.Units).)

  @name also defines @link(TUndoTitleAndUnits) which is
  used to set or undo the setting of
  frmGoPhast.Model.@link(TPhastModel.Title) and
  frmGoPhast.Model.@link(TPhastModel.Units).}
unit frmUnitsUnit;

interface
  { TODO : Provide an easy way to specify consistent units. }
uses System.UITypes,
  Windows, SysUtils, Types, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, frmCustomGoPhastUnit, Buttons, UndoItems, PhastModelUnit;

type
  {@abstract(@name is used to edit
    frmGoPhast.Model.@link(TPhastModel.Title) and
    frmGoPhast.Model.@link(TPhastModel.Units).)}
  TfrmUnits = class(TfrmCustomGoPhast)
    // @name: TBitBtn;
    // Clicking @name closes @classname without changing anything.
    btnCancel: TBitBtn;
    // @name: TBitBtn;
    // Clicking @name displays help on @classname.
    btnHelp: TBitBtn;
    // @name: TBitBtn;
    // See @link(btnOKClick).
    btnOK: TBitBtn;
    // @name: TComboBox;
    // @name is used to specify the units for dispersivity in PHAST.
    comboDispersivityUnits: TComboBox;
    // @name: TComboBox;
    // @name is used to specify the length units for flux in PHAST.
    comboFluxLengthUnits: TComboBox;
    // @name: TComboBox;
    // @name is used to specify the time units for flux in PHAST.
    comboFluxTimeUnits: TComboBox;
    // @name: TComboBox;
    // @name is used to specify the units for head in PHAST.
    comboHeadUnits: TComboBox;
    // @name: TComboBox;
    // @name is used to specify the units for the
    // horizontal grid dimension in PHAST.
    comboHorizGridUnits: TComboBox;
    // @name: TComboBox;
    // @name is used to specify the length units for
    // hydraulic conductivity in PHAST.
    comboHydraulicCondLengthUnits: TComboBox;
    // @name: TComboBox;
    // @name is used to specify the time units for
    // hydraulic conductivity in PHAST.
    comboHydraulicCondTimeUnits: TComboBox;
    // @name: TComboBox;
    // @name is used to specify the length units for
    // hydraulic conductivity in leaky boundaries PHAST.
    comboLeakyHydCondLengthUnits: TComboBox;
    // @name: TComboBox;
    // @name is used to specify the time units for
    // hydraulic conductivity in leaky boundaries PHAST.
    comboLeakyHydCondTimeUnits: TComboBox;
    // @name: TComboBox;
    // @name is used to specify the units for
    // thickness in leaky boundaries PHAST.
    comboLeakyThicknessUnits: TComboBox;
    // @name: TComboBox;
    // @name is used to specify the length units for
    // hydraulic conductivity in river boundaries PHAST.
    comboRiverHydCondLengthUnits: TComboBox;
    // @name: TComboBox;
    // @name is used to specify the time units for
    // hydraulic conductivity in river boundaries PHAST.
    comboRiverHydCondTimeUnits: TComboBox;
    // @name: TComboBox;
    // @name is used to specify the units for
    // thickness in river boundaries PHAST.
    comboRiverThicknessUnits: TComboBox;
    // @name: TComboBox;
    // @name is used to specify the units for specific storage in PHAST.
    comboSpecificStorageUnits: TComboBox;
    // @name: TComboBox;
    // @name is used to specify the units for time in PHAST.
    comboTimeUnits: TComboBox;
    // @name: TComboBox;
    // @name is used to specify the units for the
    // vertical grid dimension in PHAST.
    comboVertGridUnits: TComboBox;
    // @name: TComboBox;
    // @name is used to specify the units for well diameter in PHAST.
    comboWellDiameterUnits: TComboBox;
    // @name: TComboBox;
    // @name is used to specify the time units for the
    // flow rate in well boundaries in PHAST.
    comboWellFlowTimeUnits: TComboBox;
    // @name: TComboBox;
    // @name is used to specify the volume units for the
    // flow rate in well boundaries in PHAST.
    comboWellFlowVolumeUnits: TComboBox;
    // @name: TLabel;
    // @name displays "Dispersivity units".
    lblDispersivityUnits: TLabel;
    // @name: TLabel;
    // @name displays "Flux units".
    lblFluxLengthUnits: TLabel;
    // @name: TLabel;
    // @name displays "per".
    lblFluxTimeUnits: TLabel;
    // @name: TLabel;
    // @name displays "Head units".
    lblHeadUnits: TLabel;
    // @name: TLabel;
    // @name displays "Horizontal grid units".
    lblHorizGridUnits: TLabel;
    // @name: TLabel;
    // @name displays "Hydraulic conductivity units".
    lblHydraulicCondLengthUnits: TLabel;
    // @name: TLabel;
    // @name displays "per".
    lblHydraulicCondTimeUnits: TLabel;
    // @name: TLabel;
    // @name displays "Leaky hydraulic conductivity units".
    lblLeakyHydCondLengthUnits: TLabel;
    // @name: TLabel;
    // @name displays "per".
    lblLeakyHydCondTimeUnits: TLabel;
    // @name: TLabel;
    // @name displays "Leaky thickness units".
    lblLeakyThicknessUnits: TLabel;
    // @name: TLabel;
    // @name displays "River bed hydraulic conductivity units".
    lblRiverHydCondLengthUnits: TLabel;
    // @name: TLabel;
    // @name displays "per".
    lblRiverHydCondTimeUnits: TLabel;
    // @name: TLabel;
    // @name displays "River bed thickness units".
    lblRiverThicknessUnits: TLabel;
    // @name: TLabel;
    // @name displays "Specific storage units".
    lblSpecificStorageUnits: TLabel;
    // @name: TLabel;
    // @name displays "Time units".
    lblTimeUnits: TLabel;
    // @name: TLabel;
    // @name displays "Title (Only the first two lines will be printed in the output)".
    lblTitle: TLabel;
    // @name: TLabel;
    // @name displays "Vertical grid units".
    lblVertGridUnits: TLabel;
    // @name: TLabel;
    // @name displays "Well diameter units".
    lblWellDiameterUnits: TLabel;
    // @name: TLabel;
    // @name displays "per".
    lblWellFlowTimeUnits: TLabel;
    // @name: TLabel;
    // @name displays "Well flow rate units".
    lblWellFlowVolumeUnits: TLabel;
    // @name: TMemo;
    // @name displays the title used in PHAST.
    memoTitle: TMemo;
    // @name calls @link(ValidateTitle) to check that the title is valid.
    // If it is, it calls @link(SetData) and closes @classname.
    procedure btnOKClick(Sender: TObject);
    // @name calls @link(GetData).
    procedure FormCreate(Sender: TObject); override;
  private
    // @name displays the frmGoPhast.Model.@link(TPhastModel.Title)
    // and frmGoPhast.Model.@link(TPhastModel.Units)
    // in @classname.
    procedure GetData;
    // @name sets frmGoPhast.Model.@link(TPhastModel.Title)
    // and frmGoPhast.Model.@link(TPhastModel.Units) to the values shown
    // in @classname using a @link(TUndoTitleAndUnits).
    procedure SetData;
    // @name checks that the title in @link(memoTitle) is valid.  If not,
    // it displays a warning message and returns false.
    function ValidateTitle: boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

  {@abstract(@name is used to set or undo the setting of
    frmGoPhast.Model.@link(TPhastModel.Title) and
    frmGoPhast.Model.@link(TPhastModel.Units).)}
  TUndoTitleAndUnits = class(TCustomUndo)
  protected
    // @name: TStringList;
    // @name is a copy of frmGoPhast.Model.@link(TPhastModel.Title)
    // as it was when this instance of @classname was created.
    FOldTitle: TStringList;
    // @name: @link(TUnits);
    // @name is a copy of frmGoPhast.Model.@link(TPhastModel.Units)
    // as it was when this instance of @classname was created.
    FOldUnits: TUnits;
    // @name tells what @classname does.
    function Description: string; override;
  public
    // @name: TStringList;
    // @name is a new version of frmGoPhast.Model.@link(TPhastModel.Title).
    FNewTitle: TStringList;
    // @name: @link(TUnits);
    // @name is a new version of frmGoPhast.Model.@link(TPhastModel.Units).
    FNewUnits: TUnits;
    // @name creates a new instance of @classname.
    // @name creates @link(FOldTitle), @link(FOldUnits),
    // @link(FNewTitle), and @link(FNewUnits).
    // @name assigns frmGoPhast.Model.@link(TPhastModel.Title)
    // to @link(FOldTitle) and frmGoPhast.Model.@link(TPhastModel.Units)
    // to @link(FOldUnits).
    constructor Create;
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
    // @name assigns @link(FNewTitle) to
    // frmGoPhast.Model.@link(TPhastModel.Title)
    // and @link(FNewUnits) to
    // frmGoPhast.Model.@link(TPhastModel.Units).
    procedure DoCommand; override;
    // @name assigns @link(FOldTitle) to
    // frmGoPhast.Model.@link(TPhastModel.Title)
    // and @link(FOldUnits) to
    // frmGoPhast.Model.@link(TPhastModel.Units).
    procedure Undo; override;
  end;

implementation

uses frmGoPhastUnit, GoPhastTypes;

resourcestring
  StrTitleAndUnits = 'title and units';
  StrLineDOfTheTitle = 'Line %d of the title contains a semicolon.  This is ' +
  'invalid because the semicolon is the line continuation character in PHAST' +
  '.';

{$R *.dfm}

{ TfrmUnits }

function TfrmUnits.ValidateTitle: boolean;
var
  LineIndex: integer;
  Line: string;
begin
  result := True;

  // The semicolon is the line continuation character in PHAST.
  for LineIndex := 0 to memoTitle.Lines.Count - 1 do
  begin
    Line := memoTitle.Lines[LineIndex];
    if Pos(';', Line) >= 1 then
    begin
      result := False;
      Beep;
      MessageDlg(Format(StrLineDOfTheTitle, [LineIndex + 1]),
        mtError, [mbOK], 0);
      Exit;
    end;
  end;
end;

procedure TfrmUnits.btnOKClick(Sender: TObject);
begin
  inherited;
  if not ValidateTitle then
  begin
    Exit;
  end;

  SetData;
  Close;
end;

procedure TfrmUnits.GetData;
begin
  with frmGoPhast.PhastModel do
  begin
    memoTitle.Lines := Title;
    with Units do
    begin
      comboTimeUnits.ItemIndex := Ord(DefaultTimeUnits);
      comboHorizGridUnits.ItemIndex := Ord(DefaultHorizontalGridUnits);
      comboVertGridUnits.ItemIndex := Ord(DefaultVerticalGridUnits);
      comboHeadUnits.ItemIndex := Ord(DefaultHeadUnits);
      comboHydraulicCondLengthUnits.ItemIndex :=
        Ord(DefaultHydraulicConductivityLengthUnits);
      comboHydraulicCondTimeUnits.ItemIndex :=
        Ord(DefaultHydraulicConductivityTimeUnits);
      comboSpecificStorageUnits.ItemIndex := Ord(DefaultSpecificStorageUnits);
      comboDispersivityUnits.ItemIndex := Ord(DefaultDispersivityUnits);
      comboFluxLengthUnits.ItemIndex := Ord(DefaultFluxLengthUnits);
      comboFluxTimeUnits.ItemIndex := Ord(DefaultFluxTimeUnits);
      comboLeakyHydCondLengthUnits.ItemIndex :=
        Ord(DefaultLeakyHydraulicConductivityLengthUnits);
      comboLeakyHydCondTimeUnits.ItemIndex :=
        Ord(DefaultLeakyHydraulicConductivityTimeUnits);
      comboLeakyThicknessUnits.ItemIndex := Ord(DefaultLeakyThicknessUnits);
      comboWellDiameterUnits.ItemIndex := Ord(DefaultWellDiameterUnits);
      comboWellFlowVolumeUnits.ItemIndex := Ord(DefaultWellFlowVolumnUnits);
      comboWellFlowTimeUnits.ItemIndex := Ord(DefaultWellFlowTimeUnits);
      comboRiverHydCondLengthUnits.ItemIndex :=
        Ord(DefaultRiverBedHydraulicConductivityLengthUnits);
      comboRiverHydCondTimeUnits.ItemIndex :=
        Ord(DefaultRiverBedHydraulicConductivityTimeUnits);
      comboRiverThicknessUnits.ItemIndex := Ord(DefaultRiverBedThicknessUnits);
    end;
  end;
end;

procedure TfrmUnits.SetData;
var
  Undo: TUndoTitleAndUnits;
  PriorUpToDate: boolean;
begin
  PriorUpToDate := frmGoPhast.PhastModel.UpToDate;
  Undo := TUndoTitleAndUnits.Create;
  try
    with Undo do
    begin
      FNewTitle.Assign(memoTitle.Lines);

      with FNewUnits do
      begin
        DefaultTimeUnits := TTimeUnits(comboTimeUnits.ItemIndex);
        DefaultHorizontalGridUnits :=
          TLengthUnits(comboHorizGridUnits.ItemIndex);
        DefaultVerticalGridUnits := TLengthUnits(comboVertGridUnits.ItemIndex);
        DefaultHeadUnits := TLengthUnits(comboHeadUnits.ItemIndex);
        DefaultHydraulicConductivityLengthUnits :=
          TLengthUnits(comboHydraulicCondLengthUnits.ItemIndex);
        DefaultHydraulicConductivityTimeUnits :=
          TTimeUnits(comboHydraulicCondTimeUnits.ItemIndex);
        DefaultSpecificStorageUnits :=
          TInverseLengthUnits(comboSpecificStorageUnits.ItemIndex);
        DefaultDispersivityUnits :=
          TLengthUnits(comboDispersivityUnits.ItemIndex);
        DefaultFluxLengthUnits := TLengthUnits(comboFluxLengthUnits.ItemIndex);
        DefaultFluxTimeUnits := TTimeUnits(comboFluxTimeUnits.ItemIndex);
        DefaultLeakyHydraulicConductivityLengthUnits :=
          TLengthUnits(comboLeakyHydCondLengthUnits.ItemIndex);
        DefaultLeakyHydraulicConductivityTimeUnits :=
          TTimeUnits(comboLeakyHydCondTimeUnits.ItemIndex);
        DefaultLeakyThicknessUnits :=
          TLengthUnits(comboLeakyThicknessUnits.ItemIndex);
        DefaultWellDiameterUnits :=
          TLengthUnits(comboWellDiameterUnits.ItemIndex);
        DefaultWellFlowVolumnUnits :=
          TVolumeUnits(comboWellFlowVolumeUnits.ItemIndex);
        DefaultWellFlowTimeUnits :=
          TTimeUnits(comboWellFlowTimeUnits.ItemIndex);
        DefaultRiverBedHydraulicConductivityLengthUnits :=
          TLengthUnits(comboRiverHydCondLengthUnits.ItemIndex);
        DefaultRiverBedHydraulicConductivityTimeUnits :=
          TTimeUnits(comboRiverHydCondTimeUnits.ItemIndex);
        DefaultRiverBedThicknessUnits :=
          TLengthUnits(comboRiverThicknessUnits.ItemIndex);
      end;
    end;
  except
    Undo.Free;
    raise;
  end;
  frmGoPhast.PhastModel.UpToDate := PriorUpToDate;
  frmGoPhast.UndoStack.Submit(Undo);
end;

procedure TfrmUnits.FormCreate(Sender: TObject);
begin
  inherited;
  Constraints.MinHeight := Height;
  GetData;
end;

{ TUndoTitleAndUnits }

constructor TUndoTitleAndUnits.Create;
begin
  FOldTitle := TStringList.Create;
  FOldTitle.Assign(frmGoPhast.PhastModel.Title);
  FNewTitle := TStringList.Create;
  FOldUnits := TUnits.Create;
  FOldUnits.Assign(frmGoPhast.PhastModel.Units);
  FNewUnits := TUnits.Create;
end;

function TUndoTitleAndUnits.Description: string;
begin
  result := StrTitleAndUnits;
end;

destructor TUndoTitleAndUnits.Destroy;
begin
  FOldTitle.Free;
  FNewTitle.Free;
  FOldUnits.Free;
  FNewUnits.Free;
  inherited;
end;

procedure TUndoTitleAndUnits.DoCommand;
begin
  frmGoPhast.PhastModel.Title := FNewTitle;
  frmGoPhast.PhastModel.Units.Assign(FNewUnits);
end;

procedure TUndoTitleAndUnits.Undo;
begin
  frmGoPhast.PhastModel.Title := FOldTitle;
  frmGoPhast.PhastModel.Units := FOldUnits;
end;

end.

