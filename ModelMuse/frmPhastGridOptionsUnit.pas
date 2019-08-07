{@abstract(The main purpose of @name is to define
  @link(TfrmPhastGridOptions) which is used to edit
    frmGoPhast.Model.@link(TPhastModel.GridOptions).)
    @name also defines @link(TUndoGridOptions).}
unit frmPhastGridOptionsUnit;

interface

uses System.UITypes,
  Windows, SysUtils, Types, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, frmCustomGoPhastUnit, CheckLst, ExtCtrls, Buttons, PhastModelUnit,
  UndoItems;

type
  {@abstract(@name is used to edit
    frmGoPhast.Model.@link(TPhastModel.GridOptions).)}
  TfrmPhastGridOptions = class(TfrmCustomGoPhast)
    // @name: TBitBtn;
    // Clicking @name closes @classname without doing anything.
    btnCancel: TBitBtn;
    // @name: TBitBtn;
    // Clicking @name displays help on @classname.
    btnHelp: TBitBtn;
    // @name: TBitBtn;
    // See @link(btnOKClick).
    btnOK: TBitBtn;
    // @name: TCheckListBox;
    // @name displays the chemistry dimensions.
    clbChemistryDimensions: TCheckListBox;
    // @name: TLabel;
    // @name displays "Chemistry dimensions".
    lblChemistrDimensions: TLabel;
    // @name: TRadioGroup;
    // @name displays the print orientation.
    rgPrintOrientation: TRadioGroup;
    // @name calls @link(GetData).
    procedure FormCreate(Sender: TObject); override;
    // @name calls @link(SetData).
    procedure btnOKClick(Sender: TObject);
  private
    // @name gets the values of frmGoPhast.Model.@link(TPhastModel.GridOptions)
    procedure GetData;
    // @name sets the values of frmGoPhast.Model.@link(TPhastModel.GridOptions)
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

  // @abstract(@name is used to undo or redo the setting of
  // frmGoPhast.Model.@link(TPhastModel.GridOptions).)
  TUndoGridOptions = class(TCustomUndo)
  protected
    // @name holds a copy of frmGoPhast.Model.@link(TPhastModel.GridOptions)
    // that was present when @name was created.
    FOldGridOptions: TGridOptions;
    // @name displays what @classname does.
    function Description: string; override;
  public
    // @name holds a copy of new values for
    // frmGoPhast.Model.@link(TPhastModel.GridOptions).
    FNewGridOptions: TGridOptions;
    // @name creates the fields, and assigns @link(FOldGridOptions).
    constructor Create;
    // @name destroys the fields.
    destructor Destroy; override;
    // @name applies @link(FNewGridOptions) to
    // frmGoPhast.Model.@link(TPhastModel.GridOptions).
    procedure DoCommand; override;
    // @name applies @link(FOldGridOptions) to
    // frmGoPhast.Model.@link(TPhastModel.GridOptions).
    procedure Undo; override;
  end;

implementation

uses frmGoPhastUnit;

resourcestring
  StrGridOptions = 'grid options';
  StrYouMustCheckAtLe = 'You must check at least one of the chemical dimensi' +
  'ons check boxes.';

{$R *.dfm}

{ TfrmPhastGridOptions }

procedure TfrmPhastGridOptions.GetData;
begin
  with frmGoPhast.PhastModel.GridOptions do
  begin
    clbChemistryDimensions.Checked[0] := ChemicalDimensionX;
    clbChemistryDimensions.Checked[1] := ChemicalDimensionY;
    clbChemistryDimensions.Checked[2] := ChemicalDimensionZ;
    rgPrintOrientation.ItemIndex := Ord(PrintOrientation);
  end;
end;

procedure TfrmPhastGridOptions.SetData;
var
  Undo: TUndoGridOptions;
  PriorUpToDate: boolean;
begin
  PriorUpToDate := frmGoPhast.PhastModel.UpToDate;
  Undo := TUndoGridOptions.Create;
  try
    with Undo.FNewGridOptions do
    begin
      ChemicalDimensionX := clbChemistryDimensions.Checked[0];
      ChemicalDimensionY := clbChemistryDimensions.Checked[1];
      ChemicalDimensionZ := clbChemistryDimensions.Checked[2];
      PrintOrientation := TpgPrintOrientation(rgPrintOrientation.ItemIndex);
    end;
  except
    Undo.Free;
    raise;
  end;
  frmGoPhast.PhastModel.UpToDate := PriorUpToDate;
  frmGoPhast.UndoStack.Submit(Undo);
end;

procedure TfrmPhastGridOptions.FormCreate(Sender: TObject);
begin
  inherited;
  Constraints.MinWidth := Width;
  GetData;
end;

procedure TfrmPhastGridOptions.btnOKClick(Sender: TObject);
begin
  inherited;
  if not (clbChemistryDimensions.Checked[0]
    or clbChemistryDimensions.Checked[1]
    or clbChemistryDimensions.Checked[2]) then
  begin
    Beep;
    MessageDlg(StrYouMustCheckAtLe, mtError, [mbOK], 0);
    Exit;
  end;
  SetData;
  ModalResult := mrOK;
end;

{ TUndoGridOptions }

constructor TUndoGridOptions.Create;
begin
  inherited;
  FOldGridOptions := TGridOptions.Create;
  FOldGridOptions.Assign(frmGoPhast.PhastModel.GridOptions);
  FNewGridOptions := TGridOptions.Create;
end;

function TUndoGridOptions.Description: string;
begin
  result := StrGridOptions;
end;

destructor TUndoGridOptions.Destroy;
begin
  FOldGridOptions.Free;
  FNewGridOptions.Free;
  inherited;
end;

procedure TUndoGridOptions.DoCommand;
begin
  frmGoPhast.PhastModel.GridOptions.Assign(FNewGridOptions);
end;

procedure TUndoGridOptions.Undo;
begin
  frmGoPhast.PhastModel.GridOptions.Assign(FOldGridOptions);
end;

end.

