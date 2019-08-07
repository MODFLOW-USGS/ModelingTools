unit frmFluidPropertiesUnit;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, frmCustomGoPhastUnit, clxDataEntry, QButtons, UndoItems, ModelUnit;

type
  TfrmFluidProperties = class(TfrmCustomGoPhast)
    adeCompressibility: TRbwDataEntry;
    adeDensity: TRbwDataEntry;
    adeDiffusivity: TRbwDataEntry;
    adeViscosity: TRbwDataEntry;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    btnOK: TBitBtn;
    BitBtn2: TBitBtn;
    Label5: TLabel;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
  private
    procedure GetData;
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

  TUndoFluidProperties = class(TCustomUndo)
  protected
    OldFluidProperties: TFluidProperties;
    function Description: string; override;
  public
    NewFluidProperties: TFluidProperties;
    Constructor Create;
    Destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

var
  frmFluidProperties: TfrmFluidProperties;

implementation

uses frmGoPhastUnit;

{$R *.dfm}

{ TfrmFluidProperties }

procedure TfrmFluidProperties.GetData;
begin
  with frmGoPhast.Model.FluidProperties do
  begin
    adeCompressibility.Text := FloatToStr(FluidCompressibility);
    adeDensity.Text := FloatToStr(FluidDensity);
    adeDiffusivity.Text := FloatToStr(FluidDiffusivity);
    adeViscosity.Text := FloatToStr(FluidViscosity);
  end;
end;

procedure TfrmFluidProperties.SetData;
var
  PriorUpToDate: boolean;
  Undo: TUndoFluidProperties;
begin
  PriorUpToDate := frmGoPhast.Model.UpToDate;
  Undo := TUndoFluidProperties.Create;
  try
    with Undo.NewFluidProperties do
    begin
      FluidCompressibility := StrToFloat(adeCompressibility.Text);
      FluidDensity := StrToFloat(adeDensity.Text);
      FluidDiffusivity := StrToFloat(adeDiffusivity.Text);
      FluidViscosity := StrToFloat(adeViscosity.Text);
    end;
  except
    Undo.Free;
    raise
  end;
  frmGoPhast.Model.UpToDate := PriorUpToDate;
  frmGoPhast.UndoStack.Submit(Undo);
end;

procedure TfrmFluidProperties.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmFluidProperties.FormCreate(Sender: TObject);
begin
  inherited;
  GetData;
end;
{ TUndoFluidProperties }

constructor TUndoFluidProperties.Create;
begin
  inherited;
  OldFluidProperties:= TFluidProperties.Create(nil);
  OldFluidProperties.Assign(frmGoPhast.Model.FluidProperties);
  NewFluidProperties:= TFluidProperties.Create(nil);
end;

function TUndoFluidProperties.Description: string;
begin
  result := 'fluid properties';
end;

destructor TUndoFluidProperties.Destroy;
begin
  OldFluidProperties.Free;
  NewFluidProperties.Free;
  inherited;
end;

procedure TUndoFluidProperties.DoCommand;
begin
  frmGoPhast.Model.FluidProperties.Assign(NewFluidProperties);
end;

procedure TUndoFluidProperties.Undo;
begin
  frmGoPhast.Model.FluidProperties.Assign(OldFluidProperties);
end;

end.

