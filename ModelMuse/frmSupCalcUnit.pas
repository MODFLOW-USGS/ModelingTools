unit frmSupCalcUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomGoPhastUnit, Vcl.StdCtrls,
  ArgusDataEntry, Vcl.Buttons, Vcl.ExtCtrls, Vcl.Mask, JvExMask, JvToolEdit,
  UndoItems, SvdaPrepPropertiesUnit;

type
  TUndoSupCalc = class(TCustomUndo)
  private
    FOldSupCalc: TSupCalcProperties;
    FNewSupCalc: TSupCalcProperties;
    procedure AssignSupCalc(SupCalc: TSupCalcProperties);
  protected
    function Description: string; override;
  public
    Constructor Create(var NewSupCalc: TSupCalcProperties);
    destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

  TfrmSupCalc = class(TfrmCustomGoPhast)
    lblPestFileName: TLabel;
    fedPestControlFile: TJvFilenameEdit;
    rgSuperParameterCalculation: TRadioGroup;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    cbRunPest: TCheckBox;
    cbRunSlupCalc: TCheckBox;
    rdeExpected: TRbwDataEntry;
    lblExpected: TLabel;
    procedure FormCreate(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
  private
    procedure GetData;
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSupCalc: TfrmSupCalc;

implementation

uses
  frmGoPhastUnit;

{$R *.dfm}

{ TfrmSupCalc }

procedure TfrmSupCalc.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmSupCalc.FormCreate(Sender: TObject);
begin
  inherited;
  GetData;
end;

procedure TfrmSupCalc.GetData;
var
  SupCalc: TSupCalcProperties;
begin
  SupCalc := frmGoPhast.PhastModel.SupCalcProperties;
  fedPestControlFile.FileName := SupCalc.FileName;
  rgSuperParameterCalculation.ItemIndex := Ord(SupCalc.Method);
  rdeExpected.RealValue := SupCalc.ExpectedValue;
  cbRunPest.Checked := SupCalc.RunPest;
  cbRunSlupCalc.Checked := SupCalc.RunSupCalc;
end;

procedure TfrmSupCalc.SetData;
var
  InvalidateModelEvent: TNotifyEvent;
  SupCalc: TSupCalcProperties;
begin
  InvalidateModelEvent := nil;
  SupCalc := TSupCalcProperties.Create(InvalidateModelEvent);
  try
    SupCalc.FileName := fedPestControlFile.FileName;
    SupCalc.Method := TSupCalcMethod(rgSuperParameterCalculation.ItemIndex);
    SupCalc.ExpectedValue := rdeExpected.RealValue;
    SupCalc.RunPest := cbRunPest.Checked;
    SupCalc.RunSupCalc := cbRunSlupCalc.Checked;
    frmGoPhast.UndoStack.Submit(TUndoSupCalc.Create(SupCalc));
  finally
    SupCalc.Free;
  end;
end;

{ TUndoSupCalc }

procedure TUndoSupCalc.AssignSupCalc(SupCalc: TSupCalcProperties);
begin
  frmGoPhast.PhastModel.SupCalcProperties := SupCalc;
end;

constructor TUndoSupCalc.Create(var NewSupCalc: TSupCalcProperties);
var
  InvalidateModelEvent: TNotifyEvent;
begin
  InvalidateModelEvent := nil;
  FOldSupCalc := TSupCalcProperties.Create(InvalidateModelEvent);
  FOldSupCalc.Assign(frmGoPhast.PhastModel.SupCalcProperties);
  FNewSupCalc := NewSupCalc;
  NewSupCalc := nil;
end;

function TUndoSupCalc.Description: string;
begin
  result := 'change supcalc options';
end;

destructor TUndoSupCalc.Destroy;
begin
  FOldSupCalc.Free;
  FNewSupCalc.Free;
  inherited;
end;

procedure TUndoSupCalc.DoCommand;
begin
  AssignSupCalc(FNewSupCalc);
  inherited;
end;

procedure TUndoSupCalc.Undo;
begin
  AssignSupCalc(FOldSupCalc);
  inherited;
end;

end.
