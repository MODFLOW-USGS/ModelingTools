unit frmFileTypesUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomGoPhastUnit, Vcl.Grids,
  RbwDataGrid4, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, ExtensionTypeUnit;

type
  TFileColumns = (fcExtension, fcFunction, fcDescription);

  TfrmFileTypes = class(TfrmCustomGoPhast)
    pnlBottom: TPanel;
    btnClose: TBitBtn;
    btnHelp: TBitBtn;
    rdgFileTypes: TRbwDataGrid4;
    procedure FormCreate(Sender: TObject); override;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure rdgFileTypesMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject); override;
  private
    FExtensionList: TExtensionList;
    procedure GetData;
    procedure FillTable;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmFileTypes: TfrmFileTypes;

implementation



{$R *.dfm}

const
  FileFunctions : array [Low(TExtensionType)..High(TExtensionType)] of string
    = ('Model Input', 'Model Output', 'Modpath Input', 'Modpath Output',
    'ZoneBudget Input', 'ZoneBudget Output', 'MT3DMS or MT3D-USGS Input',
    'MT3DMS or MT3D-USGS Output',
  {$IFDEF SwiObsExtractor}
    'SwiObsExtractor Input', 'SwiObsExtractor Output',
  {$ENDIF}
    'Other Input', 'Other Output', 'Ancillary');

procedure TfrmFileTypes.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  Release;
end;

procedure TfrmFileTypes.FormCreate(Sender: TObject);
begin
  inherited;
  FExtensionList := TExtensionList.Create;
  GetData;
end;

procedure TfrmFileTypes.FormDestroy(Sender: TObject);
begin
  FExtensionList.Free;
  inherited;
end;

procedure TfrmFileTypes.FillTable;
var
  index: Integer;
  ExtType: TExtensionObject;
begin
  rdgFileTypes.BeginUpdate;
  try
    for index := 0 to FExtensionList.Count - 1 do
    begin
      ExtType := FExtensionList[index];
      rdgFileTypes.Cells[Ord(fcExtension), index+1] := ExtType.Extension;
      rdgFileTypes.Cells[Ord(fcFunction), index+1] := FileFunctions[ExtType.ExtensionType];
      rdgFileTypes.Cells[Ord(fcDescription), index+1] := ExtType.Description;
      rdgFileTypes.RowHeights[index+1] := rdgFileTypes.DefaultRowHeight;
    end;
  finally
    rdgFileTypes.EndUpdate;
  end;
end;

procedure TfrmFileTypes.GetData;
begin
  rdgFileTypes.BeginUpdate;
  try
    FExtensionList.SetDefaultExtensions;
    rdgFileTypes.RowCount := FExtensionList.Count + 1;
    rdgFileTypes.Cells[Ord(fcExtension), 0] := 'Extension';
    rdgFileTypes.Cells[Ord(fcFunction), 0] := 'Function';
    rdgFileTypes.Cells[Ord(fcDescription), 0] := 'Description';

    FillTable;
  finally
    rdgFileTypes.EndUpdate;
  end;
end;

procedure TfrmFileTypes.rdgFileTypesMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow: Integer;
  SortCol: TFileColumns;
begin
  inherited;
  rdgFileTypes.MouseToCell(X, Y, ACol, ARow);
  if ARow = 0 then
  begin
    SortCol := TFileColumns(ACol);
    case SortCol of
      fcExtension: FExtensionList.SortRecords;
      fcFunction: FExtensionList.SortFunction;
      fcDescription: FExtensionList.SortDescription;
      else Assert(False);
    end;
    FillTable;
  end;
end;

end.
