unit frmImportVertexValuesUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomGoPhastUnit, frameGridUnit,
  Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Buttons, RbwDataGrid4;

type
  TfrmImportVertexValues = class(TfrmCustomGoPhast)
    pnlBase: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    memoKeys: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    frameValues: TframeGrid;
    procedure memoKeysChange(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
  private
    procedure InitializeGrid;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmImportVertexValues: TfrmImportVertexValues;

implementation

{$R *.dfm}

procedure TfrmImportVertexValues.FormCreate(Sender: TObject);
begin
  inherited;
  InitializeGrid
end;

procedure TfrmImportVertexValues.InitializeGrid;
begin
  frameValues.Grid.Cells[0,0] := 'X';
  frameValues.Grid.Cells[1,0] := 'Y';
end;

procedure TfrmImportVertexValues.memoKeysChange(Sender: TObject);
var
  Lines: TStringList;
  ColIndex: Integer;
  FirstCol: TRbwColumn4;
begin
  inherited;
  Lines := TStringList.Create;
  try
    Lines.Assign(memoKeys.Lines);
    frameValues.Grid.ColCount := Lines.Count + 2;
    FirstCol := frameValues.Grid.Columns[0];
    for ColIndex := 0 to Lines.Count - 1 do
    begin
      frameValues.Grid.Cells[ColIndex+2,0] := Lines[ColIndex];
      frameValues.Grid.Columns[ColIndex+2].Assign(FirstCol);
    end;
  finally
    Lines.Free;
  end;
end;

end.
