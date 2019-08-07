unit DataFileWriterUnit;

interface

  uses CustomFileWriterUnit, DataItemUnit, Dialogs, GlobalData, GlobalTypesUnit,
       SysUtils;

  type TDataFileWriter = class(TCustomFileWriter)
    private
      fNrow: Integer;
      fNcol: Integer;
      fFType: TDataFileType;
      fWriteHeader: boolean;
      procedure FailSetNcol(Nc: integer);
      procedure FailSetNrow(Nr: integer);
    protected
      procedure SetFileType(FileType: string); virtual;
      function GetFileType: string;
    public
      HeaderArray: array of TStringItem;
      constructor CreateAndAllocate(Nrow: integer; Ncol: integer);  virtual;
      property Ncol: integer read fNcol write FailSetNcol; // do not allow Ncol to be assigned
      property Nrow: integer read fNrow write FailSetNrow; // do not allow Nrow to be assigned
      property WriteHeader: boolean read fWriteHeader write fWriteHeader;
  end;

implementation

{ TDataFileWriter }


constructor TDataFileWriter.CreateAndAllocate(Nrow: integer; Ncol: integer);
begin
  inherited Create();
  // Allocate array(s) here
end;

function TDataFileWriter.GetFileType: string;
begin
  case fFType of
    ftFixedFormat: result := 'fixed' ;
    ftCSV: result := 'csv';
  end;
end;

procedure TDataFileWriter.FailSetNcol(Nc: integer);
begin
  ShowMessage('Ncol is not assignable');
end;

procedure TDataFileWriter.FailSetNrow(Nr: integer);
begin
  ShowMessage('Nrow is not assignable');
end;

procedure TDataFileWriter.SetFileType(FileType: string);
var
  FTLower: string;
begin
  FTLower := AnsiLowerCase(FileType);
  if FTLower = 'csv' then
    begin
      fFType := ftCsv;
    end
  else
    if FTLower = 'fixed' then
      begin
        fFType := ftFixedFormat;
      end
    else
      begin
        ShowMessage('Argument error in TDataFileWrite.SetFileType');
      end;
end;

end.
