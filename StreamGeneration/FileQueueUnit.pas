unit FileQueueUnit;

interface

uses
  Winapi.Windows, System.Classes, System.SysUtils;

type
  TFileQueue<T: record> = class(TObject)
  private
    type
      TBuffer = array of T;
    var
    FNewFileName: string;
    FFileStream: TFileStream;
    FReadFilePosition: Int64;
    FWriteFilePosition: Int64;
    FReadBuffer: TBuffer;
    FWriteBuffer: TBuffer;
    FReadBufferSize: integer;
    FWriteBufferSize: Integer;
    FReadBufferPosition: Integer;
    FWriteBufferPosition: Integer;
    FBufferSize: Integer;
    function GetCount: Integer;
  public
    constructor Create(FileName: string; BufferSize: Integer = 1024); overload;
    constructor Create; overload;
    destructor Destroy; override;
    procedure EnQueue(AValue: T);
    function DeQueue: T;
    property Count: Integer read GetCount;
  end;

implementation

uses
  System.IOUtils;

{ TFileQueue<T> }

constructor TFileQueue<T>.Create(FileName: string; BufferSize: Integer = 1024);
begin
  Assert(TFile.Exists(FileName));
  FBufferSize := BufferSize;

  SetLength(FReadBuffer, FBufferSize);
  SetLength(FWriteBuffer, FBufferSize);

  FReadBufferPosition := 0;
  FReadBufferSize := 0;

  FWriteBufferPosition := 0;
  FWriteBufferSize := FBufferSize;

  FFileStream := TFileStream.Create(FileName, fmOpenReadWrite);
  FReadFilePosition := FFileStream.Position;
  FWriteFilePosition := FReadFilePosition;
end;

constructor TFileQueue<T>.Create;
var
  PathName: array[0..MAX_PATH] of Char;
  NewFileName: array[0..MAX_PATH] of Char;
begin
  GetTempPath(MAX_PATH, @PathName);
  GetTempFileName(PathName, 'fqu', 0, @NewFileName);
  FNewFileName := NewFileName;
  Create(FNewFileName);
end;

function TFileQueue<T>.DeQueue: T;
var
  BytesRead: Integer;
begin
  if (FReadBufferPosition >= FReadBufferSize) then
  begin
    if (FReadFilePosition < FWriteFilePosition) then
    begin
      FFileStream.Seek(FReadFilePosition, soBeginning);
      BytesRead := FFileStream.Read(FReadBuffer[0], FBufferSize*SizeOf(T));
      FReadFilePosition := FFileStream.Position;
      FReadBufferSize := BytesRead div SizeOf(T);
      FReadBufferPosition := 0;
    end
    else
    begin
      FReadBuffer := FWriteBuffer;
      SetLength(FReadBuffer, FBufferSize);
      FReadBufferPosition := 0;
      FReadBufferSize := FWriteBufferPosition;
      FWriteBufferPosition := 0;
    end;
  end;
  Assert(FReadBufferPosition < FReadBufferSize);
  result := FReadBuffer[FReadBufferPosition];
  Inc(FReadBufferPosition);
end;

destructor TFileQueue<T>.Destroy;
begin
  FFileStream.Free;
  if FileExists(FNewFileName) then
  begin
    DeleteFile(FNewFileName);
  end;
  inherited;
end;

procedure TFileQueue<T>.EnQueue(AValue: T);
begin
  FWriteBuffer[FWriteBufferPosition] := AValue;
  Inc(FWriteBufferPosition);
  if FWriteBufferPosition = FWriteBufferSize then
  begin
    FFileStream.Seek(FWriteFilePosition, soBeginning);
    FFileStream.Write(FWriteBuffer[0], FWriteBufferSize*SizeOf(T));
    FWriteFilePosition := FFileStream.Position;
    FWriteBufferPosition := 0;
  end;

end;

function TFileQueue<T>.GetCount: Integer;
begin
  result := (FWriteFilePosition-FReadFilePosition) div SizeOf(T)
    + (FReadBufferSize - FReadBufferPosition)
    + FWriteBufferPosition;
end;

end.
