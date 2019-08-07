{*******************************************************}
{       Rapware Delphi/Kylix XML basic parser           }
{  simple XML parser for XML 2 Binary 2 XML conversion  }
{                                                       }
{       Copyright (C) 2000,2001 Rapware                 }
{                                                       }
{       www.rapware.com   info@rapware.com              }
{*******************************************************}
unit rwXMLParser;
interface
uses
  sysutils,
  classes;

{$ifdef CONDITIONALEXPRESSIONS}
  {$if CompilerVersion>=20}
    {$DEFINE Delphi_2009_UP}
  {$ifend}
{$endif}

type
  TrwXMLParser = class(TObject)
  private
    // the stream which holds the XML formated DFM / component stream
    fInputStream: TStream;
    FElementName: AnsiString;
    FAttribs: TStringList;
    FData: AnsiString;
    FEndFound: boolean;
    FValidRead: boolean;
    FBuffer: PAnsiChar;
    FStreamSize: integer;
    FShowProgress: boolean;

    // property setter for the Attribs
    procedure SetAttribs(const Value: TStringList);
    // property setter for the ElementName of the last read Element
    procedure SetElementName(const Value: AnsiString);
    // property setter for the Data of the last read XML element
    procedure SetData(const Value: AnsiString);
    // property setter for if a end is found in the last read
    procedure SetEndFound(const Value: boolean);
    // property setter for if the last read was valid
    procedure SetValidRead(const Value: boolean);
    // removes the spaces from the current line
    procedure TrimSpaces;
    // Property Setter
    procedure SetStreamSize(const Value: integer);

    // Reads a validName form the stream
    function ReadNameStr: AnsiString;
    // Read a full Attribute from the stream
    procedure ReadAttrStr(var AAttrib : AnsiString; var isLast: boolean);
    function getNextChar: AnsiChar;
    function CheckNextChar: AnsiChar;
    procedure CharBack;
    procedure ReadData;
    procedure SetShowProgress(const Value: boolean);

  protected

  public
    // The constructor gets a Stream which hold the XML formatted
    // DFM / Component stream
    constructor Create(Stream: TStream);
    // The destructor cleans all temporary claimed memory.
    destructor Destroy; override;
    // Read the Next Element with all its Attribs and data if on the same line
    procedure ReadNextElement; virtual;
    // This function returns true if we are at the end of the stream
    function EOS: boolean;
    // Returns the value of Attribute
    function ValueOf(const aName: AnsiString): AnsiString;
    // Returns the Data as a Integer
    function IntegerData: Integer;
    // The Value can be a 64 bit integer
    function Int64Data: Int64;
    // Returns the Data as a Float
    function FloatData: Extended;

    // True if the last read was ended with a valid Element
    property ValidRead: boolean read FValidRead write SetValidRead;
    // The name of the last read Element
    property ElementName: AnsiString read FElementName write SetElementName;
    // All attributes of the last read Element are saved in a stringlist
    property Attribs: TStringList read FAttribs write SetAttribs;
    // The Data in the form of a AnsiString
    property Data: AnsiString read FData write SetData;
    // If the Element was closed then this property is true
    property EndFound: boolean read FEndFound write SetEndFound;
    // The actual size of the stream.
    property StreamSize: integer read FStreamSize write SetStreamSize;

    property ShowProgress: boolean read FShowProgress write SetShowProgress;
  end;

implementation

resourcestring
  StrXMLStreamReadErro = 'XML Stream read error';


{ TrwXMLParser }



const
  BufferSize= 4096;

constructor TrwXMLParser.Create(Stream: TStream);
begin
  inherited create;
  FInputStream := Stream;
  FStreamSize := Stream.Size;
  GetMem(FBuffer,BufferSize);
  FAttribs := TStringList.Create;
  FShowProgress := false;
end;

destructor TrwXMLParser.Destroy;
begin
  FreeMem(FBuffer,BufferSize);
  FAttribs.Free;
  inherited;
end;

function TrwXMLParser.EOS : boolean;
begin
  result := (fInputStream.Position = fInputStream.Size);
end;

procedure TrwXMLParser.TrimSpaces;
var
  I: integer;
  C: AnsiChar;
begin
  I := fInputStream.Read(C,1);
  while (I=1) and ((C =' ') or (C < #32)) do
    I := fInputStream.Read(C,1);
  if I = 1 then
    fInputStream.Position := fInputStream.Position - 1;
end;

function TrwXMLParser.ReadNameStr: AnsiString;
var
  I: integer;
  C: AnsiChar;
begin
  I := fInputStream.Read(C,1);
  if I=1 then // throw away the "<"
    I := fInputStream.Read(C,1);
  if (I=1) and (C = '/') then
  begin
    EndFound := true;
    C := GetNextChar;
  end;

  {$IFDEF Delphi_2009_UP}
  while (I=1) and CharInSet(C, ['a'..'z','A'..'Z','_','0'..'9']) do
  {$ELSE}
  while (I=1) and (C in ['a'..'z','A'..'Z','_','0'..'9']) do
  {$ENDIF}
  begin
    result := result + C;
    I := fInputStream.Read(C,1);
  end;
  fInputStream.Position := fInputStream.Position - 1;
end;


function  TrwXMLParser.getNextChar : AnsiChar;
var
  I: integer;
  C: AnsiChar;
begin
  I := fInputStream.Read(C,1);
  result := C;
  if I = 0 then
    raise Exception.Create(StrXMLStreamReadErro);
end;

function  TrwXMLParser.CheckNextChar : AnsiChar;
var
  I: integer;
  C: AnsiChar;
begin
  I := fInputStream.Read(C,1);
  CharBack;
  result := C;
  if I = 0 then
    raise Exception.Create(StrXMLStreamReadErro);
end;

procedure TrwXMLParser.CharBack;
begin
  fInputStream.Position := fInputStream.Position-1;
end;

procedure TrwXMLParser.ReadAttrStr(var aAttrib : AnsiString; var isLast: boolean);
var
  C: AnsiChar;
begin
  aAttrib := '';
  repeat // remove all spaces
    C := GetNextChar;
  until C <> ' ';
  if (C = '/') or (C = '>') then
  begin
    isLast := true;
    if C = '/' then
    begin
      C := GetNextChar;
      EndFound := (C = '>');
    end;
    Exit;
  end
  else
  begin
    repeat
      aAttrib := aAttrib + C;
      C := GetNextChar;
    until (C = '=') or (C < #32);
    if C < #32 then
      raise Exception.Create(StrXMLStreamReadErro);
  end;
  while (C <> '"') do // remove all spaces
    C := GetNextChar;
  C := GetNextChar;
  aAttrib := aAttrib + '=';
  repeat
    aAttrib := aAttrib + C;
    C := GetNextChar;
    if C = '"' then
    begin
      while (C = '"') and (CheckNextChar = '"') do
      begin
        aAttrib := aAttrib + '"';
        GetNextChar;
        C := GetNextChar;
      end;
    end;
  until (C = '"') or (C < #32);       
  if C < #32 then
    raise Exception.Create(StrXMLStreamReadErro);
end;


procedure TrwXMLParser.ReadData;
var
  C: AnsiChar;
begin
  Data := '';
  C := GetNextChar;
  while C <> '<' do
  begin
    Data := Data + C;
    C := GetNextChar;
  end;
  CharBack;
end;

procedure TrwXMLParser.ReadNextElement;
var
  isLast: boolean;
  aAttrib: AnsiString;
begin
  ValidRead := false;
  ElementName := '';
  Attribs.Clear;
  Data := '';
  EndFound := false;
  TrimSpaces;
  ElementName := ReadNameStr;
  IsLast := false;
  while not isLast do
  begin
    ReadAttrStr(aAttrib, isLast);
    if aAttrib > '' then
      Attribs.Add(string(aAttrib));
  end;
  if not EndFound then
    ReadData;
end;

procedure TrwXMLParser.SetAttribs(const Value: TStringList);
begin
  FAttribs := Value;
end;

procedure TrwXMLParser.SetData(const Value: AnsiString);
begin
  FData := Value;
end;

procedure TrwXMLParser.SetElementName(const Value: AnsiString);
begin
  FElementName := Value;
end;

procedure TrwXMLParser.SetEndFound(const Value: boolean);
begin
  FEndFound := Value;
end;

procedure TrwXMLParser.SetValidRead(const Value: boolean);
begin
  FValidRead := Value;
end;

procedure TrwXMLParser.SetStreamSize(const Value: integer);
begin
  FStreamSize := Value;
end;

function TrwXMLParser.ValueOf(const aName: AnsiString) : AnsiString;
begin
  result := AnsiString(Trim(Attribs.Values[string(aName)]));
end;

function TrwXMLParser.IntegerData: Integer;
begin
  result := StrToInt(Trim(string(Data)));
end;

function TrwXMLParser.Int64Data: Int64;
begin
  result := StrToInt64(Trim(string(Data)));
end;

function TrwXMLParser.FloatData: Extended;
begin
  result := StrToFloat(Trim(string(Data)));
end;

procedure TrwXMLParser.SetShowProgress(const Value: boolean);
begin
  FShowProgress := Value;
end;

end.

