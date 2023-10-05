unit CustomMf6PersistentUnit;

interface

uses
  System.SysUtils, System.Classes;

type
  TCustomMf6Persistent = class(TPersistent)
  protected
    procedure Initialize; virtual;
    function StripFollowingComments(AValue: string): string;
    function ReadEndOfSection(ALine: string; const ErrorLine: string;
      const SectionName: string; Unhandled: TStreamWriter): boolean;

  var
    FSplitter: TStringList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

constructor TCustomMf6Persistent.Create;
begin
  FSplitter := TStringList.Create;
  Initialize;
end;

function TCustomMf6Persistent.StripFollowingComments(AValue: string): string;
var
  CommentPosition: Integer;
  SingleQuotePostion: Integer;
  CharIndex: Integer;
  QuoteCount: Integer;
begin
  SingleQuotePostion := Pos('''', AValue);
  if SingleQuotePostion = 0 then
  begin
    CommentPosition := Pos('#', AValue);
    if CommentPosition > 0 then
    begin
      AValue := Copy(AValue, 1, CommentPosition - 1);
    end;
    CommentPosition := Pos('!', AValue);
    if CommentPosition > 0 then
    begin
      AValue := Copy(AValue, 1, CommentPosition - 1);
    end;
    CommentPosition := Pos('//', AValue);
    if CommentPosition > 0 then
    begin
      AValue := Copy(AValue, 1, CommentPosition - 1);
    end;
  end
  else
  begin
    CommentPosition := Pos('#', AValue);
    if CommentPosition > 0 then
    begin
      QuoteCount := 0;
      for CharIndex := 1 to Length(AValue) do
      begin
        if AValue[CharIndex] = '''' then
        begin
          Inc(QuoteCount);
        end;
        if AValue[CharIndex] = '#' then
        begin
          if not Odd(QuoteCount) then
          begin
            AValue := Copy(AValue, 1, CharIndex - 1);
            Break;
          end;
        end;
      end;
    end;
    CommentPosition := Pos('!', AValue);
    if CommentPosition > 0 then
    begin
      QuoteCount := 0;
      for CharIndex := 1 to Length(AValue) do
      begin
        if AValue[CharIndex] = '''' then
        begin
          Inc(QuoteCount);
        end;
        if AValue[CharIndex] = '!' then
        begin
          if not Odd(QuoteCount) then
          begin
            AValue := Copy(AValue, 1, CharIndex - 1);
            Break;
          end;
        end;
      end;
    end;
    CommentPosition := Pos('//', AValue);
    if CommentPosition > 0 then
    begin
      QuoteCount := 0;
      for CharIndex := 1 to Length(AValue) do
      begin
        if AValue[CharIndex] = '''' then
        begin
          Inc(QuoteCount);
        end;
        if Copy(AValue, CharIndex, 2) = '//' then
        begin
          if not Odd(QuoteCount) then
          begin
            AValue := Copy(AValue, 1, CharIndex - 1);
            Break;
          end;
        end;
      end;
    end;
  end;
  result := Trim(AValue);
end;

destructor TCustomMf6Persistent.Destroy;
begin
  FSplitter.Free;
  inherited;
end;

procedure TCustomMf6Persistent.Initialize;
begin
  FSplitter.QuoteChar := '''';
  FSplitter.Delimiter := ',';
end;

function TCustomMf6Persistent.ReadEndOfSection(ALine: string;
  const ErrorLine: string; const SectionName: string;
  Unhandled: TStreamWriter): boolean;
begin
  result := False;
  FSplitter.DelimitedText := StripFollowingComments(UpperCase(ALine));
  if FSplitter.Count > 0 then
  begin
    if FSplitter[0] = 'END' then
    begin
      if FSplitter.Count > 1 then
      begin
        if FSplitter[1] = SectionName then
        begin
          result := True;
        end
        else
        begin
          Unhandled.WriteLine(Format('Error reading the following %s line.', [SectionName]));
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else
      begin
        Unhandled.WriteLine(Format('Error reading the following %s line.', [SectionName]));
        Unhandled.WriteLine(ErrorLine);
      end;
    end;
  end
  else
  begin
    Unhandled.WriteLine(Format('Error reading the following %s line.', [SectionName]));
    Unhandled.WriteLine(ErrorLine);
  end;
end;

end.
