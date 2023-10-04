unit CustomMf6PersistentUnit;

interface

uses
  System.SysUtils, System.Classes;

type
  TCustomMf6Persistent = class(TPersistent)
  protected
    procedure Initialize; virtual; abstract;
    function StripFollowingComments(AValue: string): string;
    function ReadEndOfSection(ALine: string; const ErrorLine: string;
      const SectionName: string; Unhandled: TStreamWriter): boolean;
  public
    constructor Create; virtual;
  end;

implementation

constructor TCustomMf6Persistent.Create;
begin
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
      AValue := Trim(Copy(AValue, 1, CommentPosition - 1));
    end;
    CommentPosition := Pos('!', AValue);
    if CommentPosition > 0 then
    begin
      AValue := Trim(Copy(AValue, 1, CommentPosition - 1));
    end;
    CommentPosition := Pos('//', AValue);
    if CommentPosition > 0 then
    begin
      AValue := Trim(Copy(AValue, 1, CommentPosition - 1));
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
  result := AValue;
end;

function TCustomMf6Persistent.ReadEndOfSection(ALine: string;
  const ErrorLine: string; const SectionName: string;
  Unhandled: TStreamWriter): boolean;
begin
  result := False;
  ALine := UpperCase(ALine);
  if Pos('END', ALine) = 1 then
  begin
    ALine := Trim(Copy(ALine, 4, MAXINT));
    if ALine = SectionName then
    begin
      result := True;
    end
    else
    begin
      Unhandled.WriteLine(Format('Error reading the following %s line.', [SectionName]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

end.
