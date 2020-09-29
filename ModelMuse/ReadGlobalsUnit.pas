unit ReadGlobalsUnit;

interface

uses
  Generics.Collections, IOUtils, Types, Vcl.Dialogs;

type
  TGlobalItem = class(TObject)
    Name: string;
    Value: string;
  end;

  TGlobalList = TObjectList<TGlobalItem>;

function ReadGlobalFile(const FileName: string; List: TGlobalList): boolean;

implementation

uses
  SysUtils;

function ReadGlobalFile(const FileName: string; List: TGlobalList): boolean;
var
  Lines: TStringDynArray;
  Index: Integer;
  AName: string;
  Value: string;
  Sep: Char;
  Item: TGlobalItem;
  ALine: string;
  SpacePos: Integer;

begin
  result := False;
  if (TFile.Exists(FileName)) then
  begin
    try
      Lines := TFile.ReadAllLines(FileName);
    except on E: EInOutError do
      begin
        Beep;
        MessageDlg(E.message, mtError, [mbOK], 0);
        Exit;
      end;
    end;
    if Length(Lines) > 0 then
    begin
      result := True;
       Sep := FormatSettings.DecimalSeparator;
       try
         FormatSettings.DecimalSeparator := '.';
         for Index := 0 to Length(Lines) - 1 do
         begin
           ALine := Trim(Lines[Index]);
           if (Length(ALine) > 0) and (ALine[1] <> '#') then
           begin
             SpacePos := Pos(' ', ALine);
             if SpacePos > 0 then
             begin
               AName := Trim(Copy(ALine, 1, SpacePos-1));
               Value := Trim(Copy(ALine, SpacePos+1, MaxInt));
               Item := TGlobalItem.Create;
               Item.Name := AName;
               Item.Value := Value;
               List.Add(Item);
             end
             else
             begin
               result := False;
               Exit;
             end;
           end;
         end;
       finally
         FormatSettings.DecimalSeparator := Sep;
       end;
    end;
  end;
end;

end.
