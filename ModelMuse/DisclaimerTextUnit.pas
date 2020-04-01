unit DisclaimerTextUnit;

interface

uses
  Classes;

var
  Disclaimer: TStringList;

function DisclaimerString: string;

implementation

uses
  SysUtils;

function DisclaimerString: string;
var
  LineIndex: Integer;
  {$IFDEF FPC}
  {$ELSE}
  DisclaimBuilder: TStringBuilder;
  {$ENDIF}
begin
  {$IFDEF FPC}
  result := '';
  for LineIndex := 0 to Disclaimer.Count - 1 do
  begin
    result := result + Disclaimer[LineIndex]+' ';
  end;
  {$ELSE}
  DisclaimBuilder := TStringBuilder.Create;
  try
    for LineIndex := 0 to Disclaimer.Count - 1 do
    begin
      DisclaimBuilder.Append(Disclaimer[LineIndex]);
      DisclaimBuilder.Append(' ');
    end;
    result := DisclaimBuilder.ToString;
  finally
    DisclaimBuilder.Free;
  end;
  {$ENDIF}
end;

initialization
  Disclaimer := TStringList.Create;

{$IFDEF ApprovedSoftware}
  Disclaimer.Add('This software has been approved for release by the U.S. Geological');
  Disclaimer.Add('Survey (USGS). Although the software has been subjected to rigorous');
  Disclaimer.Add('review, the USGS reserves the right to update the software as needed');
  Disclaimer.Add('pursuant to further analysis and review. No warranty, expressed or');
  Disclaimer.Add('implied, is made by the USGS or the U.S. Government as to the');
  Disclaimer.Add('functionality of the software and related material nor shall the');
  Disclaimer.Add('fact of release constitute any such warranty. Furthermore, the');
  Disclaimer.Add('software is released on condition that neither the USGS nor the U.S.');
  Disclaimer.Add('Government shall be held liable for any damages resulting from its');
  Disclaimer.Add('authorized or unauthorized use. Also refer to the USGS Water');
  Disclaimer.Add('Resources Software User Rights Notice for complete use, copyright,');
  Disclaimer.Add('and distribution information.');
{$ELSE}
  Disclaimer.Add('This software is preliminary or provisional and is subject to revision.');
  Disclaimer.Add('It is being provided to meet the need for timely best science. The');
  Disclaimer.Add('software has not received final approval by the U.S. Geological Survey');
  Disclaimer.Add('(USGS). No warranty, expressed or implied, is made by the USGS or the');
  Disclaimer.Add('U.S. Government as to the functionality of the software and related');
  Disclaimer.Add('material nor shall the fact of release constitute any such warranty. The');
  Disclaimer.Add('software is provided on the condition that neither the USGS nor the U.S.');
  Disclaimer.Add('Government shall be held liable for any damages resulting from the');
  Disclaimer.Add('authorized or unauthorized use of the software.');
{$ENDIF}


finalization
  Disclaimer.Free;

end.
