unit Modflow2005Unit;

interface

  uses Classes, Dialogs, SysUtils,
       DependentsUnit, GlobalBasicData, GlobalTypesUnit, ModelMateClassesUnit,
       ModflowUnit, Utilities, JvBaseDlg, JvProgressDialog;

  function BuildMF2005InstructFiles(var aProject: TProject;
                const Iuobsv: Array of integer;
                const NumDep: Array of integer; ModelUse: TModelUse): boolean;
  function BuildMF2005PvalTemplate(var aProject: TProject;
                var aMessage: string; ModelUse: TModelUse): boolean;
  procedure GetMF2005HeadObs(const FName: TFileName; var dsTemp: TDepSet;
                var PlotSymbol: integer; var IUHOBSV: integer;
                var HOBDRY: string; var NumObs: integer;
                var JvProgressDialog: TJvProgressDialog);
  procedure GetMF2005FlowObs(const FName: TFileName; var dsTemp: TDepSet;
                var PlotSymbol: integer; const GroupName: string12;
                var IUOBSV: integer; var NumObs: integer);
  procedure GetMF2005HeadDep(const FName: TFileName; var dsTemp: TDepSet;
                var PlotSymbol: integer; var IUHOBSV: integer;
                var HOBDRY: string; var NumDep: integer;
                const aDepCat: TDepCat);

implementation

//###################################################################

function BuildMF2005InstructFiles(var aProject: TProject;
                const Iuobsv: Array of integer;
                const NumDep: Array of integer; ModelUse: TModelUse): boolean;
// Build all instruction files required for reading observations from
// MODFLOW-2005 observation output files identified by unit number in the
// first line of each Observation input file (E.g. IUHOBSV in HOB file)
// Use StandardFile option for JUPITER instruction files.
var
  I, J, K, KI, KIF, KIndex, NObsPkg, NSkip: integer;
  slNameFile: TStringList;
  IFileIndex, NDepPerFile, UnitNums: array of integer;
  InsFileNames, MOFileNames, DepOutFileNames: array of TFileName;
  slInsFiles: array of TStringList;
  AbsDir, ErrMsg, Ext, Line, Line2, Sep: string;
  Found, OK, TempResult: boolean;
  Marker: Char;
  NameFile: TFileName;
  TempFileName: string;
  TotalDeps: integer;
  GpNameTemp: string;
  DatTemp: TDepAttType;
  IDAP: integer;
  GpName: array [1..5] of string12;
begin
  TempResult := False;
  OK := True;
  case aProject.ActiveApp of
    aaUcode: Ext := 'jif';
    aaPest: Ext := 'pif';
    aaApp3: Ext := 'jif';
    aaApp4: Ext := 'jif';
  end;
  Marker := '@';
  NObsPkg := High(Iuobsv) + 1;
  KIF := 0; // Count of number of instruction files required.
  KI := -1;
  ErrMsg := '';
  slNameFile := TStringList.Create;
  // Allocate arrays
  SetLength(IFileIndex,NObsPkg); // Index of position in slInsFiles array for each package.
  SetLength(NDepPerFile,NObsPkg);
  SetLength(UnitNums,NObsPkg); // Unit numbers for each instruction file in use.
  SetLength(InsFileNames,NObsPkg);
  SetLength(MOFileNames,NObsPkg);
  SetLength(DepOutFileNames,NObsPkg);
  SetLength(slInsFiles,NObsPkg);

  GpName[1] := 'DRN_flows';
  GpName[2] := 'RIV_flows';
  GpName[3] := 'GHB_flows';
  GpName[4] := 'CHOB_flows';
  GpName[5] := 'STR_flows';

  // Number of string lists actually used for instruction files depends on
  // how Iuobsv units are assigned.  Maximum of NObsPkg instruction files
  // (string lists) may be used.
  TotalDeps := 0;
  for I := 0 to NObsPkg - 1 do
    begin
      IFileIndex[I] := 0;
      NDepPerFile[I] := 0;
      UnitNums[I] := 0;
      InsFileNames[I] := '';
      MOFileNames[I] := '';
      DepOutFileNames[I] := '';
      slInsFiles[I] := TStringList.Create;
      TotalDeps := TotalDeps + NumDep[I];
    end;
  try
    // Find names of observation output files corresponding to unit numbers
    // provided in Iuobsv, and assign appropriate number of observations to
    // each instruction file.
    case ModelUse of
      muCalib: NameFile := PathToAbsPath(ProjectDirectory, aProject.ModflowNameFile);
      muPred: NameFile := PathToAbsPath(ProjectDirectory, aProject.ModflowNameFilePred);
    end;
    if not IsNonBlank(NameFile) then
      begin
        ErrMsg := 'Project does not contain an entry for Name File';
        OK := False;
      end;
    if OK then
      begin
        if (not FileExists(NameFile)) then
          begin
            ErrMsg := 'Name File (' + NameFile + ') does not exist';
            OK := False;
          end;
      end;
    if OK then
      begin
        try
          slNameFile.LoadFromFile(NameFile);
        except
          ErrMsg := 'Unable to open Name File: ' + NameFile;
          OK := False;
        end;
      end;
    if OK then
      begin
        for I := 0 to NObsPkg - 1 do
          begin
            if GetNameForUnit(slNameFile, Iuobsv[I], DepOutFileNames[I]) then
              begin
                // Find position (KIndex) in slInsFiles of unit Iuobsv[I],
                //   if not found, increment KIF.
                KIndex := -1;
                if KIF > 0 then
                  begin
                    for K := 0 to KIF - 1 do
                      begin
                        if Iuobsv[I] = UnitNums[K] then
                          begin
                            // Unit number found; just assign KIndex.
                            KIndex := K;
                          end;
                      end;
                  end;
                if KIndex = -1 then
                  begin
                    // Unit number not found; store it and increment KIF and KIndex
                    KIF := KIF + 1;
                    KIndex := KIF - 1;
                    UnitNums[KIndex] := Iuobsv[I];
                    MOFileNames[KIndex] := DepOutFileNames[I]; // as listed in name file
                  end;
                IFileIndex[I] := KIndex;
                NDepPerFile[IFileIndex[I]] := NDepPerFile[IFileIndex[I]] + NumDep[I];
              end;
          end;
        // Construct all instruction files.
        DatTemp := datGroupName;
        IDAP := DepAttPos(DatTemp);
        // For each observation
        for J := 0 to TotalDeps - 1 do
          begin
            // Find group name for this observation
            case ModelUse of
              muCalib: GpNameTemp := aProject.ObsSet.Items[J].AllAtts.Items[IDAP].Text;
              muPred: GpNameTemp := aProject.PredSet.Items[J].AllAtts.Items[IDAP].Text;
            end;
            // Find package and string list corresponding to the group name.
            KIndex := -1;
            if (GpNameTemp = 'Heads') or (GpNameTemp = 'Head_Changes') then
                KIndex := IFileIndex[0]
            else
              for I := 1 to NObsPkg - 1 do
                begin
                  if GpNameTemp = ConvertString(GpName[I]) then
                    KIndex := IFileIndex[I]
                end;
            if KIndex > -1  then
              begin
                if slInsFiles[KIndex].Count = 0 then
                  begin
                    // Generate StandardFile header.
                    Line := Ext + ' ' + Marker;
                    slInsFiles[KIndex].Add(Line);
                    // Define NSkip based on observation type.
                    if KIndex = 0 then
                      NSkip := 1
                    else
                      NSkip := 0;
                    Line := 'StandardFile  ' + IntToStr(NSkip) + '  1  ' +
                             IntToStr(NDepPerFile[KIndex]);
                    slInsFiles[KIndex].Add(Line);
                  end;
                // Write observation name to appropriate string list for this package.
                case ModelUse of
                  muCalib: slInsFiles[KIndex].Add(ConvertString(aProject.ObsSet.Items[J].Name));
                  muPred: slInsFiles[KIndex].Add(ConvertString(aProject.PredSet.Items[J].Name));
                end;
                KI := J;
              end
            else
              begin
                ErrMsg := 'Non-standard group name found: ' + GpNameTemp +
                    '. Standard group names are: Heads, Head_Changes, ' +
                    'DRN_flows, RIV_flows, GHB_flows, CHOB_flows and STR_flows.';
                OK := False;
              end;
          end;
      end;
    if OK then
      begin
        // Check the numbers.
        case ModelUse of
          muCalib:
            begin
              if KI+1 <> aProject.ObsSet.Count then
                begin
//                  ErrMsg := 'KI+1 <> aProject.ObsSet.Count  ';
//                  OK := False;
                end;
              K := 0;
              for I := 0 to NObsPkg - 1 do
                begin
                  K := K + NumDep[I];
                end;
              if K <> aProject.ObsSet.Count then
                begin
//                  ErrMsg := ErrMsg + 'Sum obs by package <> aProject.ObsSet.Count  ';
//                  OK := False;
                end;
              K := 0;
              for I := 0 to KIF - 1 do
                begin
                  K := K + NDepPerFile[I];
                end;
              if K <> aProject.ObsSet.Count then
                begin
//                  ErrMsg := ErrMsg + 'Sum DepPerFile <> aProject.ObsSet.Count  ';
//                  OK := False;
                end;
              K := 0;
              for I := 0 to KIF - 1 do
                begin
                  K := K + slInsFiles[I].Count - 2; // Discount 2-line header
                end;
              if K <> aProject.ObsSet.Count then
                begin
//                  ErrMsg := ErrMsg + 'Sum instructions <> aProject.ObsSet.Count  ';
//                  OK := False;
                end;
            end;
          muPred:
            begin
              if KI+1 <> aProject.PredSet.Count then
                begin
//                  ErrMsg := 'KI+1 <> aProject.PredSet.Count  ';
//                  OK := False;
                end;
              K := 0;
              for I := 0 to NObsPkg - 1 do
                begin
                  K := K + NumDep[I];
                end;
              if K <> aProject.PredSet.Count then
                begin
//                  ErrMsg := ErrMsg + 'K <> aProject.PredSet.Count  ';
//                  OK := False;
                end;
              K := 0;
              for I := 0 to KIF - 1 do
                begin
                  K := K + NDepPerFile[I];
                end;
              if K <> aProject.PredSet.Count then
                begin
//                  ErrMsg := ErrMsg + 'K <> aProject.PredSet.Count  ';
//                  OK := False;
                end;
              K := 0;
              for I := 0 to KIF - 1 do
                begin
                  K := K + slInsFiles[I].Count - 2; // Discount 2-line header
                end;
              if K <> aProject.PredSet.Count then
                begin
//                  ErrMsg := ErrMsg + 'K <> aProject.PredSet.Count  ';
//                  OK := False;
                end;
            end;
        end;
      end;
    if OK then
      begin
        // Save string lists as instruction files
        if (DirectoryExists(aProject.AbsAppDirectory(ModelUse)))  then
          begin
            // Loop through required instruction files.
            for K := 0 to KIF - 1 do
              begin
                // Generate name for instruction file and save file.
                AbsDir := aProject.AbsAppDirectory(ModelUse);
                if AbsDir = PathDelimiter then
                  Sep := ''
                else
                  Sep := PathDelimiter;
                TempFileName := ExtractFileName(MOFileNames[K]) + '.' + Ext;
                InsFileNames[K] := AbsDir + Sep + TempFileName;
                slInsFiles[K].SaveToFile(InsFileNames[K]);
              end;
          end
        else
          begin
            ErrMsg := ErrMSG + 'Error in aProject.AppDirectory ';
            OK := False;
            AbsDir := aProject.AbsAppDirectory(ModelUse);
            if AbsDir = '' then
              begin
                Line := ' Application directory is undefined.  ';
                case aProject.ActiveApp of
                  aaUcode: Line2 := 'Define directory in "Basic UCODE Settings" dialog';
                  aaPest: Line2 := 'Define directory in "Pest Settings" dialog';
                  aaApp3: Line2 := '';
                  aaApp4: Line2 := '';
                end;
                ShowMessage(Line + Line2);
              end
            else
              if not DirectoryExists(AbsDir) then
                begin
                  Line := ' Application directory "' + AbsDir +
                          '" does not exist.';
                  ShowMessage(Line);
                end
              else
                begin
                  Line := 'Project name is undefined.  Define it using Project menu.';
                  ShowMessage(Line);
                end;
          end;
      end
    else
      begin
        Line := 'Error (BuildMF2005InstructFiles): ' + ErrMsg;
        ShowMessage(Line);
      end;
    // Ensure that each pair (of model-output file and instruction file)
    // is in aProject.MOFiles or aProject.MOFilesPred.
    if OK then
      begin
        for K := 0 to KIF - 1 do
          begin
            Found := False;
            with aProject do
              begin
                case ModelUse of
                  muCalib:
                    begin
                      for I := 0 to MOFiles.Count - 1 do
  { TODO 2 -cObservations : Need logic for case where one file name of pair
  matches but other doesn't.  Ucode can't handle duplicate instructions for
  same observation. }
                        begin
                          if (SameText(MOFiles.Items[I].ModelFile, MOFileNames[K])) and
                             (SameText(MOFiles.Items[I].AppFile, InsFileNames[K])) then
                            begin
                              Found := True;
                            end;
                       end;
                      if not Found then
                        begin
                          I := MOFiles.Count;
                          MOFiles.Add;
                          MOFiles.Items[I].ModelFile := MOFileNames[K];
                          MOFiles.Items[I].AppFile := InsFileNames[K];
                        end;
                    end;
                  muPred:
                    begin
                      for I := 0 to MOFilesPred.Count - 1 do
  { TODO 2 -cPredictions : Need logic for case where one file name of pair
  matches but other doesn't.  Ucode can't handle duplicate instructions for
  same prediction. }
                        begin
                          if (SameText(MOFilesPred.Items[I].ModelFile, MOFileNames[K])) and
                             (SameText(MOFilesPred.Items[I].AppFile, InsFileNames[K])) then
                            begin
                              Found := True;
                            end;
                        end;
                      if not Found then
                        begin
                          I := MOFilesPred.Count;
                          MOFilesPred.Add;
                          MOFilesPred.Items[I].ModelFile := MOFileNames[K];
                          MOFilesPred.Items[I].AppFile := InsFileNames[K];
                        end;
                    end;
                end;
              end;
          end;
      end;
    TempResult := OK;
  finally
    result := TempResult;
    SetLength(IFileIndex,0);
    SetLength(NDepPerFile,0);
    SetLength(UnitNums,0);
    SetLength(InsFileNames,0);
    SetLength(MOFileNames,0);
    SetLength(DepOutFileNames,0);
    slNameFile.Free;
    for I := 0 to NobsPkg - 1 do
      begin
        slInsFiles[I].Free;
      end;
    SetLength(slInsFiles,0);
  end;
end;

//###################################################################

function BuildMF2005PvalTemplate(var aProject: TProject;
             var aMessage: string; ModelUse: TModelUse): boolean;
// Build a template file (for either Ucode or Pest) for constructing a
// MODFLOW-2005 PVAL file.  For successful execution, aProject must
// contain at least one parameter in ParamSet, and ModflowNameFile must
// be an existing MODFLOW-2005 name file.  If file ModflowNameFile contains
// a PVAL line, the file name on that line is used to name the template
// file.  If file ModflowNameFile does not contain a PVAL line, a PVAL line is
// added to the file.  aProject.MIFiles is checked or modified to ensure that
// the it includes a file pair containing the PVAL file and the corresponding
// template file.
var
  Found, OK: boolean;
  AbsPath1, AbsPath2, AbsPath3, AbsPath4, AbsAppDir, aDir, Ext, Line, Par: string;
  slNameFile, slPValTplFile: TStringList;
  NameFile, NameFileBase, PvalFile, PValTplFile: TFileName;
  I, J, LenField, LenStr, UnitNum: integer;
  Delimiter: Char;
  ParField: string;
begin
  Delimiter := '@';
  LenField := 24;
  aMessage := '';
  OK := True;
  // If there are no parameters, don't bother, return False
  if aProject.ParamSet.Count = 0 then
    begin
      aMessage := 'Project contains no parameters';
      OK := False;
    end;
  // Ensure that ModflowNameFile exists and is in directory ModelDirectory
  NameFile := aProject.AbsModflowNameFile(ModelUse);
  if FileExists(NameFile) then
    begin
      aDir := ExtractFileDir(NameFile);
      case ModelUse of
        muCalib: aProject.ModelDirectory := aDir; // setter converts to relative path;
        muPred: aProject.ModelDirectoryPred := aDir; // setter converts to relative path;
      end;
    end
  else
    begin
      if aProject.LocateModflowNameFile(ModelUse) then
        begin
          NameFile := aProject.AbsModflowNameFile(ModelUse);;
        end
      else
        begin
          OK := False;
          aMessage := 'Name file: "' + NameFile + '" not found';
        end;
    end;
  //
  if OK then
    begin
      NameFileBase := ExtractFileName(NameFile);
      NameFileBase := ChangeFileExt(NameFileBase,'');
      // Ensure that Name File contains an entry for the PVAL file, and
      // assign PValFile as that file name
      if GetNameFileEntry(NameFile, 'PVAL', UnitNum, PvalFile) then
        begin
          // Convert PvalFile to an absolute path.
          PvalFile := aProject.AbsModelDirectory(muCalib) + PathDelimiter + PvalFile;
        end
      else
        begin
          // Modify Name File to add a PVAL entry at the bottom of the file
          slNameFile := TStringList.Create; // Existing Name File
          try
            slNameFile.LoadFromFile(NameFile);
            I := 0;
            // Find an unused unit number for PVAL file
            while I < slNameFile.Count do
              begin
                Line := GetNextDataString(slNameFile, I);
                if Line <> '' then
                  begin
                    J := StrToInt(ParseByBlanks(Line,2));
                    if J >= UnitNum then UnitNum := J + 1;
                  end;
              end;
            // Add a PVAL entry to the name file
            PvalFile := aProject.AbsModelDirectory(ModelUse) + PathDelimiter + ConvertString(aProject.ProjName) + '.pval';
            Line := 'pval  ' + IntToStr(UnitNum) + '  ' + PvalFile;
            slNameFile.Add(Line);
            slNameFile.SaveToFile(NameFile);
          finally
            slNameFile.Free;
          end;
        end;
      // Template file will be saved in AppDirectory.
      case aProject.ActiveApp of
        aaUcode: Ext := 'jtf';
        aaPest: Ext := 'ptf';
        aaApp3: Ext := 'jtf';
        aaApp4: Ext := 'jtf';
      end;
{ It's no good if AbsAppDirectory = '\'.  It's needed to know where to save template file  }
      AbsAppDir := aProject.AbsAppDirectory(ModelUse);
      if AbsAppDir = PathDelimiter then
        begin
          OK := False;
          aMessage := 'Project needs to be saved';
        end;
    end;
  if OK then
    begin
      // Assign path name for PVAL template file if not already defined.
      PValTplFile := '';
      if IsNonBlank(PvalFile) then
        begin
          PValTplFile := aProject.GetTemplateFile(PValFile,ModelUse);
        end;
      if IsNonBlank(PValTplFile) then
        begin
          PValTplFile := AbsAppDir + PathDelimiter + PValTplFile;
        end
      else
        begin
          PValTplFile := AbsAppDir + PathDelimiter + NameFileBase + '_pval.' + Ext;
        end;
      // Build template file for MF2005 PVAL File and save it in the
      // aProject.AppDirectory.
      slPValTplFile := TStringList.Create; // PVAL file to be created
      try
        // Build template file
        // Construct initial lines
        Line := Ext + ' @';
        slPValTplFile.Add(Line);
        Line := '# MODFLOW-2005 PVAL file for project ' + ConvertString(aProject.ProjName);
        slPValTplFile.Add(Line);
        Line := IntToStr(aProject.ParamSet.Count);
        slPValTplFile.Add(Line);
        // Add a line for each parameter
        for I := 0 to aProject.ParamSet.Count - 1 do
          begin
            Par := ConvertString(aProject.ParamSet.Items[I].Name);
            ParField := Delimiter + Par;
            LenStr := Length(ParField);
            for J := LenStr+1 to LenField-1 do
              begin
                ParField := ParField + ' ';
              end;
            ParField := ParField + Delimiter;
            Line := Par + ' ' + ParField;
            slPValTplFile.Add(Line);
          end;
        slPValTplFile.SaveToFile(PValTplFile);
      finally
        slPValTplFile.Free;
      end;
      // Ensure that PVAL file (PValFile) and template file (PValTplFile)
      // are in aProject.MIFiles
      Found := False;
      with aProject do
        begin
          AbsPath2 := PathToAbsPath(AbsModelDirectory(ModelUse), PValFile);
          AbsPath4 := PathToAbsPath(ProjectDirectory, PvalTplFile);
          case ModelUse of
            muCalib:
              begin
                for I := 0 to MIFiles.Count - 1 do
                  begin
                    AbsPath1 := MIFiles.Items[I].AbsModelFile;
                    AbsPath3 := MIFiles.Items[I].AbsAppFile;
                    if (SameText(AbsPath1, AbsPath2)) and
                       (SameText(AbsPath3, AbsPath4)) then
                      begin
                        Found := True;
                      end;
                  end;
              end;
            muPred:
              begin
                for I := 0 to MIFilesPred.Count - 1 do
                  begin
                    AbsPath1 := MIFilesPred.Items[I].AbsModelFile;
                    AbsPath3 := MIFilesPred.Items[I].AbsAppFile;
                    if (SameText(AbsPath1, AbsPath2)) and
                       (SameText(AbsPath3, AbsPath4)) then
                      begin
                        Found := True;
                      end;
                  end;
              end;
          end;
          if not Found then
            begin
              case ModelUse of
                muCalib:
                  begin
                    I := MIFiles.Count;
                    MIFiles.Add;
                    MIFiles.Items[I].ModelFile := PValFile;
                    MIFiles.Items[I].AppFile := PValTplFile;
                  end;
                muPred:
                  begin
                    I := MIFilesPred.Count;
                    MIFilesPred.Add;
                    MIFilesPred.Items[I].ModelFile := PValFile;
                    MIFilesPred.Items[I].AppFile := PValTplFile;
                  end;
              end;
              ProjChanged := True;
            end;
        end;
    end;
  result := OK;
end;

//###################################################################

procedure GetMF2005FlowObs(const FName: TFileName; var dsTemp: TDepSet;
                           var PlotSymbol: integer; const GroupName: string12;
                           var IUOBSV: integer; var NumObs: integer);
// Read observations from Modflow-2005 flow-observation file and append to osTemp
var
  // Modflow variables
  NQ: integer;
  NQOB, NQCL: integer;
  FLWOBS: double;
  OBSNAM: string;
  //
  slObsFile: TStringList;
  ErrMsg, Line: string;
  Index, AbsNQCL: integer;
  I, J: Integer;
  OK: boolean;
begin
  OK := True;
  NQ := 0;
  NQOB := 0;
  NQCL := 0;
  NumObs := 0;
  slObsFile := TStringList.Create;
  try
    slObsFile.LoadFromFile(FName);
    Index := 0;
    Line := GetNextDataString(slObsFile, Index); // Read Item 1
    try
      NQ := StrToInt(ParseByBlanks(Line,1));
      IUOBSV := StrToInt(ParseByBlanks(Line,4));
    except
      ErrMsg := 'Error reading line: ' + Line;
      ShowMessage(ErrMsg);
      OK := False;
    end;
    if OK then
      begin
        Line := GetNextString(slObsFile,Index); // Read Item 2
        for I := 0 to NQ - 1 do
          begin
            Line := GetNextString(slObsFile,Index); // Read Item 3
            try
              NQOB := StrToInt(ParseByBlanks(Line,1));
              NQCL := StrToInt(ParseByBlanks(Line,2));
            except
              ErrMsg := 'Error reading line: ' + Line;
              ShowMessage(ErrMsg);
              OK := False;
            end;
            if OK then
              begin
                AbsNQCL := Abs(NQCL);
                for J := 0 to NQOB - 1 do
                  begin
                    if OK then
                      begin
                        Line := GetNextString(slObsFile,Index); // Read Item 4
                        OBSNAM := ParseByBlanks(Line,1);
                        if (Length(OBSNAM) > 1) and (OBSNAM[1] = '''')
                          and (OBSNAM[Length(OBSNAM)] = '''') then
                        begin
                          OBSNAM := Copy(OBSNAM, 2, Length(OBSNAM)-2);
                        end;
                        try
                          FLWOBS := StrToFloat(ParseByBlanks(Line,4));
                          // Add observation to observation set
                          dsTemp.Add;
                { TODO 3 -cObservations : Probably should define default values for Statistic
                and StatFlag for Flow observations? }
                          dsTemp.Items[dsTemp.Count - 1].DefineDep(ConvertString20(OBSNAM), GroupName,
                                                                   FLWOBS, PlotSymbol, dcObs);
                          NumObs := NumObs + 1;
                        except
                          ErrMsg := 'Error reading line: ' + Line;
                          ShowMessage(ErrMsg);
                          OK := False;
                        end;
                      end;
                  end;
                Index := Index + AbsNQCL;   // Skip over unneeded Item 5 records
              end;
          end;
        PlotSymbol := PlotSymbol + 1;
      end;
  finally
    slObsFile.Free;
  end;
end;

//###################################################################

procedure GetMF2005HeadObs(const FName: TFileName; var dsTemp: TDepSet;
                           var PlotSymbol: integer; var IUHOBSV: integer;
                           var HOBDRY: string; var NumObs: integer;
                           var JvProgressDialog: TJvProgressDialog);
// Read observations from Modflow-2005 HOB file and append to osTemp
var
  // Modflow variables
  IREFSP, ITT, LAY, MAXM, MOBS, NH: integer;
  HOBS: double;
  OBSNAM: string;
  MLAY: array of integer;
  PR: array of double;
  //
  slHOBFile: TStringList;
  Line: string;
  GroupObs, GPHeads, GPChanges: string12;
  Index, AbsIrefsp, AbsLay, PSHeadChange, PSobs: integer;
  J, Pos: integer;
  InitHead: double;
  Value: string;
begin
  GPHeads := 'Heads';
  GPChanges := 'Head_Changes';
  PSHeadChange := PlotSymbol;
  PSobs := PlotSymbol;
  InitHead := -88888.0;
  NumObs := 0;
  slHOBFile := TStringList.Create;
  try
  slHOBFile.LoadFromFile(FName);
  Index := 0;
  Line := GetNextDataString(slHOBFile, Index); // Read Item 1
  NH := StrToInt(ParseByBlanks(Line,1));
  MOBS := StrToInt(ParseByBlanks(Line,2));
  MAXM := StrToInt(ParseByBlanks(Line,3));
  IUHOBSV := StrToInt(ParseByBlanks(Line,4));
  HOBDRY := ParseByBlanks(Line,5);
  //
  if MOBS > 0 then
    begin
      SetLength(MLAY,MAXM);
      SetLength(PR,MAXM);
    end;
  Line := GetNextString(slHOBFile, Index); // Read Item 2
  JvProgressDialog.Max := NH;
  JvProgressDialog.Position := 0;
  JvProgressDialog.Text := 'Reading Head Observations';
  while NumObs < NH do
    begin
      Line := GetNextString(slHOBFile, Index); // Read Item 3
      OBSNAM := ParseByBlanks(Line,1);
      if (Length(OBSNAM) > 1) and (OBSNAM[1] = '''')
        and (OBSNAM[Length(OBSNAM)] = '''') then
      begin
        OBSNAM := Copy(OBSNAM, 2, Length(OBSNAM)-2);
      end;
      LAY := StrToInt(ParseByBlanks(Line,2));
      IREFSP := StrToInt(ParseByBlanks(Line,5));
      HOBS := StrToFloat(ParseByBlanks(Line,9));
      AbsLay := Abs(Lay);
      AbsIrefsp := Abs(IREFSP);
      if LAY < 0 then
        begin
          // Read Item 4 -- This won't work if the layer and proportion
          // are on different lines.
{ TODO 3 -cObservations : Add support for reading MLAY and PR in true free format
(for case where values are on multiple lines) }
          Line := GetNextString(slHOBFile, Index); // Read Item 4
          Pos := 1;
          for J := 0 to AbsLay - 1 do
            begin
              Value := ParseByBlanks(Line,Pos);
              while Value = '' do
              begin
                Line := GetNextString(slHOBFile, Index);
                Pos := 1;
                Value := ParseByBlanks(Line,Pos);
              end;
              MLAY[J] := StrToInt(Value);
              PR[J] := StrToFloat(ParseByBlanks(Line,Pos+1));
              Pos := Pos + 2;
            end;
        end;
      if IREFSP < 0 then
        begin
          Line := GetNextString(slHOBFile, Index); // Read Item 5
          ITT := StrToInt(ParseByBlanks(Line,1));
          for J := 0 to AbsIrefsp - 1 do
            begin
              Line := GetNextString(slHOBFile, Index); // Read Item 6
              OBSNAM := ParseByBlanks(Line,1);
              if (Length(OBSNAM) > 1) and (OBSNAM[1] = '''')
                and (OBSNAM[Length(OBSNAM)] = '''') then
              begin
                OBSNAM := Copy(OBSNAM, 2, Length(OBSNAM)-2);
              end;
              HOBS := StrToFloat(ParseByBlanks(Line,4));
              if J = 0 then
                begin
                  PSobs := PlotSymbol;
                  GroupObs := GpHeads;
                  InitHead := HOBS;
                end
              else
                begin
                  if ITT = 2 then
                    begin
                      if PSHeadChange = PlotSymbol then PSHeadChange := PlotSymbol + 1;
                      PSObs := PSHeadChange; // depends on ITT
                      GroupObs := GpChanges;
                      HOBS := HOBS - InitHead;
                    end;
                end;
              // Add observation to observation set
              dsTemp.Add;
{ TODO 3 -cObservations : Probably should define default values for Statistic
and StatFlag for Head and Head-change observations? }
              dsTemp.Items[dsTemp.Count - 1].DefineDep(ConvertString20(OBSNAM), GroupObs,
                                                       HOBS, PSobs, dcObs);
              NumObs := NumObs + 1;
            end;
        end
      else
        begin
          // Add observation to observation set
          dsTemp.Add;
{ TODO 3 -cObservations : Probably should define default values for Statistic
and StatFlag for Head observations? }
          dsTemp.Items[dsTemp.Count - 1].DefineDep(ConvertString20(OBSNAM), GpHeads,
                                                   HOBS, PlotSymbol, dcObs);
          NumObs := NumObs + 1;
        end;
      //
      JvProgressDialog.Position := NumObs;
    end;
  PlotSymbol := PSHeadChange + 1;
  finally
    slHOBFile.Free;
    JvProgressDialog.Hide;
  end;
end;

procedure GetMF2005HeadDep(const FName: TFileName; var dsTemp: TDepSet;
                           var PlotSymbol: integer; var IUHOBSV: integer;
                           var HOBDRY: string; var NumDep: integer;
                           const aDepCat: TDepCat);
// Read observations from Modflow-2005 HOB file and append to dsTemp.
var
  // Modflow variables
  NH, MOBS, LAY, IREFSP, ITT: integer;
  HOBS: double;
  OBSNAM: string;
  MLAY: array of integer;
  PR: array of double;
  //
  slHOBFile: TStringList;
  Line: string;
  GroupDep, GPHeads, GPChanges: string12;
  Index, AbsIrefsp, AbsLay, PSHeadChange, PSdep: integer;
  J, Pos: integer;
  InitHead: double;
begin
  GPHeads := 'Heads';
  GPChanges := 'Head_Changes';
  PSHeadChange := PlotSymbol;
  PSdep := PlotSymbol;
  InitHead := -88888.0;
  NumDep := 0;
  slHOBFile := TStringList.Create;
  try
  slHOBFile.LoadFromFile(FName);
  Index := 0;
  Line := GetNextDataString(slHOBFile, Index); // Read Item 1
  NH := StrToInt(ParseByBlanks(Line,1));
  MOBS := StrToInt(ParseByBlanks(Line,2));
  IUHOBSV := StrToInt(ParseByBlanks(Line,4));
  HOBDRY := ParseByBlanks(Line,5);
  //
  if MOBS > 0 then
    begin
      SetLength(MLAY,MOBS);
      SetLength(PR,MOBS);
    end;
  Line := GetNextString(slHOBFile, Index); // Read Item 2
  while NumDep < NH do
    begin
      Line := GetNextString(slHOBFile, Index); // Read Item 3
      OBSNAM := ParseByBlanks(Line,1);
      LAY := StrToInt(ParseByBlanks(Line,2));
      IREFSP := StrToInt(ParseByBlanks(Line,5));
      HOBS := StrToFloat(ParseByBlanks(Line,9));
      AbsLay := Abs(Lay);
      AbsIrefsp := Abs(IREFSP);
      if LAY < 0 then
        begin
          // Read Item 4 -- This won't work if Item 4 is on more than 1 line
{ TODO 3 -cDependents : Add support for reading MLAY and PR in true free format
(for case where values are on multiple lines) }
          Line := GetNextString(slHOBFile, Index); // Read Item 4
          Pos := 1;
          for J := 0 to AbsLay - 1 do
            begin
              MLAY[J] := StrToInt(ParseByBlanks(Line,Pos));
              PR[J] := StrToFloat(ParseByBlanks(Line,Pos+1));
              Pos := Pos + 2;
            end;
        end;
      if IREFSP < 0 then
        begin
          Line := GetNextString(slHOBFile, Index); // Read Item 5
          ITT := StrToInt(ParseByBlanks(Line,1));
          for J := 0 to AbsIrefsp - 1 do
            begin
              Line := GetNextString(slHOBFile, Index); // Read Item 6
              OBSNAM := ParseByBlanks(Line,1);
              HOBS := StrToFloat(ParseByBlanks(Line,4));
              if J = 0 then
                begin
                  PSdep := PlotSymbol;
                  GroupDep := GpHeads;
                  InitHead := HOBS;
                end
              else
                begin
                  if ITT = 2 then
                    begin
                      if PSHeadChange = PlotSymbol then PSHeadChange := PlotSymbol + 1;
                      PSdep := PSHeadChange; // depends on ITT
                      GroupDep := GpChanges;
                      HOBS := HOBS - InitHead;
                    end;
                end;
              // Add dependent to dependent set
              dsTemp.Add;
{ TODO 3 -cDependents : Probably should define default values for Statistic
and StatFlag for Head and Head-change observations? }
              dsTemp.Items[dsTemp.Count - 1].DefineDep(ConvertString20(OBSNAM), GroupDep,
                                                       HOBS, PSdep, aDepCat);
              NumDep := NumDep + 1;
            end;
        end
      else
        begin
          // Add dependent to dependent set
          dsTemp.Add;
{ TODO 3 -cDependents : Probably should define default values for Statistic
and StatFlag for Head observations? }
          dsTemp.Items[dsTemp.Count - 1].DefineDep(ConvertString20(OBSNAM), GpHeads,
                                                   HOBS, PlotSymbol, aDepCat);
          NumDep := NumDep + 1;
        end;
      //
    end;
  PlotSymbol := PSHeadChange + 1;
  finally
    slHOBFile.Free;
  end;
end;

//###################################################################

initialization

finalization

end.

