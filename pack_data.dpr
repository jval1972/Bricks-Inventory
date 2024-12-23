program zip_test;

{$APPTYPE CONSOLE}

uses
  FastMM4 in 'FastMM4.pas',
  FastMM4Messages in 'FastMM4Messages.pas',
  SysUtils,
  Classes,
  bi_binary in 'bi_binary.pas',
  PNGZLIB1 in 'PNGZLIB1.pas',
  bi_delphi in 'bi_delphi.pas',
  bi_tmp in 'bi_tmp.pas',
  bi_wadfile in 'bi_wadfile.pas';

var
  b: TBinary;
  quiet: boolean;
  basedefault: string;
  i, j: integer;
  lst: TDStringList;
  prefix: string;
  mocs: TStringList;
  ids: TStringList;
  rc: TStringList;
  wr: TWadWriter;

const
  NUMFILES = 36;
  PATH_BEGIN = 23;

var
  files: array[0..NUMFILES - 1] of string[64] = (
    'E:\BrickInventory\bin\db\',
    'E:\BrickInventory\bin\cache\cache.db',
    'E:\BrickInventory\bin\db\db_basebrick.txt',
    'E:\BrickInventory\bin\db\db_books.txt',
    'E:\BrickInventory\bin\db\db_boxes.txt',
    'E:\BrickInventory\bin\db\db_catalogs.txt',
    'E:\BrickInventory\bin\db\db_categories.txt',
    'E:\BrickInventory\bin\db\db_codes.txt',
    'E:\BrickInventory\bin\db\db_colors.txt',
    'E:\BrickInventory\bin\db\db_crawlerlinks.txt',
    'E:\BrickInventory\bin\db\db_cross_stats.txt',
    'E:\BrickInventory\bin\db\db_currency.txt',
    'E:\BrickInventory\bin\db\db_currencyconvert.txt',
    'E:\BrickInventory\bin\db\db_gears.txt',
    'E:\BrickInventory\bin\db\db_instructions.txt',
    'E:\BrickInventory\bin\db\db_knownpieces.txt',
    'E:\BrickInventory\bin\db\db_newnames.txt',
    'E:\BrickInventory\bin\db\db_parttypes.txt',
    'E:\BrickInventory\bin\db\db_pieces.txt',
    'E:\BrickInventory\bin\db\db_pieces_alias.txt',
    'E:\BrickInventory\bin\db\db_pieces_bl.txt',
    'E:\BrickInventory\bin\db\db_pieces_categories.txt',
    'E:\BrickInventory\bin\db\db_pieces_extra.txt',
    'E:\BrickInventory\bin\db\db_pieces_inventories.txt',
    'E:\BrickInventory\bin\db\db_pieces_weight.txt',
    'E:\BrickInventory\bin\db\db_pieces_years.txt',
    'E:\BrickInventory\bin\db\db_relationships.txt',
    'E:\BrickInventory\bin\db\db_sets.txt',
    'E:\BrickInventory\bin\db\db_set_assets.txt',
    'E:\BrickInventory\bin\db\db_set_pieces.txt',
    'E:\BrickInventory\bin\db\parts.db',
    'E:\BrickInventory\bin\db\sets1.db',
    'E:\BrickInventory\bin\db\sets2.db',
    'E:\BrickInventory\bin\db\sets3.db',
    'E:\BrickInventory\bin\db\parts\*.txt',
    'E:\BrickInventory\bin\db\sets\*.txt'
  );

function M_CheckParm(const parm: string): integer;
var
  i: integer;
begin
  for i := 1 to ParamCount do
    if ParamStr(i) = parm then
    begin
      Result := i;
      Exit;
    end;
  Result := 0;
end;

procedure verboseln(const s: string);
begin
  if quiet then
    Exit;

  writeln(s);
end;

procedure LoadMOCs;
var
  i: integer;
begin
  mocs.LoadFromFile(files[0] + '\db_mocs.txt');
  mocs.Delete(0);
  for i := mocs.Count - 1 downto 1 do
    mocs[i] := firstword(mocs[i], ',');
  for i := 1 to 99 do
    mocs.Add('fig' + IntToStrzFill(3, i));

  mocs.Add('mickeymouse01');
  mocs.Add('mosaic_plates_sman_128x128');
  mocs.Add('mosaic_kazantz2_96x224');
  mocs.Add('mosaic_donald01_48x48');
  mocs.Add('mosaic_agalia_96x64');
  mocs.Add('moneybin');
  mocs.Add('LUGBULK-2015');
  mocs.Add('LUGBULK-2016');
  mocs.Add('LUGBULK-2017');
  mocs.Add('LUGBULK-2018');
  mocs.Add('LUGBULK-2019');
  mocs.Add('LUGBULK-2020');
  mocs.Add('LUGBULK-2021');
  mocs.Add('LUGBULK-2021');
  mocs.Add('lugbulk2017');

  mocs.Sorted := True;
end;

function CheckMoc(const fin: string): string;
var
  sl: TStringList;
  check: string;
  i: integer;
begin
  Result := fin;

  sl := TStringList.Create;

  check := strlower(fname(fin));
  if (check = 'db_pieces_bl.txt') or (check = 'db_pieces_categories.txt') then
  begin
    sl.LoadFromFile(fin);
    for i := sl.Count - 1 downto 1 do
      if mocs.IndexOf(secondword(sl[i], ',')) >= 0 then
        sl.Delete(i);
    Result := I_NewTempFile(check);
    sl.SaveToFile(Result);
  end
  else if (check = 'db_knownpieces.txt') or (check = 'db_parttypes.txt') or
    (check = 'db_pieces.txt') or (check = 'db_pieces_extra.txt') or
    (check = 'db_pieces_years.txt') then
  begin
    sl.LoadFromFile(fin);
    for i := sl.Count - 1 downto 1 do
      if mocs.IndexOf(firstword(sl[i], ',')) >= 0 then
        sl.Delete(i);
    Result := I_NewTempFile(check);
    sl.SaveToFile(Result);
  end;
  sl.Free;
end;

procedure DoZip(const fin, finname, fout: string);
var
  i: integer;
  id: string;

  function fix_fout: string;
  var
    ii: integer;
  begin
    Result := fout;
    for ii := 1 to length(Result) do
      if Result[ii] = '-' then
        Result[ii] := '_';
  end;

begin
  verboseln('Loading ' + fname(fin));
  b.LoadFromFile(fin, False);
  verboseln('Saving ' + fname(fout));
  b.SaveToFile(fix_fout, True);

  id := '';
  for i := PATH_BEGIN to Length(finname) do
    id := id + finname[i];
  rc.Add(Format('%d DATABASE %s', [ids.Count, fix_fout]));
  ids.Add(id);

  wr.AddFile(id, fix_fout);
end;

begin
  { TODO -oUser -cConsole Main : Insert code here }
  I_InitTempFiles;

  b := TBinary.Create;
  b.CompressionLevel := zcDefault;

  quiet := (M_CheckParm('-q') > 0) or (M_CheckParm('-quiet') > 0);
  basedefault := ExtractFilePath(ParamStr(0)) + 'data\';
  MakeDir(basedefault);
  MakeDir(basedefault + 'Sets');
  MakeDir(basedefault + 'Parts');

  // Current drive
  for i := 0 to NUMFILES - 1 do
    files[i][1] := basedefault[1];

  rc := TStringList.Create;

  ids := TStringList.Create;
  ids.Add('Files');

  mocs := TStringList.Create;
  LoadMOCs;

  wr := TWadWriter.Create;

  // Index 0 if the db path
  for i := 1 to NUMFILES - 1 do
  begin
    if CharPos('*', files[i]) < 1 then
    begin
      DoZip(
        CheckMoc(files[i]),
        files[i],
        basedefault + fname(files[i]) + '.z');
    end
    else
    begin
      lst := findfiles(files[i]);
      if Pos('\SETS\', strupper(files[i])) > 0 then
        prefix := 'Sets'
      else
        prefix := 'Parts';
      for j := lst.Count - 1 downto 0 do
      begin
        if (mocs.IndexOf(lst[j]) < 0) and (CharPos(' ', lst[j]) = 0) then
          DoZip(
            files[0] + prefix + '\' + lst[j],
            files[0] + prefix + '\' + lst[j],
            basedefault + prefix + '\' + fname(lst[j]) + '.z'
          )
        else
          lst.Delete(j);
      end;
      lst.SaveToFile(basedefault + prefix + '.txt');
      lst.Free;
    end;
  end;

  mocs.Free;

  ids.SaveToFile(basedefault + 'database.txt');
  ids.Free;

  verboseln('Saving wad file');
  wr.SaveToFile(basedefault + 'database.dat');
  wr.Free;

  rc.Add('0 DATABASE ' + basedefault + 'database.txt');
  rc.Clear;
  rc.Add('1 DATABASE ' + basedefault + 'database.dat');
  rc.SaveToFile('database.rc');
  rc.Free;

  b.Free;

  I_ShutDownTempFiles;

  verboseln('All done');
  readln;
end.

