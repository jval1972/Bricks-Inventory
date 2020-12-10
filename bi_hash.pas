unit bi_hash;

interface

uses
  SysUtils, Classes, bi_delphi;

const
  HASHSIZE = 8192 * 4;

type
  THashTable = class(TObject)
  private
    positions: array[0..HASHSIZE - 1] of TDNumberList;
    fList: TStringList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AssignStringList(const s: TStringList);
    function GetPos(const value: string): integer;
    property List: TStringList read fList;
  end;

implementation

function Hash(const name: string): integer;
var
  b: Byte;
  i: integer;
begin
  if Length(name) = 0 then
  begin
    Result := 0;
    exit;
  end;

  b := Ord(name[1]);

  result := 5381 * 33 + b;

  for i := 2 to Length(name) do
  begin
    b := Ord(name[i]);
    result := result * 33 + b;
  end;

  result := result and (HASHSIZE - 1);
end;

constructor THashTable.Create;
begin
  inherited;
  flist := nil;
  FillChar(positions, SizeOf(positions), 0);
end;

destructor THashTable.Destroy;
var
  i: integer;
begin
  for i := 0 to HASHSIZE - 1 do
    if positions[i] <> nil then
      positions[i].Free;
  inherited;
end;

procedure THashTable.AssignStringList(const s: TStringList);
var
  i: integer;
  h: integer;
begin
  flist := s;
  for i := 0 to flist.Count - 1 do
  begin
    h := Hash(flist.Strings[i]);
    if positions[h] = nil then
      positions[h] := TDNumberList.Create;
    positions[h].Add(i);
  end;
end;

function THashTable.GetPos(const value: string): integer;
var
  h: integer;
  i: integer;
  n: integer;
begin
  if flist = nil then
  begin
    result := -1;
    exit;
  end;

  if flist.Count = 0 then
  begin
    result := -1;
    exit;
  end;

  h := Hash(value);
  if positions[h] = nil then
  begin
    result := fList.IndexOf(value);
    exit;
  end;

  for i := 0 to positions[h].Count - 1 do
  begin
    n := positions[h].Numbers[i];
    if (n > -1) and (n < fList.Count) then
      if flist.Strings[n] = value then
      begin
        result := n;
        exit;
      end;
  end;

  result := fList.IndexOf(value);
end;

end.
