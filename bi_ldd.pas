//------------------------------------------------------------------------------
//
//  BrickInventory: A tool for managing your brick collection
//  Copyright (C) 2014-2024 by Jim Valavanis
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
// DESCRIPTION:
//   LDD Stuf
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit bi_ldd;

interface

uses
  SysUtils, Classes, bi_db;

const
  LDD_BRICK_WIDTH = 0.8;
  LDD_BRICK_HEIGHT = 0.96;
  LDD_PLATE_HEIGHT = 0.32;
  LDD_STUD_OVER_HEIGHT = 0.178;
  LDD_MAP_STARTX = -0.4;
  LDD_MAP_STARTZ = -0.4;

const
  LDD_DEF_STACK_HEIGHT = 30;

type
  lddmaterial_t = record
    MatID: integer;
    Red: integer;
    Green: integer;
    Blue: integer;
    Alpha: integer;
    MaterialType: string[16];
    MatDesc: string[64];
  end;
  lddmaterial_p = ^lddmaterial_t;
  lddmaterial_a = array[0..$FF] of lddmaterial_t;
  lddmaterial_pa = ^lddmaterial_a;

  lddprimitive_t = record
    Primitive: string[15];
    minX, minY, minZ: double;
    maxX, maxY, maxZ: double;
  end;
  lddprimitive_p = ^lddprimitive_t;
  lddprimitive_a = array[0..$FFFF] of lddprimitive_t;
  lddprimitive_pa = ^lddprimitive_a;

type
  TLDDPrimitiveInfo = class(TObject)
  protected
    fminX, fminY, fminZ: double;
    fmaxX, fmaxY, fmaxZ: double;
    // Actual Size
    function GetxSize: double;
    function GetySize: double;
    function GetzSize: double;
    // How to move to safely avoid collisions
    function GetdmapX: double;
    function GetdmapY: double;
    function GetdmapZ: double;
    // Map Units
    function GetMapX: integer;
    function GetMapY: integer;
    function GetMapZ: integer;
  public
    basename: string;
    constructor Create; virtual;
    property minX: double read fminX write fminX;
    property minY: double read fminY write fminY;
    property minZ: double read fminZ write fminZ;
    property maxX: double read fmaxX write fmaxX;
    property maxY: double read fmaxY write fmaxY;
    property maxZ: double read fmaxZ write fmaxZ;
    // Actual Size
    property xSize: double read GetxSize;
    property ySize: double read GetySize;
    property zSize: double read GetzSize;
    // How to move to safely avoid collisions
    property dmapX: double read GetdmapX;
    property dmapY: double read GetdmapY;
    property dmapZ: double read GetdmapZ;
    // Map Units
    property MapX: integer read GetMapX;
    property MapY: integer read GetMapY;
    property MapZ: integer read GetMapZ;
  end;

type
  TLDD = class(TObject)
  private
    // Container for primitives
    fPrimitivesContainer: TStringList;
    // Index for primitives
    // May contain the TLDDPrimitiveInfo object multiple times
    fPrimitives: TStringList;
    flddmaterials: lddmaterial_pa;
    fnmaterials: integer;
    ferrorlog: string;
    fstackheight: integer;
    function _ftoa(const f: double): string;
  protected
    function LDD_InitColors: boolean;
    function LDD_InitPrimitives: boolean;
    function PrimitiveBaseName(const part: string): string;
    function MapXToX(const x: integer): double;
    function MapYToY(const y: integer): double;
    function MapZToZ(const z: integer): double;
    procedure ClearLog;
    procedure Log(const s: string);
    procedure LogLn(const s: string);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Primitive(const part: string): TLDDPrimitiveInfo;
    function GenerateLXFML(const inv: TBrickInventory; const aname: string): string;
    property errorlog: string read ferrorlog;
    property stackheight: integer read fstackheight write fstackheight;
  end;

implementation

uses
  bi_globals, bi_utils, bi_delphi;

// -----------------------------------------------------------------------------

constructor TLDDPrimitiveInfo.Create;
begin
  basename := '';
  fminX := 0.0;
  fminY := 0.0;
  fminZ := 0.0;
  fmaxX := 0.0;
  fmaxY := 0.0;
  fmaxZ := 0.0;
  Inherited Create;
end;

function TLDDPrimitiveInfo.GetxSize: double;
begin
  Result := maxX - minX;
end;

function TLDDPrimitiveInfo.GetySize: double;
begin
  Result := maxY - minY;
end;

function TLDDPrimitiveInfo.GetzSize: double;
begin
  Result := maxZ - minZ;
end;

function TLDDPrimitiveInfo.GetdmapX: double;
var
  nstuds: integer;
begin
  nstuds := MapX;
  Result := (nstuds + 1) * LDD_BRICK_WIDTH;
end;

function TLDDPrimitiveInfo.GetdmapY: double;
var
  nstuds: integer;
begin
  nstuds := MapY;
  Result := (nstuds + 1) * LDD_PLATE_HEIGHT;
end;

function TLDDPrimitiveInfo.GetdmapZ: double;
var
  nstuds: integer;
begin
  nstuds := MapZ;
  Result := (nstuds + 1) * LDD_BRICK_WIDTH;
end;

function TLDDPrimitiveInfo.GetMapX: integer;
begin
  Result := Round(xSize / LDD_BRICK_WIDTH);
end;

function TLDDPrimitiveInfo.GetMapY: integer;
begin
  Result := Round(ySize / LDD_PLATE_HEIGHT);
end;

function TLDDPrimitiveInfo.GetMapZ: integer;
begin
  Result := Round(zSize / LDD_BRICK_WIDTH);
end;

// -----------------------------------------------------------------------------

function TLDD.LDD_InitColors: boolean;
var
  sl: TStringList;
  fn: string;
  i, j: integer;
  ln, sMatID, sRed, sGreen, sBlue, sAlpha, sMaterialType, sMatDesc: string;
  matp: lddmaterial_p;
  cp: colorinfo_p;
  rc, gc, bc: integer;
  rdiff, gdiff, bdiff: integer;
  cc: LongWord;
  dist: double;
  mindist: double;
  db_transparent, ldd_transparent: boolean;
begin
  Result := False;
  flddmaterials := nil;
  fnmaterials := 0;

  fn := basedefault + 'db\db_lddmaterials.txt';
  if not fexists(fn) then
    Exit;

  sl := TStringList.Create;
  sl.LoadFromFile(fn);
  if sl.Count > 1 then
    if sl.Strings[0] = 'MatID,Red,Green,Blue,Alpha,MaterialType,MatDesc' then
    begin
      Result := True;
      fnmaterials := sl.Count - 1;
      flddmaterials := mallocz(fnmaterials * SizeOf(lddmaterial_t));
      for i := 1 to sl.Count - 1 do
      begin
        ln := sl.Strings[i];
        splitstring(ln, sMatID, sRed, sGreen, sBlue, sAlpha, sMaterialType, sMatDesc, ',');
        matp := @flddmaterials[i - 1];
        matp.MatID := atoi(sMatID);
        matp.Red := atoi(sRed);
        matp.Green := atoi(sGreen);
        matp.Blue := atoi(sBlue);
        matp.Alpha := atoi(sAlpha);
        matp.MaterialType := sMaterialType;
        matp.MatDesc := sMatDesc;
      end;
      for i := -1 to LASTNORMALCOLORINDEX do
      begin
        cp := db.Colors(i);
        if (cp.id = i) or (i = -1) then
        begin
          cc := RGBInvert(cp.RGB);
          rc := cc and $FF;
          gc := (cc shr 8) and $FF;
          bc := (cc shr 16) and $FF;
          db_transparent := Pos('Trans', cp.name) > 0;
          mindist := $ffffffff;
          cp.lddColor := 1;
          for j := 0 to fnmaterials - 1 do
          begin
            matp := @flddmaterials[j];
            ldd_transparent := matp.Alpha < 255;
            if ldd_transparent = db_transparent then
            begin
              rdiff := matp.Red - rc;
              gdiff := matp.Green - gc;
              bdiff := matp.Blue - bc;
              dist := 0.30 * rdiff * rdiff + 0.59 * gdiff * gdiff + 0.11 * bdiff * bdiff;
              if dist < mindist then
              begin
                cp.lddColor := matp.MatID;
                mindist := dist;
              end;
            end;
          end;
        end;
      end;
    end;

  sl.Free;
end;

function TLDD.LDD_InitPrimitives: boolean;
var
  sl: TStringList;
  aliaslist: TStringList;
  fn: string;
  i, j: integer;
  ln, sPrimitive, sminX, sminY, sminZ, smaxX, smaxY, smaxZ, saliases: string;
  linfo: TLDDPrimitiveInfo;
begin
  Result := False;

  fPrimitivesContainer := TStringList.Create;
  fPrimitivesContainer.Duplicates := dupAccept;
  fPrimitivesContainer.CaseSensitive := False;
  fPrimitives := TStringList.Create;
  fPrimitives.Duplicates := dupIgnore;
  fPrimitives.CaseSensitive := False;

  fn := basedefault + 'db\db_lddprimitives.txt';
  if not fexists(fn) then
    Exit;

  sl := TStringList.Create;
  sl.LoadFromFile(fn);
  if sl.Count > 1 then
    if sl.Strings[0] = 'Primitive,minX,minY,minZ,maxX,maxY,maxZ,aliases' then
    begin
      Result := True;
      for i := 1 to sl.Count - 1 do
      begin
        ln := sl.Strings[i];
        splitstring(ln, sPrimitive, sminX, sminY, sminZ, smaxX, smaxY, smaxZ, saliases, ',');

        linfo := TLDDPrimitiveInfo.Create;
        linfo.basename := sPrimitive;
        linfo.minX := atof(sminX);
        linfo.minY := atof(sminY);
        linfo.minZ := atof(sminZ);
        linfo.maxX := atof(smaxX);
        linfo.maxY := atof(smaxY);
        linfo.maxZ := atof(smaxZ);

        fPrimitivesContainer.AddObject(sPrimitive, linfo);

        if saliases <> '' then
        begin
          aliaslist := string2stringlist(saliases, ';');
          if aliaslist.IndexOf(sPrimitive) < 0 then
            aliaslist.Add(sPrimitive);
        end
        else
        begin
          aliaslist := TStringList.Create;
          aliaslist.Add(sPrimitive);
        end;

        for j := 0 to aliaslist.Count - 1 do
          fPrimitives.AddObject(aliaslist[j], linfo);

        aliaslist.Free;
      end;
    end;

  fPrimitives.Sorted := True;

  sl.Free;
end;

constructor TLDD.Create;
begin
  Inherited Create;
  stackheight := LDD_DEF_STACK_HEIGHT;
  LDD_InitColors;
  LDD_InitPrimitives;
end;

destructor TLDD.Destroy;
begin
  if fnmaterials > 0 then
    memfree(pointer(flddmaterials), fnmaterials * SizeOf(lddmaterial_t));

  fPrimitives.Free;

  FreeList(fPrimitivesContainer); // Also delete TLDDPrimitiveInfo objects

  Inherited Destroy;
end;

function TLDD.PrimitiveBaseName(const part: string): string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to Length(part) do
    if part[i] in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'] then
      Result := Result + part[i]
    else
      Break;
  if Result = '' then
    Result := part;
end;

type
  primitivelookup_t = record
    part: string[15];
    prim: string[15];
  end;

const
  NUM_EXTRAPRIMITIVES = 5;
  EXTRAPRIMITIVES: array[0..NUM_EXTRAPRIMITIVES - 1] of primitivelookup_t = (
    (part: '38648c01'; prim: '10830'),
    (part: '80440'; prim: '24312'),
    (part: '90370'; prim: '90393'),
    (part: '66897'; prim: '21445'),
    (part: '1941'; prim: '20612')
  );

function TLDD.Primitive(const part: string): TLDDPrimitiveInfo;
var
  i, idx: integer;
  related: TStringList;
begin
  idx := fPrimitives.IndexOf(PrimitiveBaseName(part));
  if idx < 0 then
  begin
    related := db.GetRelatedPieces(part);
    for i := 0 to related.Count - 1 do
    begin
      idx := fPrimitives.IndexOf(PrimitiveBaseName(related[i]));
      if idx >= 0 then
        Break;
    end;
    related.Free;
  end;
  if idx < 0 then
  begin
    for i := 0 to NUM_EXTRAPRIMITIVES - 1 do
      if part = EXTRAPRIMITIVES[i].part then
      begin
        idx := fPrimitives.IndexOf(EXTRAPRIMITIVES[i].prim);
        if idx >= 0 then
          Break;
      end;
  end;
  if idx >= 0 then
    Result := fPrimitives.Objects[idx] as TLDDPrimitiveInfo
  else
    Result := nil;
end;

function TLDD._ftoa(const f: double): string;
begin
  Result := Format('%2.15n', [f]);
end;

function TLDD.MapXToX(const x: integer): double;
begin
  Result := x * LDD_BRICK_WIDTH;
end;

function TLDD.MapYToY(const y: integer): double;
begin
  Result := y * LDD_PLATE_HEIGHT;
end;

function TLDD.MapZToZ(const z: integer): double;
begin
  Result := z * LDD_BRICK_WIDTH;
end;

procedure TLDD.ClearLog;
begin
  ferrorlog := '';
end;

procedure TLDD.Log(const s: string);
begin
  ferrorlog := ferrorlog + s;
end;

procedure TLDD.LogLn(const s: string);
begin
  Log(s + #13#10);
end;

const
  LDD_MAP_SIZE_X = 128;
  LDD_MAP_SIZE_Z = 128;
  LDD_MAP_SIZE_ADD = 64;

type
  lddmap_t = array[0..LDD_MAP_SIZE_X + LDD_MAP_SIZE_ADD - 1, 0..LDD_MAP_SIZE_z + LDD_MAP_SIZE_ADD - 1] of integer;
  lddmap_p = ^lddmap_t;

function TLDD.GenerateLXFML(const inv: TBrickInventory; const aname: string): string;
var
  i: integer;
  ret: string;
  tmpinv: TBrickInventory;
  mapp: lddmap_p;
  aa: integer;

  procedure _write(const s: string);
  begin
    ret := ret + s;
  end;

  procedure _writeln(const s: string);
  begin
    _write(s + #13#10);
  end;

  procedure _write_brick(const bp: brickpool_p);
  var
    ii, ix, iz: integer;
    istack: integer;
    prim: TLDDPrimitiveInfo;
    cp: colorinfo_p;
    saa: string;
    mapbestx, mapbesty, mapbestz, tmpmaxy: integer;
    xsize, ysize, zsize: integer;
    ixsize, izsize: integer;
    mapsizex, mapsizez: integer;
  begin
    prim := Primitive(bp.part);
    if prim = nil then
    begin
      LogLn('Can not match LDD primitive ' + bp.part);
      Exit;
    end;

    cp := db.Colors(bp.color);

    xsize := prim.MapX;
    ysize := prim.MapY;
    zsize := prim.MapZ;

    if xsize < LDD_MAP_SIZE_ADD then
      mapsizex := LDD_MAP_SIZE_X
    else
      mapsizex := LDD_MAP_SIZE_X + LDD_MAP_SIZE_ADD - xsize - 1;
    if zsize < LDD_MAP_SIZE_ADD then
      mapsizez := LDD_MAP_SIZE_Z
    else
      mapsizez := LDD_MAP_SIZE_Z + LDD_MAP_SIZE_ADD - zsize - 1;

    ii := 0;
    while ii < bp.num do
    begin
      mapbestx := 0;
      mapbesty := MAXINT;
      mapbestz := 0;

      // Scan all map to find where to place the part
      for ix := 0 to mapsizex - 1 do
      begin
        for iz := 0 to mapsizez - 1 do
        begin
          tmpmaxy := 0;
          // Scan portion of map to see at which height the new part can fit
          for ixsize := 0 to xsize do
            for izsize := 0 to zsize do
            // Find the highest point of the map portion
              if mapp[ix + ixsize, iz + izsize] > tmpmaxy then
                tmpmaxy := mapp[ix + ixsize, iz + izsize];
          if tmpmaxy < mapbesty then
          begin
            mapbestx := ix;
            mapbesty := tmpmaxy;
            mapbestz := iz;
          end;
          if mapbesty = 0 then
            Break; // Zero is the bottom, can not find better position
        end;
        if mapbesty = 0 then
          Break;
      end;

      istack := 0;
      while istack < stackheight do
      begin
        for ix := mapbestx to mapbestx + xsize do
          for iz := mapbestz to mapbestz + zsize do
            mapp[ix, iz] := mapbesty + ysize + 1;

        saa := itoa(aa);
        inc(aa);

        _writeln('  <Brick refID="' + saa + '" designID="' + prim.basename + '">');
        _writeln('    <Part refID="' + saa + '" designID="' + prim.basename + '" materials="' + itoa(cp.lddColor) + ',0,0,0" decoration="0">');
        _writeln('      <Bone refID="' + saa + '" transformation="1,0,0,0,1,0,0,0,1,' +
          _ftoa(MapXToX(mapbestx) - prim.minX) + ',' +
          _ftoa(MapYToY(mapbesty) - prim.minY) + ',' +
          _ftoa(MapZToZ(mapbestz) - prim.minZ) + '">'
        );
        _writeln('      </Bone>');
        _writeln('    </Part>');
        _writeln('  </Brick>');

        inc(ii);
        if ii = bp.num then
          break;

        mapbesty := mapbesty + ysize + 1;
        istack := istack + ysize + 1;
      end;
    end;
  end;

begin
  ClearLog;

  ret := '';
  _writeln('<?xml version="1.0" encoding="UTF-8" standalone="no" ?>');
  _writeln('<LXFML versionMajor="5" versionMinor="0" name="' + aname + '">');
  _writeln('  <Meta>');
  _writeln('    <Application name="LEGO Digital Designer" versionMajor="4" versionMinor="3"/>');
  _writeln('    <Brand name="LDDExtended"/>');
  _writeln('    <BrickSet version="2670"/>');
  _writeln('  </Meta>');
  _writeln('  <Cameras>');
  _writeln('    <Camera refID="0" fieldOfView="80" distance="250.0" transformation="0,0,1,0.4,0.9,0,-0.9,0.4,0,-220.0,90.0,50.0"/>');
  _writeln('  </Cameras>');
  _writeln('  <Bricks cameraRef="0">');

  tmpinv := inv.Clone;
  tmpinv.DismandalAllSets;
  tmpinv.SortPiecesByPartNumber;

  mapp := mallocz(SizeOf(lddmap_t));

  aa := 0;

  for i := 0 to tmpinv.numlooseparts - 1 do
    _write_brick(@tmpinv.looseparts[i]);

  memfree(pointer(mapp), SizeOf(lddmap_t));

  tmpinv.Free;

  _writeln('  </Bricks>');
  _writeln('  <RigidSystems>');
  _writeln('    <RigidSystem>');
  _writeln('    </RigidSystem>');
  _writeln('  </RigidSystems>');
  _writeln('  <GroupSystems>');
  _writeln('    <GroupSystem>');
  _writeln('    </GroupSystem>');
  _writeln('  </GroupSystems>');
  _writeln('  <BuildingInstructions>');
  _writeln('  </BuildingInstructions>');
  _writeln('</LXFML>');

  Result := ret;
end;

end.
