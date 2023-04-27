//------------------------------------------------------------------------------
//
//  BrickInventory: A tool for managing your brick collection
//  Copyright (C) 2014-2023 by Jim Valavanis
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
//    Quantize image
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------


unit bi_quantize;

interface

uses
  Windows,
  SysUtils,
  Graphics;

procedure ReduceTo8Bit(var bmp: TBitmap);

implementation

uses
  bi_delphi, bi_system, bi_multithread;

// Color quantization algorythm from https://rosettacode.org
type
  oct_node_p = ^oct_node_t;
  oct_node_t = record
    /// sum of all colors represented by this node. 64 bit in case of HUGE image */
    r, g, b: int64;
    count, heap_idx: integer;
    kids: array[0..7] of oct_node_p;
    parent: oct_node_p;
    n_kids, kid_idx, flags, depth: byte;
  end;
  oct_node_a = array[0..2047] of oct_node_t;
  oct_node_pa = ^oct_node_a;
  oct_node_ap = array[0..2047] of oct_node_p;
  oct_node_pp = ^oct_node_ap;

type
  node_heap_p = ^node_heap_t;
  node_heap_t = record
	  alloc, n: integer;
	  buf: oct_node_pp;
  end;

// cmp function that decides the ordering in the heap.  This is how we determine
// which octree node to fold next, the heart of the algorithm.
function cmp_node(const a, b: oct_node_p): integer;
var
  ac, bc: integer;
begin
  if a.n_kids < b.n_kids then
  begin
    Result := -1;
    Exit;
  end;
  if a.n_kids > b.n_kids then
  begin
    Result := 1;
    Exit;
  end;

  ac := a.count * (1 + a.kid_idx) shr a.depth;
  bc := b.count * (1 + b.kid_idx) shr b.depth;
  if ac < bc then
    Result := -1 else
  if ac > bc then
    Result := 1
  else
    Result := 0;
end;

procedure down_heap(h: node_heap_p; p: oct_node_p);
var
  n, m: integer;
begin
	n := p.heap_idx;
	while true do
  begin
		m := n * 2;
		if m >= h.n then
      break;
		if (m + 1 < h.n) and (cmp_node(h.buf[m], h.buf[m + 1]) > 0) then
      inc(m);

		if cmp_node(p, h.buf[m]) <= 0 then
      Break;

		h.buf[n] := h.buf[m];
		h.buf[n].heap_idx := n;
		n := m;
	end;
	h.buf[n] := p;
	p.heap_idx := n;
end;

procedure up_heap(h: node_heap_p; p: oct_node_p);
var
  n: integer;
  prev: oct_node_p;
begin
	n := p.heap_idx;

	while n > 1 do
  begin
		prev := h.buf[n div 2];
		if cmp_node(p, prev) >= 0 then
      Break;

		h.buf[n] := prev;
		prev.heap_idx := n;
		n := n div 2;
	end;
	h.buf[n] := p;
	p.heap_idx := n;
end;

const
  ON_INHEAP = 1;

procedure heap_add(h: node_heap_p; p: oct_node_p);
begin
	if p.flags and ON_INHEAP <> 0 then
  begin
		down_heap(h, p);
		up_heap(h, p);
		Exit;
	end;

	p.flags := p.flags or ON_INHEAP;
	if h.n = 0 then
    h.n := 1;
	if h.n >= h.alloc then
  begin
		while h.n >= h.alloc do
      h.alloc := h.alloc + 1024;
    ReallocMem(h.buf, SizeOf(oct_node_p) * h.alloc);
	end;

	p.heap_idx := h.n;
	h.buf[h.n] := p;
  inc(h.n);
	up_heap(h, p);
end;

function pop_heap(h: node_heap_p): oct_node_p;
begin
	if h.n <= 1 then
  begin
    Result := nil;
    Exit;
   end;

	Result := h.buf[1];
  dec(h.n);
	h.buf[1] := h.buf[h.n];

	h.buf[h.n] := nil;

	h.buf[1].heap_idx := 1;
	down_heap(h, h.buf[1]);
end;

var
  pool: oct_node_p = nil;
  nodeslen: integer = 0;

function node_new(const idx, depth: byte; p: oct_node_p): oct_node_p;
var
  x: oct_node_p;
begin
	if nodeslen <= 1 then
  begin
		GetMem(x, SizeOf(oct_node_t) * 2048);
    ZeroMemory(x, SizeOf(oct_node_t) * 2048);
		x.parent := pool;
		pool := x;
		nodeslen := 2048;
	end;

  dec(nodeslen);
  x := @(oct_node_pa(pool)[nodeslen]);
	x.kid_idx := idx;
	x.depth := depth;
	x.parent := p;
	if p <> nil then inc(p.n_kids);
	Result := x;
end;

function bitvalue(const x: integer): byte;
begin
  if x <> 0 then
    Result := 1
  else
    Result := 0;
end;

// adding a color triple to octree
function node_insert(root: oct_node_p; const pix: LongWord): oct_node_p;
const
  // 8: number of significant bits used for tree.  It's probably good enough
  // for most images to use a value of 5.  This affects how many nodes eventually
  // end up in the tree and heap, thus smaller values helps with both speed
  // and memory.
  OCT_DEPTH = 5;
var
  i, bit: byte;
  depth: integer;
  r, g, b: byte;
begin
  r := GetRValue(pix);
  g := GetGValue(pix);
  b := GetBValue(pix);
  bit := 1 shl 7;
  for depth := 0 to OCT_DEPTH - 1 do
  begin
    i := (1 - bitvalue(g and bit)) * 4 + (1 - bitvalue(r and bit)) * 2 + (1 - bitvalue(b and bit));
    if root.kids[i] = nil then
      root.kids[i] := node_new(i, depth, root);

    root := root.kids[i];
    bit := bit shr 1;
  end;

  root.r := root.r + r;
  root.g := root.g + g;
  root.b := root.b + b;
  inc(root.count);
  Result := root;
end;

// remove a node in octree and add its count and colors to parent node.
function node_fold(p: oct_node_p): oct_node_p;
var
  q: oct_node_p;
begin
  if p.n_kids > 0 then
  begin
    Result := nil;
    Exit;
  end;
  q := p.parent;
  q.count := q.count + p.count;

  q.r := q.r + p.r;
  q.g := q.g + p.g;
  q.b := q.b + p.b;
  dec(q.n_kids);
  q.kids[p.kid_idx] := nil;
  Result := q;
end;

// traverse the octree just like construction, but this time we replace the pixel
//   color with color stored in the tree node */
function get_color_quantized(root: oct_node_p; const c: LongWord; const allowzero: boolean): LongWord;
var
  i, bit: byte;
  r, g, b: byte;
begin
  r := GetRValue(c);
  g := GetGValue(c);
  b := GetBValue(c);
  bit := 1 shl 7;
  while bit <> 0 do
  begin
    i := (1 - bitvalue(g and bit)) * 4 + (1 - bitvalue(r and bit)) * 2 + (1 - bitvalue(b and bit));
    if root.kids[i] = nil then
      Break;
    root := root.kids[i];
    bit := bit shr 1;
  end;

  r := root.r;
  g := root.g;
  b := root.b;
  Result := RGB(r, g, b);
  if not allowzero then
    if Result = 0 then
      Result := $1;
end;

procedure node_free;
var
	p: oct_node_p;
begin
	while pool <> nil do
  begin
		p := pool.parent;
		FreeMem(pool);
		pool := p;
	end;
end;

type
  PLongWordArray = ^TLongWordArray;
  TLongWordArray = array[0..$FFFF] of LongWord;

procedure color_quantize_image(im: PLongWordArray; const imsize: integer;
  const n_colors: integer; const n: TDNumberList; const allowzero: boolean);
var
  i: integer;
  heap: node_heap_t;
  got, root: oct_node_p;
  dd: double;
begin
  heap.alloc := 0;
  heap.n := 0;
  heap.buf := nil;

  root := node_new(0, 0, nil);
  for i := 0 to imsize - 1 do
    heap_add(@heap, node_insert(root, im[i]));

  while heap.n > n_colors + 1 do
    heap_add(@heap, node_fold(pop_heap(@heap)));

  for i := 1 to heap.n - 1 do
  begin
    got := heap.buf[i];
    dd := got.count;
    got.r := Round(got.r / dd);
    got.g := Round(got.g / dd);
    got.b := Round(got.b / dd);
    if got.r < 0 then got.r := 0 else if got.r > 255 then got.r := 255;
    if got.g < 0 then got.g := 0 else if got.g > 255 then got.g := 255;
    if got.b < 0 then got.b := 0 else if got.b > 255 then got.b := 255;
    n.Add(RGB(got.r, got.g, got.b));
  end;

  for i := 0 to imsize - 1 do
    im[i] := get_color_quantized(root, im[i], allowzero);

  node_free;
  FreeMem(heap.buf);
end;

type
  tagLOGPALETTE255 = packed record
    palVersion: Word;
    palNumEntries: Word;
    palPalEntry: array[0..255] of LongWord;
  end;

function searchsorted(const n: TDNumberList; const num: Integer): integer;
var
  First: Integer;
  Last: Integer;
  Pivot: Integer;
  Found: Boolean;
begin
  First := 0;
  Last := n.Count - 1;
  Found := False; //Initializes the Found flag (Not found yet)
  Result := -1; //Initializes the Result

  //If First > Last then the searched item doesn't exist
  //If the item is found the loop will stop
  while (First <= Last) and (not Found) do
  begin
    //Gets the middle of the selected range
    Pivot := (First + Last) div 2;
    //Compares the String in the middle with the searched one
    if n.Numbers[Pivot] = num then
    begin
      Found := True;
      Result := Pivot;
    end
    //If the Item in the middle has a bigger value than
    //the searched item, then select the first half
    else if n.Numbers[Pivot] > num then
      Last := Pivot - 1
        //else select the second half
    else 
      First := Pivot + 1;
  end;
end;

type
  qiteratordata_t = record
    n: TDNumberList;
    bmp: TBitmap;
    L: PLongWordArray;
    id: integer;
    numids: integer;
  end;
  qiteratordata_p = ^qiteratordata_t;

function findcolorindexes_thr(p: Pointer): integer; stdcall;
var
  parms: qiteratordata_p;
  i, j: integer;
  L: PLongWordArray;
  n: TDNumberList;
  bmp: TBitmap;
  B: PByteArray;
  oldL, c: LongWord;
  oldI, idx: integer;
begin
  parms := qiteratordata_p(p);
  n := parms.n;
  bmp := parms.bmp;
  L := parms.L;

  oldL := n.Numbers[0];
  oldI := 0;

  for i := 0 to bmp.Height - 1 do
    if i mod parms.numids = parms.id then
    begin
      B := bmp.ScanLine[i];
      for j := 0 to bmp.Width - 1 do
      begin
        idx := i * bmp.Width + j;
        c := L[idx];
        if c = oldL then
          B[j] := oldI
        else
        begin
          oldI := searchsorted(n, c);
          B[j] := oldI;
          oldL := c;
        end;
      end;
    end;

  Result := 0;
end;

procedure ReduceTo8Bit(var bmp: TBitmap);
var
  L: PLongWordArray;
  imgsize: integer;
  B: PByteArray;
  i, j: integer;
  tmp: byte;
  pal: tagLOGPALETTE255;
  lpal: PLogPalette;
  n: TDNumberList;
  oldf: TPixelFormat;
  oldL, c: LongWord;
  oldI, idx: integer;
  parms: array[0..7] of qiteratordata_t;
begin
  imgsize := bmp.Width * bmp.Height;
  oldf := bmp.PixelFormat;
  bmp.PixelFormat := pf32bit;
  L := malloc(imgsize * 4);
  for i := 0 to bmp.Height - 1 do
    memcpy(@L[i * bmp.Width], bmp.ScanLine[i], 4 * bmp.Width);

  bmp.PixelFormat := pf8bit;
  pool := nil;
  nodeslen := 0;

  n := TDNumberList.Create;

  try
    color_quantize_image(L, imgsize, 256, n, True);
  except
    bmp.PixelFormat := oldf;
    memfree(Pointer(L), imgsize * 4);
    Exit;
  end;

{  oldL := L[0];
  n.Add(oldL);

  for i := 1 to imgsize - 1 do
  begin
    if L[i] <> oldL then
      if n.IndexOf(L[i]) < 0 then
      begin
        n.Add(L[i]);
        if n.Count = 256 then
          Break;
      end;
  end;      }

  n.Sort;

  for i := 0 to n.Count - 1 do
    pal.palPalEntry[i] := n.Numbers[i];
  for i := n.Count to 255 do
    pal.palPalEntry[i] := 0;

  pal.palVersion := $300;
  pal.palNumEntries := 256;
  lpal := @pal;
  for i := 0 to 255 do
  begin
    tmp := lpal.palPalEntry[i].peRed;
    lpal.palPalEntry[i].peRed := lpal.palPalEntry[i].peBlue;
    lpal.palPalEntry[i].peBlue := tmp;
  end;

  bmp.Palette := CreatePalette(lpal^);


  if usemultithread then
  begin
    for i := 0 to 7 do
    begin
      parms[i].n := n;
      parms[i].bmp := bmp;
      parms[i].L := L;
      parms[i].id := i;
      parms[i].numids := 8;
    end;
    MT_Execute(
        @findcolorindexes_thr, @parms[0],
        @findcolorindexes_thr, @parms[1],
        @findcolorindexes_thr, @parms[2],
        @findcolorindexes_thr, @parms[3],
        @findcolorindexes_thr, @parms[4],
        @findcolorindexes_thr, @parms[5],
        @findcolorindexes_thr, @parms[6],
        @findcolorindexes_thr, @parms[7]
      );
  end
  else
  begin
    oldL := n.Numbers[0];
    oldI := 0;

    for i := 0 to bmp.Height - 1 do
    begin
      B := bmp.ScanLine[i];
      for j := 0 to bmp.Width - 1 do
      begin
        idx := i * bmp.Width + j;
        c := L[idx];
        if c = oldL then
          B[j] := oldI
        else
        begin
          oldI := searchsorted(n, c);
          B[j] := oldI;
          oldL := c;
        end;
      end;
    end;
  end;

  memfree(Pointer(L), imgsize * 4);
  n.Free;
end;

end.
