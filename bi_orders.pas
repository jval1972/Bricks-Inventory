//------------------------------------------------------------------------------
//
//  BrickInventory: A tool for managing your brick collection
//  Copyright (C) 2014-2019 by Jim Valavanis
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
//    Bricklink orders
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit bi_orders;

interface

uses
  SysUtils, Classes, bi_db, bl_orderxml;

type
  ordercost_t = record
    num: integer;
    totcost: double;
  end;

type
  orders_t = array[0..1023] of IXMLORDERSType;
  orders_p = ^orders_t;

type
  orderevalhistory_t = record
    time: TDateTime;
    eval: double;
  end;

  TOrderItemInfo = class
    orderid: integer;
    orderdate: string;
    orderseller: string;
    orderstatus: string;
    part: string;
    color: integer;
    condition: string;
    num: integer;
    price: Double;
    pricetot: Double;
    currency: string;
  end;

  TOrders = class
  private
    fnumorders: integer;
    forders: orders_t;
    fpieceorderinfo: TStringList;
    procedure ProssesOrder(const oo: IXMLORDERType);
    procedure ProssesItem(const oo: IXMLORDERType; const ii: IXMLITEMType);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure LoadFilesDirectory(dir: string);
    procedure LoadFile(fname: string);
    function ItemInfo(const part: string; const color: Integer): TStringList;
    function ItemCost(const part: string; const color: Integer): ordercost_t;
    function ItemCostDbl(const part: string; const color: Integer): double;
    function order(const index: integer): IXMLORDERType; overload;
    function order(const index: string): IXMLORDERType; overload;
    function OrderInventory(const orderid: integer): TBrickInventory; overload;
    function OrderInventory(const orderid: string): TBrickInventory; overload;
    function orderEvaluatedPrice(const index: integer): double; overload;
    function orderEvaluatedPrice(const index: string): double; overload;
    procedure StoreEvalHistory(const fn: string; const index: string);
    property numorders: integer read fnumorders;
    property orders: orders_t read forders;
  end;

function EvaluatedPrice(const order: IXMLORDERType): Double;

implementation

uses
  bi_delphi, bi_utils, bi_io, bi_system, bi_globals;

constructor TOrders.Create;
begin
  fnumorders := 0;
  ZeroMemory(@forders, SizeOf(orders_t));
  fpieceorderinfo := TStringList.Create;
  fpieceorderinfo.Sorted := True;
  inherited;
end;

destructor TOrders.Destroy;
var
  i, j: integer;
begin
  for i := 0 to fpieceorderinfo.Count - 1 do
    for j := 0 to (fpieceorderinfo.Objects[i] as TStringList).Count - 1 do
      (fpieceorderinfo.Objects[i] as TStringList).Objects[j].Free;
  FreeList(fpieceorderinfo);
  for i := 0 to fnumorders - 1 do
    I_ClearInterface(IInterface(forders[i]));
  inherited;
end;

function TOrders.order(const index: integer): IXMLORDERType;
var
  i, j: integer;
begin
  for i := 0 to fnumorders - 1 do
    for j := 0 to forders[i].Count - 1 do
      if forders[i].ORDER[j].ORDERID = index then
      begin
        Result := forders[i].ORDER[j];
        Exit;
      end;
  Result := nil;
end;

function TOrders.order(const index: string): IXMLORDERType;
var
  idx: integer;
begin
  idx := StrToIntDef(index, -1);
  if idx < 0 then
  begin
    Result := nil;
    Exit;
  end;
  Result := order(idx);
end;

function TOrders.OrderInventory(const orderid: integer): TBrickInventory;
var
  i, j: integer;
  oo: IXMLORDERType;
  ii: IXMLITEMType;
begin
  Result := TBrickInventory.Create;
  oo := order(orderid);
  if oo = nil then
    Exit;

  for i := 0 to oo.ITEM.Count - 1 do
  begin
    ii := oo.ITEM.Items[i];
    if ii.ITEMTYPE = 'P' then
      Result.AddLoosePart(db.RebrickablePart(ii.ITEMID), db.BrickLinkColorToSystemColor(ii.COLOR), ii.QTY)
    else if (ii.ITEMTYPE = 'S') or (ii.ITEMTYPE = 'M') then
      for j := 0 to ii.QTY - 1 do
        Result.AddSet(db.RebrickablePart(ii.ITEMID), False);
  end;
end;

function TOrders.OrderInventory(const orderid: string): TBrickInventory;
var
  idx: integer;
begin
  idx := StrToIntDef(orderid, -1);
  Result := OrderInventory(idx);
end;

procedure TOrders.ProssesOrder(const oo: IXMLORDERType);
var
  i: integer;
begin
  for i := 0 to oo.ITEM.Count - 1 do
    ProssesItem(oo, oo.ITEM.Items[i]);
end;

procedure TOrders.ProssesItem(const oo: IXMLORDERType; const ii: IXMLITEMType);
var
  s, scolor: string;
  price: Double;
  pricetot: Double;
  idx: integer;
  lst: TStringList;
  oinf: TOrderItemInfo;
  rcolor: integer;
  tp1, tp2, tp3: double;
  tq1, tq2, tq3: integer;
  oname: string;
begin
  rcolor := db.BrickLinkColorToSystemColor(ii.COLOR);
  scolor := IntToStr(rcolor);
  s := db.RebrickablePart(ii.ITEMID) + ',' + scolor;
  idx := fpieceorderinfo.IndexOf(s);
  if idx < 0 then
    idx := fpieceorderinfo.AddObject(s, TStringList.Create);
  lst := fpieceorderinfo.Objects[idx] as TStringList;
  lst.Sorted := True;
  oinf := TOrderItemInfo.Create;
  oname := itoa(oo.ORDERID);
  oinf.orderid := oo.ORDERID;
  oinf.orderdate := oo.ORDERDATE;
  oinf.orderseller := oo.SELLER;
  oinf.orderstatus := oo.ORDERSTATUS;
  oinf.part := ii.ITEMID;
  oinf.color := rcolor;
  oinf.condition := ii.CONDITION;
  oinf.num := ii.QTY;
  tp1 := atof(ii.TP1, 0.0);
  tp2 := atof(ii.TP2, 0.0);
  tp3 := atof(ii.TP3, 0.0);
  tq1 := atoi(ii.TQ1, MAXINT);
  tq2 := atoi(ii.TQ2, MAXINT);
  tq3 := atoi(ii.TQ3, MAXINT);

  if oinf.num >= tq3 then
    price := tp3
  else if oinf.num >= tq2 then
    price := tp2
  else if oinf.num >= tq1 then
    price := tp1
  else
    price := atof(ii.PRICE);
  price := price * (100 - atof(ii.sale, 0.0)) / 100;
  oinf.price := price;
  pricetot := price * atof(oo.BASEGRANDTOTAL) / atof(oo.ORDERTOTAL);
  //*******************************************
  oinf.price := atof(ii.PRICE);
  pricetot := atof(ii.PRICE) * atof(oo.BASEGRANDTOTAL) / atof(oo.ORDERTOTAL);
  //*******************************************
  oinf.pricetot := pricetot;
  oinf.currency := oo.BASECURRENCYCODE;
  lst.AddObject(oname, oinf);
end;

procedure TOrders.LoadFilesDirectory(dir: string);
var
  files: TDStringList;
  i, j: integer;
begin
  if Length(dir) > 0 then
    if not (dir[Length(dir)] in ['\', '/']) then
      dir := dir + '\';
  files := findfiles(dir + '*.xml');
  for i := 0 to files.Count - 1 do
  begin
    printf('Loading orders file %s'#13#10, [files.Strings[i]]);
    forders[fnumorders] := LoadORDERS(dir + files.Strings[i]);
    for j := 0 to forders[fnumorders].Count - 1 do
      ProssesOrder(forders[fnumorders].ORDER[j]);
    Inc(fnumorders);
  end;
  files.Free;
end;

procedure TOrders.LoadFile(fname: string);
var
  i: integer;
begin
  printf('Loading orders file %s'#13#10, [fname]);
  forders[fnumorders] := LoadORDERS(fname);
  for i := 0 to forders[fnumorders].Count - 1 do
    ProssesOrder(forders[fnumorders].ORDER[i]);
  Inc(fnumorders);
end;

function TOrders.ItemInfo(const part: string; const color: Integer): TStringList;
var
  s: string;
  idx: integer;
begin
  s := part + ',' + IntToStr(color);
  idx := fpieceorderinfo.IndexOf(s);
  if idx < 0 then
  begin
    s := db.RebrickablePart(part) + ',' + IntToStr(color);
    idx := fpieceorderinfo.IndexOf(s);
    if idx < 0 then
    begin
      s := db.BrickLinkPart(part) + ',' + IntToStr(color);
      idx := fpieceorderinfo.IndexOf(s);
    end;
    if idx < 0 then
    begin
      Result := nil;
      Exit;
    end;
  end;
  Result := fpieceorderinfo.Objects[idx] as TStringList;
end;

function TOrders.ItemCost(const part: string; const color: Integer): ordercost_t;
var
  oinf: TStringList;
  oitem: TOrderItemInfo;
  i: integer;
  curconv: Double;
  totbricks: integer;
  totprice: Double;
begin
  oinf := ItemInfo(part, color);

  if oinf = nil then
  begin
    Result.num := 0;
    Result.totcost := 0.0;
    Exit;
  end;

  if oinf.Count = 0 then
  begin
    Result.num := 0;
    Result.totcost := 0.0;
    Exit;
  end;

  totbricks := 0;
  totprice := 0.0;

  for i := 0 to oinf.Count - 1 do
  begin
    oitem := oinf.Objects[i] as TOrderItemInfo;

    totbricks := totbricks + oitem.num;

    curconv := db.ConvertCurrency(oitem.currency);
    totprice := totprice + oitem.num * oitem.pricetot * curconv;
  end;

  Result.num := totbricks;
  Result.totcost := totprice;
end;

function TOrders.ItemCostDbl(const part: string; const color: Integer): double;
var
  oret: ordercost_t;
begin
  oret := ItemCost(part, color);
  if oret.num = 0 then
    Result := 0.0
  else
    Result := oret.totcost / oret.num;
end;

function TOrders.orderEvaluatedPrice(const index: integer): double;
begin
  Result := EvaluatedPrice(order(index));
end;

function TOrders.orderEvaluatedPrice(const index: string): double;
begin
  Result := EvaluatedPrice(order(index));
end;

procedure TOrders.StoreEvalHistory(const fn: string; const index: string);
var
  e: orderevalhistory_t;
  f: TFileStream;
begin
  e.eval := orderEvaluatedPrice(index);
  e.time := Now;

  if fexists(fn) then
  begin
    f := TFileStream.Create(fn, fmOpenReadWrite or fmShareDenyWrite);
    f.Position := f.Size;
  end
  else
    f := TFileStream.Create(fn, fmCreate or fmShareDenyWrite);

  f.Write(e, SizeOf(e));
  f.Free;
end;

function EvaluatedPrice(const order: IXMLORDERType): Double;
var
  i: integer;
  it: IXMLITEMType;
  pci: TPieceColorInfo;
begin
  Result := 0.0;
  if order = nil then
    Exit;

  for i := 0 to order.ITEM.Count - 1 do
  begin
    it := order.ITEM.Items[i];
    pci := db.piececolorinfo(db.RebrickablePart(it.ITEMID), db.BrickLinkColorToSystemColor(it.COLOR));
    if pci <> nil then
    begin
      if it.CONDITION = 'N' then
        Result := Result + it.QTY * pci.EvaluatePriceNew
      else
        Result := Result + it.QTY * pci.EvaluatePriceUsed;
    end
    else
    begin
      I_Warning('EvaluatedPrice(): Missing priceguide for (%s,%d)'#13#10, [it.ITEMID, it.COLOR]);
      Result := Result + it.QTY * atof(it.PRICE);
    end;
  end;

end;

end.
