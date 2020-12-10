//------------------------------------------------------------------------------
//
//  BrickInventory: A tool for managing your brick collection
//  Copyright (C) 2014-2018 by Jim Valavanis
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
//    Price adjustments
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit bi_priceadjust;

interface

uses
  bi_db, bi_delphi, DateUtils;

procedure PRICEADJUST(const part: string; const color: integer; var pg: priceguide_t; var av: availability_t; const t: TDateTime); overload;

procedure PRICEADJUST(const part: string; const color: integer; const A: parecdate_p); overload;

implementation

procedure PRICEADJUST(const part: string; const color: integer; var pg: priceguide_t; var av: availability_t; const t: TDateTime); overload;
var
  allmoney: extended;
begin
  if color = 334 then
  begin
    if part = '3001' then
    begin
      if between(pg.uMaxPrice, 9999, 10000) then
      begin
        if pg.uTotalQty > 1 then
        begin
          pg.uQtyAvgPrice := (pg.uTotalQty * pg.uQtyAvgPrice - 9999.99) / (pg.uTotalQty - 1);
          pg.uTotalQty := pg.uTotalQty - 1;
        end
        else
          pg.uQtyAvgPrice := pg.nQtyAvgPrice * 0.8;
        pg.uAvgPrice := pg.uQtyAvgPrice;
      end;
      Exit;
    end;
    Exit;
  end;
  if color = -1 then
  begin
    if part = '5766-1' then
    begin
      if av.nTotalQty > 50000 then
      begin
        allmoney := av.nTotalQty * av.nQtyAvgPrice;
        av.nTotalQty := av.nTotalQty - 50000;
        av.nTotalLots := av.nTotalLots - 1;
        allmoney := allmoney - 50000 * 0.0008;
        av.nQtyAvgPrice := dbl_safe_div(allmoney, av.nTotalQty);
      end;
      Exit;
    end;
    Exit;
  end;
end;

procedure PRICEADJUST(const part: string; const color: integer; const A: parecdate_p); overload;
begin
  PRICEADJUST(part, color, A.priceguide, A.availability, A.date);
end;

end.
