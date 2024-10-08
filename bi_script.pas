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
//    Script engine
//
//------------------------------------------------------------------------------
//  E-Mail: jvalavanis@gmail.com
//  Site  : https://sourceforge.net/projects/brickinventory/
//------------------------------------------------------------------------------

unit bi_script;

interface

uses
  SysUtils, Classes;

//
// JVAL
//
// Script Engine
//

// TScriptEngine
const
  MAX_STRING_SIZE = 1024;

type
  TScriptEngine = class
  private
    sc_String: PChar;
    sc_Integer: integer;
    sc_Float: double;
    sc_Line: integer;
    sc_End: boolean;
    fNewLine: boolean;
    fBracketLevel: integer;
    fParenthesisLevel: integer;
    ScriptBuffer: PChar;
    ScriptPtr: PChar;
    ScriptEndPtr: PChar;
    StringBuffer: array [0..MAX_STRING_SIZE] of char;
    ScriptSize: integer;
    AlreadyGot: boolean;
    ignonelist: TStringList;
  protected
    function fToken: string;
  public
    constructor Create(const tx: string); virtual;
    destructor Destroy; override;
    procedure AddIgnoreToken(const s: string);
    procedure Clear;
    procedure SetText(const tx: string); virtual;
    procedure ScriptError(const err: string); overload;
    procedure ScriptError(const Fmt: string; const Args: array of const); overload;
    function GetString: boolean;
    function GetStringEOL: string;
    procedure MustGetString;
    procedure MustGetStringName(const name: string);
    function GetInteger: boolean;
    procedure MustGetInteger;
    function GetFloat: boolean;
    procedure MustGetFloat;
    procedure UnGet;
    function MatchString(const strs: TStringList): integer; overload;
    function MatchString(const str: string): boolean; overload;
    function MatchPosString(const str: string): boolean;
    function MustMatchString(strs: TStringList): integer;
    function Compare(const txt: string): boolean;
    property _Integer: integer read sc_Integer;
    property _Float: double read sc_Float;
    property _String: string read fToken;
    property _Finished: boolean read sc_End;
    property _Line: integer read sc_Line;
    property NewLine: boolean read fNewLine;
    property BracketLevel: integer read fBracketLevel;
    property ParenthesisLevel: integer read fParenthesisLevel;
  end;

function RemoveLineQuotes(const sctext: string): string;


implementation

uses
  bi_delphi;

const
  ASCII_QUOTE = '"';
  ASCII_COMMENT1 = '/';
  ASCII_COMMENT = $2F2F; // = '//'


// TScriptEngine
constructor TScriptEngine.Create(const tx: string);
begin
  Inherited Create;
  ignonelist := TStringList.Create;
  fBracketLevel := 0;
  fParenthesisLevel := 0;
  fNewLine := False;
  Clear;
  SetText(tx);
end;

destructor TScriptEngine.Destroy;
begin
  Clear;
  ignonelist.Free;
  Inherited;
end;

procedure TScriptEngine.AddIgnoreToken(const s: string);
begin
  ignonelist.Add(UpperCase(s));
end;

function TScriptEngine.fToken: string;
begin
  Result := string(sc_String);
end;

procedure TScriptEngine.Clear;
begin
  if ScriptSize > 0 then
    freemem(pointer(ScriptBuffer), ScriptSize);

  ScriptBuffer := nil;
  ScriptSize := 0;
  ScriptPtr := ScriptBuffer;
  ScriptEndPtr := ScriptPtr + ScriptSize;
  sc_Line := 1;
  sc_End := False;
  sc_String := @StringBuffer[0];
  AlreadyGot := False;
end;

procedure TScriptEngine.SetText(const tx: string);
var
  p: Pointer;
  size: integer;
begin
  size := Length(tx);
  GetMem(p, size);
  Move(tx[1], p^, size);

  Clear;
  ScriptBuffer := p;
  ScriptSize := size;
  ScriptPtr := ScriptBuffer;
  ScriptEndPtr := ScriptPtr + ScriptSize;
  sc_Line := 1;
  sc_End := False;
  sc_String := @StringBuffer[0];
  AlreadyGot := False;
end;

procedure TScriptEngine.ScriptError(const err: string);
begin

//  I_Warning('%s'#13#10, [err]);
end;

procedure TScriptEngine.ScriptError(const fmt: string; const Args: array of const);
var
  s: string;
begin
  s := Format(fmt, Args);
  ScriptError(s);
end;


procedure TScriptEngine.MustGetString;
begin
  if not GetString then
    ScriptError('TScriptEngine.MustGetString(): Missing string at Line %d', [sc_Line]);
end;

procedure TScriptEngine.MustGetStringName(const name: string);
begin
  MustGetString;
  if not Compare(name) then
    ScriptError('TScriptEngine.MustGetStringName(): "%s" expected at Line %d', [name, sc_Line]);
end;

function TScriptEngine.GetInteger: boolean;
var
  code: integer;
begin
  if GetString then
  begin
    val(string(sc_String), sc_Integer, code);
    if code <> 0 then
    begin
      ScriptError(
          'TScriptEngine.GetInteger(): Bad numeric constant "%s" at Line %d',
          [sc_String, sc_Line]);
      Result := False;
      Exit;
    end;
    Result := True;
  end
  else
    Result := False;
end;

procedure TScriptEngine.MustGetInteger;
begin
  if not GetInteger then
    ScriptError('TScriptEngine.MustGetInteger(): Missing integer at Line %d', [sc_Line]);
end;

type
  TCharArray = packed array[0..$FFFF] of Char;
  PCharArray = ^TCharArray;

function IsNumericC(const c: char): boolean;
begin
  Result := c in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
end;

function TScriptEngine.GetFloat: boolean;
var
  code: integer;
  i: integer;
  str: string;
  aa: PCharArray;
begin
  if GetString then
  begin
    str := string(sc_String);
    if Integer(ScriptEndPtr) - Integer(ScriptPtr) > 4 then
    begin
      aa := PCharArray(ScriptPtr);
      if (aa[0] = ',') and (CharPos(',', str) = 0) and (CharPos('.', str) = 0) then
        if IsNumericC(aa[1]) and IsNumericC(aa[2]) and IsNumericC(aa[3]) then
          if aa[4] = '.' then
          begin
            GetString;
            str := str + string(sc_String);
          end;
    end;
    val(str, sc_Float, code);
    if code <> 0 then
    begin
      for i := 1 to Length(str) do
        if str[i] in ['.', ','] then
          str[i] := '.';
      val(str, sc_Float, code);
      if code <> 0 then
      begin
        for i := 1 to Length(str) do
          if str[i] = '.' then
            str[i] := ',';
        val(str, sc_Float, code);
        if code <> 0 then
        begin
          ScriptError(
              'TScriptEngine.GetFloat(): Bad numeric constant "%s" at Line %d',
              [sc_String, sc_Line]);
          Result := False;
          Exit;
        end;
      end;
    end;
    Result := True;
  end
  else
    Result := True;
end;

procedure TScriptEngine.MustGetFloat;
begin
  if not GetFloat then
    ScriptError('TScriptEngine.MustGetFloat(): Missing double at Line %d', [sc_Line]);
end;

procedure TScriptEngine.UnGet;
// Assumes there is a valid string in sc_String.
begin
  AlreadyGot := True;
end;

function TScriptEngine.MatchString(const str: string): boolean;
begin
  Result := Compare(str);
end;

function TScriptEngine.MatchString(const strs: TStringList): integer;
// Returns the index of the first match to sc_String from the passed
// array of strings, or -1 if not found.
var
  i: integer;
begin
  for i := 0 to strs.Count - 1 do
  begin
    if Compare(strs.Strings[i]) then
    begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;

function TScriptEngine.MatchPosString(const str: string): boolean;
begin
  Result := Pos(UpperCase(str), UpperCase(string(sc_String))) > 0;
end;

function TScriptEngine.MustMatchString(strs: TStringList): integer;
var
  i: integer;
begin
  i := MatchString(strs);
  if i = -1 then
    ScriptError('TScriptEngine.MustMatchString(): List'#13#10'%s'#13#10'expected at Line %d',  [strs.Text, sc_Line]);

  Result := i;
end;

function TScriptEngine.Compare(const txt: string): boolean;
begin
  Result := UpperCase(txt) = UpperCase(string(sc_String));
end;

function TScriptEngine.GetString: boolean;
var
  txt: PChar;
  foundToken: boolean;
begin
  if AlreadyGot then
  begin
    AlreadyGot := False;
    Result := True;
    Exit;
  end;

  fNewLine := False;
  foundToken := False;
  if ScriptPtr >= ScriptEndPtr then
  begin
    sc_End := True;
    Result := False;
    Exit;
  end;

  while not foundToken do
  begin
    while (ScriptPtr^ <= Chr(32)) or (ScriptPtr^ in ['{', '}', '(', ')', ',']) do
    begin
      if ScriptPtr >= ScriptEndPtr then
      begin
        sc_End := True;
        Result := False;
        Exit;
      end;
      if ScriptPtr^ = '{' then
        inc(fBracketLevel)
      else if ScriptPtr^ = '}' then
      begin
        dec(fBracketLevel);
        if fBracketLevel < 0 then
          ScriptError('TScriptEngine.GetString(): Closing bracket "}" found at line %d without opening bracket "{"', [sc_Line]);
      end
      else if ScriptPtr^ = '(' then
        inc(fParenthesisLevel)
      else if ScriptPtr^ = ')' then
      begin
        dec(fParenthesisLevel);
        if fParenthesisLevel < 0 then
          ScriptError('TScriptEngine.GetString(): Closing parenthesis ")" found at line %d without opening parenthesis "("', [sc_Line]);
      end
      else if ScriptPtr^ = Chr(10) then
      begin
        inc(sc_Line);
        fNewLine := True;
      end;
      inc(ScriptPtr);
    end;

    if ScriptPtr >= ScriptEndPtr then
    begin
      sc_End := True;
      Result := False;
      Exit;
    end;

    if ScriptPtr^ <> ASCII_COMMENT1 then
    begin // Found a token
      foundToken := True;
    end
    else
    begin // Skip comment

      if ScriptPtr >= ScriptEndPtr then
      begin
        sc_End := True;
        Result := False;
        Exit;
      end;
      inc(ScriptPtr);
      if ScriptPtr^ = ASCII_COMMENT1 then
      begin
        while (ScriptPtr^ <> Chr(13)) and (ScriptPtr^ <> Chr(10)) do
        begin
          if ScriptPtr >= ScriptEndPtr then
          begin
            sc_End := True;
            Result := False;
            Exit;
          end;
          inc(ScriptPtr);
        end;
        inc(sc_Line);
        fNewLine := True;
      end;
    end;
  end;

  txt := sc_String;
  if ScriptPtr^ = ASCII_QUOTE then
  begin // Quoted string
    inc(ScriptPtr);
    while ScriptPtr^ <> ASCII_QUOTE do
    begin
      txt^ := ScriptPtr^;
      inc(txt);
      inc(ScriptPtr);
      if (ScriptPtr = ScriptEndPtr) or
         (txt = @sc_String[MAX_STRING_SIZE - 1]) then
        break;
    end;
    inc(ScriptPtr);
  end
  else
  begin // Normal string
    while (ScriptPtr^ > Chr(32)) and (ScriptPtr < ScriptEndPtr) and
          (PWord(ScriptPtr)^ <> ASCII_COMMENT) and (not (ScriptPtr^ in ['{', '}', '(', ')', ','])) do
    begin
      txt^ := ScriptPtr^;
      inc(txt);
      inc(ScriptPtr);
      if (ScriptPtr = ScriptEndPtr) or
         (txt = @sc_String[MAX_STRING_SIZE - 1]) then
        break;

    end;
  end;
  txt^ := Chr(0);
  if ignonelist.IndexOf(UpperCase(string(sc_String))) < 0 then
    Result := True
  else
    Result := GetString;
end;

function TScriptEngine.GetStringEOL: string;
begin
  Result := '';
  if not GetString then
    Exit;

  if fNewLine then
  begin
    AlreadyGot := True;
    Exit;
  end;
  Result := string(sc_string);
  while not sc_End and not fNewLine do
  begin
    GetString;
    if fNewLine then
    begin
      AlreadyGot := True;
      Exit;
    end;
    Result := Result + ' ' + string(sc_string);
  end;
end;


function RemoveLineQuotes(const sctext: string): string;
var
  stmp: string;
  s: TStringList;
  i, p: integer;
begin                               
  Result := '';
  s := TStringList.Create;
  try
    s.Text := sctext;
    for i := 0 to s.Count - 1 do
    begin
      stmp := Trim(s[i]);
      p := Pos('//', stmp);
      if p > 0 then
        stmp := Copy(stmp, 1, p - 1);
      p := CharPos(';', stmp);
      if p > 0 then
        stmp := Copy(stmp, 1, p - 1);
      if stmp <> '' then
        Result := Result + stmp + #13#10;
    end;
  finally
    s.Free;
  end;
end;

end.

