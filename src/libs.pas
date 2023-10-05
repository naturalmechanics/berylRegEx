unit Libs;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Character;
Type
  TstringArray      = Array of String;

function IsInteger(s: string): boolean;
function split_byWhiteSpace(s: String) : TstringArray;

implementation

function IsInteger(s: string): boolean;
var
  c: char;
begin
  Result := s.length > 0;
  for c in s do
    if not (c in ['0'..'9']) then
      exit(False);
end;

function split_byWhiteSpace(s: String) : TStringArray;
var
  strArr : Array of String;
  i : Integer;
  reading : Boolean;
  c : Char;
  curr : String;

begin
  reading := False;

  curr    := '';
  SetLength(StrArr,0);

  for i := 1 to Length(s) do
  begin

    c := s[i];

    if isWhiteSpace(c) and (not reading) then Continue;
    if isWhiteSpace(c) and (reading) then
    begin

      SetLength(StrArr, Length(StrArr) + 1);
      StrArr[Length(StrArr) - 1] := curr ;

      curr := '';
      reading := False;
      Continue;
    end;

    curr := curr + c;

    reading := True;

  end;

  if Trim (curr) <> ''  then
  begin

    SetLength(StrArr, Length(StrArr) + 1);
    StrArr[Length(StrArr) - 1] := curr ;

  end;

  Result := StrArr;
end;

end.

