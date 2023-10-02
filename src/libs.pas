unit Libs;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

function IsInteger(s: string): boolean;

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

end.

