unit MathCore.Math;

interface

function Add(a, b: Integer): Integer;
function Subtract(a, b: Integer): Integer;
function Multiply(a, b: Integer): Integer;
function Divide(a, b: Integer): Integer;

implementation

uses
  SysUtils;

function Add(a, b: Integer): Integer;
begin
  Result := a + b;
end;

function Subtract(a, b: Integer): Integer;
begin
  Result := a - b;
end;

function Multiply(a, b: Integer): Integer;
begin
  Result := a * b;
end;

function Divide(a, b: Integer): Integer;
begin
  if b = 0 then
    raise Exception.Create('Division by zero');
  Result := a div b;
end;

end.
