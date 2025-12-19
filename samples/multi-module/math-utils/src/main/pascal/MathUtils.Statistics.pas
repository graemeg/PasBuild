unit MathUtils.Statistics;

interface

uses
  MathCore.Math;

function Average(values: array of Integer): Integer;
function Max(values: array of Integer): Integer;
function Min(values: array of Integer): Integer;

implementation

uses
  SysUtils;

function Average(values: array of Integer): Integer;
var
  i, sum: Integer;
begin
  if Length(values) = 0 then
    raise Exception.Create('Empty array');

  sum := 0;
  for i := 0 to High(values) do
    sum := Add(sum, values[i]);

  Result := Divide(sum, Length(values));
end;

function Max(values: array of Integer): Integer;
var
  i, lmax: Integer;
begin
  if Length(values) = 0 then
    raise Exception.Create('Empty array');

  lmax := values[0];
  for i := 1 to High(values) do
    if values[i] > lmax then
      lmax := values[i];

  Result := lmax;
end;

function Min(values: array of Integer): Integer;
var
  i, lmin: Integer;
begin
  if Length(values) = 0 then
    raise Exception.Create('Empty array');

  lmin := values[0];
  for i := 1 to High(values) do
    if values[i] < lmin then
      lmin := values[i];

  Result := lmin;
end;

end.
