program Calculator;

uses
  SysUtils,
  MathCore.Math,
  MathUtils.Statistics;

var
  values: array[0..4] of Integer;
  i: Integer;
begin
  WriteLn('Simple Multi-Module Calculator');
  WriteLn('==============================');
  WriteLn('');

  WriteLn('Basic Math Operations:');
  WriteLn('  5 + 3 = ', Add(5, 3));
  WriteLn('  10 - 4 = ', Subtract(10, 4));
  WriteLn('  6 * 7 = ', Multiply(6, 7));
  WriteLn('  20 / 4 = ', Divide(20, 4));
  WriteLn('');

  WriteLn('Statistics Operations:');
  values[0] := 10;
  values[1] := 20;
  values[2] := 30;
  values[3] := 40;
  values[4] := 50;

  WriteLn('  Values: ');
  for i := 0 to High(values) do
    Write(values[i], ' ');
  WriteLn('');

  WriteLn('  Average: ', Average(values));
  WriteLn('  Maximum: ', Max(values));
  WriteLn('  Minimum: ', Min(values));
  WriteLn('');

  WriteLn('Build successful! All modules compiled correctly.');
end.
