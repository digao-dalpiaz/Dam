{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazDamPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  DamUnit, DamDialog, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('LazDamPackage', @Register);
end.
