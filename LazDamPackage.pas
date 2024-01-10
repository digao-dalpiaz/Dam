{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazDamPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  Vcl.DamUnit, Vcl.DamDialog, DamLanguage, DamInternalExcept, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('LazDamPackage', @Register);
end.
