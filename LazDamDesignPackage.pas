{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazDamDesignPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  Vcl.DamFileGenerator, Vcl.DamFind, Vcl.DamList, Vcl.DamMsgEdit, Vcl.DamReg, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('Vcl.DamReg', @Vcl.DamReg.Register);
end;

initialization
  RegisterPackage('LazDamDesignPackage', @Register);
end.
