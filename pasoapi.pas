{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pasoapi;

{$warn 5023 off : no warning about unused units}
interface

uses
  PasOAPI.Objects, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pasoapi', @Register);
end.
