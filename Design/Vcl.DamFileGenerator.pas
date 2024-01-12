{$IFNDEF DESIGN_FMX}unit Vcl.DamFileGenerator;{$ENDIF}

interface

uses {$INCLUDE 'DamUnitByEnvDesign.inc'};

procedure GenerateFile(Dam: TDam; const Template: string);

implementation

uses
{$IFDEF FPC}
  Classes, SysUtils, LazIDEIntf
{$ELSE}
  System.Classes, System.SysUtils, ToolsAPI
{$ENDIF};

procedure GenerateFile(Dam: TDam; const Template: string);
var
{$IFNDEF FPC}
  ModServices: IOTAModuleServices;
  Module: IOTAModule;
{$ENDIF}
  C: TComponent;
  Dir: string;
  StmUnit: string;
  Msg: TDamMsg;
  S: TStringList;
  A, aFuncName, aFuncKind, aResFunc, aPreCmd, aCmd, aMethod, aParDec, aParUse, aCab, aFunc: string;
  aDeclarations, aImplementations: TStringBuilder;
const ENTER = #13#10;
begin
{$IFDEF FPC}
  Dir := LazarusIDE.ActiveProject.Directory;
{$ELSE}
  ModServices := BorlandIDEServices as IOTAModuleServices;
  Module := ModServices.FindFormModule(Dam.Owner.Name);
  if Module = nil then
    raise Exception.Create('Form Module not found');

  Dir := ExtractFilePath(Module.FileName);
{$ENDIF}

  StmUnit := Dam.DamUnitName;

  aDeclarations := TStringBuilder.Create;
  aImplementations := TStringBuilder.Create;
  try
    for C in Dam.Owner do
    begin
      if not (C is TDamMsg) then Continue;

      Msg := TDamMsg(C);
      if not SameText(Msg.Dam.DamUnitName, StmUnit) then Continue;

      A := Msg.Name;
      if A[1] = '_' then Delete(A, 1, 1);

      aFuncName := A;

      aCmd := Dam.Owner.Name+'.'+Msg.Name;

      aParDec := '';
      aParUse := '';
      if Pos('%p', Msg.Message) > 0 then
      begin
        aParDec := '(Params: TDamParams)';
        aParUse := '(Params)';
      end;

      if (Msg.RaiseExcept) or (Msg.Buttons in [dbOK, dbOne]) then
      begin
        aFuncKind := 'procedure';
        aResFunc := '';
        aPreCmd := '';
        aMethod := 'Run';
      end else
      if (Msg.Buttons in [dbYesNo, dbTwo]) then
      begin
        aFuncKind := 'function';
        aResFunc := ': Boolean';
        aPreCmd := 'Result := ';
        aMethod := 'RunAsBool';
      end else
      begin
        aFuncKind := 'function';
        aResFunc := ': TDamMsgRes';
        aPreCmd := 'Result := ';
        aMethod := 'Run';
      end;

      aCab := aFuncKind+' '+aFuncName+aParDec+aResFunc+';';
      aFunc :=
        aCab+ENTER+
        'begin'+ENTER+
        '  '+aPreCmd+aCmd+'.'+aMethod+aParUse+';'+ENTER+
        'end;'+ENTER;

      aDeclarations.AppendLine(aCab);
      aImplementations.AppendLine(aFunc);
    end;

    if aDeclarations.Length=0 then aDeclarations.AppendLine('//No Dam Messages defined');
    if aImplementations.Length=0 then aImplementations.AppendLine('//No Dam Messages defined'+ENTER);

    A := Template;
    A := StringReplace(A, '<ENV>', {$IFDEF DESIGN_FMX}'FMX'{$ELSE}'Vcl'{$ENDIF}, [rfReplaceAll]);
    A := StringReplace(A, '<UNIT>', ExtractFileName(StmUnit), []);
    A := StringReplace(A, '<TIMESTAMP>', DateTimeToStr(Now), []);
    A := StringReplace(A, '<USES>', Dam.Owner.UnitName, []);
    A := StringReplace(A, '<DECLARATIONS>', aDeclarations.ToString, []);
    A := StringReplace(A, '<FUNCTIONS>', aImplementations.ToString, []);
  finally
    aDeclarations.Free;
    aImplementations.Free;
  end;

  S := TStringList.Create;
  try
    S.Text := A;
    S.SaveToFile(Dir + StmUnit + '.pas');
  finally
    S.Free;
  end;
end;

end.
