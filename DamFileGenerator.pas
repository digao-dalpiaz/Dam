unit DamFileGenerator;

interface

uses DamUnit;

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
  aFile: string;
  StmUnit: string;
  Msg: TDamMsg;
  S: TStringList;
  A, aFuncName, aFuncKind, aResFunc, aPreCmd, aCmd, aEv, aPar1, aPar2, aCab, aTxt: string;
  aDecs, Func: string;
  aTime: string;
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

  aFile := StmUnit + '.pas';

  aTime := DateTimeToStr(Now);

  for C in Dam.Owner do
  if C is TDamMsg then
  begin
    Msg := TDamMsg(C);
    if (Msg.Dam.DamUnitName = StmUnit) then
    begin
      A := Msg.Name;
      if A[1] = '_' then Delete(A, 1, 1);

      aFuncName := A;
      aCmd := Dam.Owner.Name+'.'+Msg.Name;

      aPar1 := '';
      aPar2 := '';
      if Pos('%p', Msg.Message) > 0 then
      begin
        aPar1 := '(Params: TDamParams)';
        aPar2 := '(Params)';
      end;

      if (Msg.RaiseExcept) or (Msg.Buttons in [dbOK, dbOne]) then
      begin
        aFuncKind := 'procedure';
        aResFunc := '';
        aPreCmd := '';
        aEv := 'Run';
      end else
      if (Msg.Buttons in [dbYesNo, dbTwo]) then
      begin
        aFuncKind := 'function';
        aResFunc := ': Boolean';
        aPreCmd := 'Result := ';
        aEv := 'RunAsBool';
      end else
      begin
        aFuncKind := 'function';
        aResFunc := ': TDamMsgRes';
        aPreCmd := 'Result := ';
        aEv := 'Run';
      end;

      aCab := aFuncKind+' '+aFuncName+aPar1+aResFunc+';';
      aTxt := aCab+ENTER+
              'begin'+ENTER+
              '  '+aPreCmd+aCmd+'.'+aEv+aPar2+';'+ENTER+
              'end;'+ENTER;

      aDecs := aDecs + aCab + ENTER;
      Func := Func + aTxt + ENTER;
    end;
  end;

  A := Template;
  A := StringReplace(A, '<UNIT>', StmUnit, []);
  A := StringReplace(A, '<TIMESTAMP>', aTime, []);
  A := StringReplace(A, '<USES>', Dam.Owner.UnitName, []);
  A := StringReplace(A, '<DECLARATIONS>', aDecs, []);
  A := StringReplace(A, '<FUNCTIONS>', Func, []);

  S := TStringList.Create;
  try
    S.Text := A;
    S.SaveToFile(Dir + aFile);
  finally
    S.Free;
  end;
end;

end.
