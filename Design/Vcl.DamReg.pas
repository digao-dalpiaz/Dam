{$IFNDEF DESIGN_FMX}unit Vcl.DamReg;{$ENDIF}

{$IFDEF DESIGN_FMX}
  {$IF CompilerVersion >= 26} //XE5
    {$DEFINE USE_FMX_NEW_UNITS}
  {$ENDIF}
{$ENDIF}

interface

procedure Register;

implementation

uses
{$IFDEF FPC}
  Forms, Classes, LResources, ComponentEditors
{$ELSE}
  Vcl.Forms, System.Classes, DesignIntf, DesignEditors
  {$IFDEF DESIGN_FMX}
    {$IFDEF USE_FMX_NEW_UNITS}
    , FMX.Controls
    {$ELSE}
    , FMX.Types
    {$ENDIF}
  {$ELSE}
  , Vcl.Controls
  {$ENDIF}
{$ENDIF}
{$IFDEF DESIGN_FMX}
  , FMX.DamUnit, FMX.DamList
{$ELSE}
  , Vcl.DamUnit, Vcl.DamList
{$ENDIF};

type
  TDamPropEdit = class(TComponentEditor)
  public
    procedure Edit; override;
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

{$IFDEF DCC}
procedure RegisterEnv(C: TPersistentClass);
begin
  GroupDescendentsWith(C,
    {$IFDEF DESIGN_FMX}
    FMX.
      {$IFDEF USE_FMX_NEW_UNITS}
      Controls
      {$ELSE}
      Types
      {$ENDIF}
    {$ELSE}
    Vcl.Controls
    {$ENDIF}.TControl);
end;
{$ENDIF}

procedure Register;
begin
  {$IFDEF FPC}
    {$I Dam.lrs}
  {$ELSE}
    RegisterEnv(TDam);
    RegisterEnv(TDamMsg);
  {$ENDIF}

  RegisterComponents('Digao', [TDam]);
  RegisterNoIcon([TDamMsg]);
  RegisterComponentEditor(TDam, TDamPropEdit);

  try
    {$IFNDEF FPC}System.{$ENDIF}Classes.RegisterClass(TDamMsg);
  except
  end;
end;

//

procedure TDamPropEdit.Edit;
var
  I: Integer;
  F: TFrmDamList;
begin
  F := nil;

  for I := 0 to Screen.FormCount-1 do
  begin
    if Screen.Forms[I] is TFrmDamList then
      if TFrmDamList(Screen.Forms[I]).GetDam = Component then
      begin
        F := TFrmDamList(Screen.Forms[I]);
        Break;
      end;
  end;

  if F=nil then
    F := TFrmDamList.Create(Component, Designer);
  F.Show;
end;

procedure TDamPropEdit.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: Edit;
  end;
end;

function TDamPropEdit.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Manage Dam Messages';
  end;
end;

function TDamPropEdit.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
