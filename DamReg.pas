unit DamReg;

interface

uses DesignEditors;

type
  TDamPropEdit = class(TComponentEditor)
  public
    procedure Edit; override;
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

procedure Register;

implementation

uses DamUnit, DamList, Vcl.Forms, System.Classes,
  DesignIntf;

procedure Register;
begin
    RegisterComponents('Digao', [TDam]);

    RegisterNoIcon([TDamMsg]);

    RegisterComponentEditor(TDam, TDamPropEdit);
end;

//

procedure TDamPropEdit.Edit;
var I: Integer;
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
