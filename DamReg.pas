unit DamReg;

interface

uses DesignEditors;

type
  TDamPropEdit = class(TComponentEditor)
  public
    procedure Edit; override;
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

end.
