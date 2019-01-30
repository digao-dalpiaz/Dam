program DamExample;

uses
  Vcl.Forms,
  UFrmExample in 'UFrmExample.pas' {FrmExample};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmExample, FrmExample);
  Application.Run;
end.
