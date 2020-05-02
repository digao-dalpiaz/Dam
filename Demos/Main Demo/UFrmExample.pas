unit UFrmExample;

interface

uses Vcl.Forms, Vcl.StdCtrls, Vcl.Controls, DamUnit, System.Classes;

type
  TFrmExample = class(TForm)
    Dam1: TDam;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    _InfoGoodJob: TDamMsg;
    Button4: TButton;
    Label1: TLabel;
    EdName: TEdit;
    _QuestionSaveFile: TDamMsg;
    Button5: TButton;
    Button6: TButton;
    _RaiseLoadingFile: TDamMsg;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  end;

var
  FrmExample: TFrmExample;

implementation

{$R *.dfm}

uses MsgDialogs, System.SysUtils;

{$IF CompilerVersion < 29} //below Delphi XE8
  {$DEFINE USE_ARRAY}
{$ENDIF}

procedure TFrmExample.Button1Click(Sender: TObject);
begin
{$IFDEF USE_ARRAY}
  MsgInfo('This is the information number %p!', DamParams([100]));
{$ELSE}
  MsgInfo('This is the information number %p!', [100]);
{$ENDIF}
end;

procedure TFrmExample.Button2Click(Sender: TObject);
begin
  if MsgQuest('Do you want to continue this function now?') then
    MsgInfo('Sucess')
  else
    MsgError('Aborted');
end;

procedure TFrmExample.Button3Click(Sender: TObject);
begin
  InfoGoodJob;
end;

procedure TFrmExample.Button4Click(Sender: TObject);
begin
{$IFDEF USE_ARRAY}
  if QuestionSaveFile(DamParams([EdName.Text, DateTimeToStr(Now)])) then
{$ELSE}
  if QuestionSaveFile([EdName.Text, DateTimeToStr(Now)]) then
{$ENDIF}
    MsgInfo('File saved')
  else
    MsgError('File not saved');
end;

procedure TFrmExample.Button5Click(Sender: TObject);
begin
{$IFDEF USE_ARRAY}
  MsgRaise('Fatal error while <b>%p</b>', DamParams(['writing file']));
{$ELSE}
  MsgRaise('Fatal error while <b>%p</b>', ['writing file']);
{$ENDIF}
end;

procedure TFrmExample.Button6Click(Sender: TObject);
begin
{$IFDEF USE_ARRAY}
  RaiseLoadingFile(DamParams(['c:\test.txt']));
{$ELSE}
  RaiseLoadingFile(['c:\test.txt']);
{$ENDIF}
end;

end.
