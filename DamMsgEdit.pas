unit DamMsgEdit;

interface

uses
{$IFDEF FPC}
  Forms, Buttons, StdCtrls, ExtCtrls, ColorBox, Controls,
{$ELSE}
  Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Controls,
  Vcl.Buttons, System.Classes,
{$ENDIF}
  //
  DamUnit, DzHTMLText;

type
  TFrmDamMsgEdit = class(TForm)
    BtnOK: TBitBtn;
    BtnCancel: TBitBtn;
    EdNome: TEdit;
    W5: TShape;
    W6: TShape;
    W4: TShape;
    W2: TShape;
    W3: TShape;
    W1: TShape;
    BtnBold: TSpeedButton;
    BtnItalic: TSpeedButton;
    BtnUnderline: TSpeedButton;
    BtnDoFont: TSpeedButton;
    BtnDoSize: TSpeedButton;
    BtnDoFontColor: TSpeedButton;
    BtnDoBgColor: TSpeedButton;
    LbFontColor: TLabel;
    LbBgColor: TLabel;
    BtnCenter: TSpeedButton;
    BtnRight: TSpeedButton;
    BtnDoAnyColor: TSpeedButton;
    EdSize: TComboBox;
    Box: TScrollBox;
    BtnPreview: TBitBtn;
    LbMsg: TDzHTMLText;
    BtnLeft: TSpeedButton;
    BoxQuickButtons: TFlowPanel;
    tInfo: TSpeedButton;
    tQuest: TSpeedButton;
    tWarn: TSpeedButton;
    tError: TSpeedButton;
    tRaise: TSpeedButton;
    BtnDoLink: TSpeedButton;
    EdFont: TComboBox;
    LbAnyColor: TLabel;
    EdFontColor: TColorBox;
    EdBgColor: TColorBox;
    EdAnyColor: TColorBox;
    Label1: TLabel;
    BtnHelp: TBitBtn;
    BtnParameter: TSpeedButton;
    BtnExceptPar: TSpeedButton;
    M: TMemo;
    procedure MChange(Sender: TObject);
    procedure BtnBoldClick(Sender: TObject);
    procedure BtnItalicClick(Sender: TObject);
    procedure BtnUnderlineClick(Sender: TObject);
    procedure BtnDoSizeClick(Sender: TObject);
    procedure BtnDoFontColorClick(Sender: TObject);
    procedure tInfoClick(Sender: TObject);
    procedure BtnPreviewClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure BtnDoBgColorClick(Sender: TObject);
    procedure BtnDoAnyColorClick(Sender: TObject);
    procedure BtnCenterClick(Sender: TObject);
    procedure BtnRightClick(Sender: TObject);
    procedure BtnDoLinkClick(Sender: TObject);
    procedure BtnLeftClick(Sender: TObject);
    procedure BtnDoFontClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EdFontDropDown(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure BtnParameterClick(Sender: TObject);
    procedure BtnExceptParClick(Sender: TObject);
  public
    Dam: TDam;
    DamMsg: TDamMsg;
    procedure StoreComp(Target: TDamMsg);
  private
    procedure PutSelText(TagOpen: string; TagClose: string='');
    procedure SetBtn(C: TDamMsg);
  end;

var
  FrmDamMsgEdit: TFrmDamMsgEdit;

implementation

{$R *.dfm}

uses
{$IFDEF FPC}
  Graphics, SysUtils, LCLIntf;
{$ELSE}
  Vcl.Graphics, System.SysUtils,
  Winapi.Windows, Winapi.Messages, Winapi.ShellAPI;
{$ENDIF}

procedure TFrmDamMsgEdit.FormCreate(Sender: TObject);
begin
  LbMsg.Text := '';
end;

procedure TFrmDamMsgEdit.FormShow(Sender: TObject);
var A: string;
    V: Boolean;
begin
  EdFont.Text := Dam.MessageFont.Name;
  EdSize.Text := IntToStr(Dam.MessageFont.Size);

  Box.Color := Dam.MessageColor;
  LbMsg.Font.Assign(Dam.MessageFont);
  LbMsg.Images := Dam.Images;

  if DamMsg <> nil then
  begin
      A := DamMsg.Name;
      if A[1] = '_' then Delete(A, 1, 1);
      EdNome.Text := A;

      M.Text := DamMsg.Message;
      //MChange(nil);

      V := (DamMsg.Button1 = '')
       and (DamMsg.Button2 = '')
       and (DamMsg.Button3 = '')
       and (DamMsg.CustomTitle = '')
       and (DamMsg.CustomIcon.Empty)
       and (DamMsg.Title = dtByIcon);

      tRaise.Down := (V)
        and (DamMsg.RaiseExcept)
        and (DamMsg.Icon = diError)
        and (DamMsg.Buttons = dbOK);

      tInfo.Down := (V)
        and (not DamMsg.RaiseExcept)
        and (DamMsg.Icon = diInfo)
        and (DamMsg.Buttons = dbOK);

      tWarn.Down := (V)
        and (not DamMsg.RaiseExcept)
        and (DamMsg.Icon = diWarn)
        and (DamMsg.Buttons = dbOK);

      tQuest.Down := (V)
        and (not DamMsg.RaiseExcept)
        and (DamMsg.Icon = diQuest)
        and (DamMsg.Buttons = dbYesNo);

      tError.Down := (V)
        and (not DamMsg.RaiseExcept)
        and (DamMsg.Icon = diError)
        and (DamMsg.Buttons = dbOK);
  end;
end;

procedure TFrmDamMsgEdit.EdFontDropDown(Sender: TObject);
begin
  if EdFont.Items.Count=0 then
    EdFont.Items.Assign(Screen.Fonts);
end;

procedure TFrmDamMsgEdit.MChange(Sender: TObject);
begin
  LbMsg.Text := M.Text;
end;

function CorToStr(C: TColor): string;
begin
  Result := ColorToString(C);
  if Result.StartsWith('$00') then Delete(Result, 2, 2);
end;

procedure TFrmDamMsgEdit.PutSelText(TagOpen: string; TagClose: string='');
begin
  if TagClose='' then TagClose := TagOpen;

  M.SelText := '<'+TagOpen+'>'+M.SelText+'</'+TagClose+'>';
end;

procedure TFrmDamMsgEdit.BtnBoldClick(Sender: TObject);
begin
  PutSelText('b');
end;

procedure TFrmDamMsgEdit.BtnItalicClick(Sender: TObject);
begin
  PutSelText('i');
end;

procedure TFrmDamMsgEdit.BtnUnderlineClick(Sender: TObject);
begin
  PutSelText('u');
end;

procedure TFrmDamMsgEdit.BtnLeftClick(Sender: TObject);
begin
  PutSelText('l');
end;

procedure TFrmDamMsgEdit.BtnCenterClick(Sender: TObject);
begin
  PutSelText('c');
end;

procedure TFrmDamMsgEdit.BtnRightClick(Sender: TObject);
begin
  PutSelText('r');
end;

procedure TFrmDamMsgEdit.BtnDoFontClick(Sender: TObject);
begin
  PutSelText('fn:'+EdFont.Text, 'fn');
end;

procedure TFrmDamMsgEdit.BtnDoSizeClick(Sender: TObject);
begin
  PutSelText('fs:'+EdSize.Text, 'fs');
end;

procedure TFrmDamMsgEdit.BtnDoFontColorClick(Sender: TObject);
begin
  PutSelText('fc:'+CorToStr(EdFontColor.Selected), 'fc');
end;

procedure TFrmDamMsgEdit.BtnDoBgColorClick(Sender: TObject);
begin
  PutSelText('bc:'+CorToStr(EdBgColor.Selected), 'bc');
end;

procedure TFrmDamMsgEdit.BtnDoAnyColorClick(Sender: TObject);
begin
  M.SelText := CorToStr(EdAnyColor.Selected);
end;

procedure TFrmDamMsgEdit.BtnDoLinkClick(Sender: TObject);
begin
  PutSelText('a');
end;

procedure TFrmDamMsgEdit.BtnParameterClick(Sender: TObject);
begin
  M.SelText := DAM_PARAM_IDENT;
end;

procedure TFrmDamMsgEdit.BtnExceptParClick(Sender: TObject);
begin
  M.SelText := DAM_PARAM_EXCEPTION;
end;

procedure ClearMsg(Msg: TDamMsg);
var Def: TDamMsg;
    FocoInvertido: Boolean;
    AMsg: string;
begin
  Def := TDamMsg.Create(nil);
  try
    AMsg := Msg.Message;
    FocoInvertido := Msg.SwapFocus;

    Msg.Assign(Def); //get default properties

    Msg.Message := AMsg;
    Msg.SwapFocus := FocoInvertido;
  finally
    Def.Free;
  end;
end;

procedure TFrmDamMsgEdit.tInfoClick(Sender: TObject);
var aTipo: string;
    aNome: string;

  function CheckStartWith(Btn: TSpeedButton): Boolean;
  var Prefix: string;
  begin
    Result := False;

    Prefix := Btn.Caption;
    if aNome.StartsWith(Prefix) then
    begin
        Result := True;
        Delete(aNome, 1, Length(Prefix));
    end;
  end;

begin
  aTipo := TSpeedButton(Sender).Caption;
  aNome := EdNome.Text;

  if not CheckStartWith(tInfo) then
  if not CheckStartWith(tQuest) then
  if not CheckStartWith(tWarn) then
  if not CheckStartWith(tError) then
  if not CheckStartWith(tRaise) then {};

  EdNome.Text := aTipo+aNome;
end;

procedure TFrmDamMsgEdit.SetBtn(C: TDamMsg);
begin
  if tInfo.Down then
  begin
    ClearMsg(C);
    C.Icon := diInfo;
  end else

  if tWarn.Down then
  begin
    ClearMsg(C);
    C.Icon := diWarn;
  end else

  if tError.Down then
  begin
    ClearMsg(C);
    C.Icon := diError;
  end else

  if tQuest.Down then
  begin
    ClearMsg(C);
    C.Icon := diQuest;
    C.Buttons := dbYesNo;
  end else

  if tRaise.Down then
  begin
    ClearMsg(C);
    C.Icon := diError;
    C.RaiseExcept := True;
  end;
end;

procedure TFrmDamMsgEdit.BtnPreviewClick(Sender: TObject);
var C: TDamMsg;
begin
  C := TDamMsg.Create(nil);
  try
    C.Dam := Dam;

    if DamMsg<>nil then C.Assign(DamMsg);

    C.Message := M.Text;
    SetBtn(C);

    C.Preview;
  finally
    C.Free;
  end;
end;

procedure TFrmDamMsgEdit.BtnOKClick(Sender: TObject);
var A: string;
begin
  EdNome.Text := Trim(EdNome.Text);

  if EdNome.Text = '' then
    raise Exception.Create('Please, specify a name');

  A := '_' + EdNome.Text;

  if not ((DamMsg <> nil) and (DamMsg.Name = A)) then
    if Dam.Owner.FindComponent(A) <> nil then
      raise Exception.Create(EdNome.Text + ' already exists');

  EdNome.Text := A;
  ModalResult := mrOk;
end;

procedure TFrmDamMsgEdit.StoreComp(Target: TDamMsg);
begin
  Target.Name := EdNome.Text;
  Target.Message := M.Text;
  SetBtn(Target);
end;

procedure TFrmDamMsgEdit.BtnHelpClick(Sender: TObject);
const URL = 'https://github.com/digao-dalpiaz/Dam';
begin
{$IFDEF FPC}
  OpenURL(URL);
{$ELSE}
  ShellExecute(0, '', URL, '', '', SW_SHOWNORMAL);
{$ENDIF}
end;

end.
