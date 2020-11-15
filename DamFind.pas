unit DamFind;

interface

uses
{$IFDEF FPC}
  Forms, StdCtrls, Controls, ExtCtrls, Classes,
{$ELSE}
  Vcl.Forms, Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls, System.Classes,
{$ENDIF}
  DamUnit;

type
  TFrmDamFind = class(TForm)
    Label1: TLabel;
    EdText: TEdit;
    L: TListBox;
    BoxDIO: TPanel;
    BtnCancel: TButton;
    BtnOK: TButton;
    EdMessage: TMemo;
    procedure EdTextChange(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure LClick(Sender: TObject);
    procedure LDblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    Dam: TDam;
  end;

var
  FrmDamFind: TFrmDamFind;

function DoFindDamMessage(objDam: TDam; out objMsg: TDamMsg): Boolean;

implementation

{$R *.dfm}

uses
{$IFDEF FPC}
  SysUtils, Dialogs;
{$ELSE}
  System.SysUtils, Vcl.Dialogs;
{$ENDIF}

function DoFindDamMessage(objDam: TDam; out objMsg: TDamMsg): Boolean;
begin
  FrmDamFind := TFrmDamFind.Create(Application);
  try
    FrmDamFind.Dam := objDam;
    Result := FrmDamFind.ShowModal = mrOk;
    if Result then
      objMsg := TDamMsg(FrmDamFind.L.Items.Objects[FrmDamFind.L.ItemIndex]);
  finally
    FrmDamFind.Free;
  end;
end;

//

procedure TFrmDamFind.FormShow(Sender: TObject);
begin
  //Anchors must be defined on show due to Lazarus incorrect form size behavior
  EdText.Anchors := [akLeft,akRight,akTop];
  L.Anchors := [akLeft,akRight,akTop,akBottom];
  EdMessage.Anchors := [akLeft,akRight,akBottom];
  BoxDIO.Anchors := [akBottom];
end;

procedure TFrmDamFind.EdTextChange(Sender: TObject);
var
  C: TComponent;
  Msg: TDamMsg;
  FindText, MsgName: string;
begin
  L.Items.BeginUpdate;
  try
    L.Items.Clear;

    FindText := UpperCase(EdText.Text);
    if FindText=EmptyStr then Exit;

    for C in Dam.Owner do
    begin
      if C.GetParentComponent = Dam then
      begin
        Msg := TDamMsg(C); //should be TDamMsg object!

        if UpperCase(Msg.Name).Contains(FindText)
         or UpperCase(Msg.Message).Contains(FindText) then
        begin
          MsgName := Msg.Name;
          if MsgName.StartsWith('_') then Delete(MsgName, 1, 1);

          L.AddItem(MsgName, Msg);
        end;
      end;
    end;

    if L.Items.Count>0 then
      L.ItemIndex := 0; //select first item
  finally
    L.Items.EndUpdate;
    LClick(nil);
  end;
end;

procedure TFrmDamFind.LClick(Sender: TObject);
var Msg: TDamMsg;
begin
  if L.ItemIndex<>-1 then
  begin
    Msg := TDamMsg(L.Items.Objects[L.ItemIndex]);
    EdMessage.Text := Msg.Message;
  end else
    EdMessage.Text := EmptyStr;
end;

procedure TFrmDamFind.LDblClick(Sender: TObject);
begin
  BtnOK.Click;
end;

procedure TFrmDamFind.BtnOKClick(Sender: TObject);
begin
  if L.ItemIndex=-1 then
  begin
    MessageDlg('Please, select one message!', mtError, [mbOK], 0);
    Exit;
  end;

  ModalResult := mrOk;
end;

end.
