unit DamList;

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses
{$IFDEF FPC}
  Forms, StdCtrls, Controls, ExtCtrls, Buttons, ActnList, ComCtrls,
  Types, Classes,
  PropEdits, ComponentEditors, FGL,
{$ELSE}
  DesignWindows, System.Classes, System.Actions, Vcl.ActnList,
  Vcl.ImgList, Vcl.Controls, Vcl.ComCtrls, Vcl.StdCtrls,
  Vcl.Buttons, Vcl.ExtCtrls, 
  {$IF CompilerVersion >= 29}System.ImageList, {$ENDIF}
  //
  System.UITypes, System.Types, Vcl.Forms, Winapi.Messages,
  DesignIntf, System.Generics.Collections,
{$ENDIF}
  DamUnit;

type
  {$IFDEF FPC}
  TList<T> = class(TFPGList<T>);
  {$ELSE}
  TPersistentSelectionList = class(TDesignerSelections);
  {$ENDIF}

  TFrmDamList = class({$IFDEF FPC}TForm{$ELSE}TDesignWindow{$ENDIF})
    L: TListBox;
    IL: TImageList;
    IB: TImageList;
    IP: TImageList;
    BoxButtons: TPanel;
    RzBorder1: TBevel;
    RzBorder2: TBevel;
    RzBorder3: TBevel;
    BtnAdd: TBitBtn;
    BtnDel: TBitBtn;
    BtnPreview: TBitBtn;
    BtnCopy: TBitBtn;
    BtnPaste: TBitBtn;
    BtnCut: TBitBtn;
    BtnUp: TBitBtn;
    BtnDown: TBitBtn;
    BtnBuildUnit: TBitBtn;
    BtnHide: TBitBtn;
    BtnAddWizard: TBitBtn;
    BtnEdit: TBitBtn;
    ActionList: TActionList;
    Action_Del: TAction;
    MUnit: TMemo;
    StatusBar: TStatusBar;
    BtnFind: TBitBtn;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BtnAddClick(Sender: TObject);
    procedure LClick(Sender: TObject);
    procedure BtnDelClick(Sender: TObject);
    procedure BtnPreviewClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure FormResize(Sender: TObject);
    procedure BtnCutClick(Sender: TObject);
    procedure BtnCopyClick(Sender: TObject);
    procedure BtnPasteClick(Sender: TObject);
    procedure BtnBuildUnitClick(Sender: TObject);
    procedure BtnHideClick(Sender: TObject);
    procedure BtnAddWizardClick(Sender: TObject);
    procedure BtnEditClick(Sender: TObject);
    procedure LDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Action_DelExecute(Sender: TObject);
    procedure BtnUpOrDownClick(Sender: TObject);
    procedure BtnFindClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    Dam: TDam;
    Own: TComponent;
    Sel: TList<TObject>; //selected objects
    {$IFDEF FPC}
    FDesigner: TComponentEditorDesigner;
    {$ENDIF}

    Freeze: Boolean; //freeze list update

    procedure FillList;
    procedure UpdSelection;
    procedure UpdButtons;

    procedure DoModified;
    procedure DoSelectObject(C: TDamMsg);
    procedure MessageObjectAdded(C: TDamMsg);
    function GetFirstSel: TDamMsg;
    function SelContains(Obj: TObject): Boolean;

    {$IFNDEF FPC}
    procedure WMClipboardUpdate(var Msg: TMessage); message WM_CLIPBOARDUPDATE;
    procedure UpdClipboard;
    {$ENDIF}

    {$IFDEF FPC}
    procedure RegisterGlobalDesignHook;

    procedure OnSetSelection(const ASelection: TPersistentSelectionList);
    procedure OnModified(Sender: TObject);
    procedure OnDeleting(Item: TPersistent);
    {$ENDIF}
  public
    constructor Create(aComp: TComponent; aDesign: {$IFDEF FPC}TComponentEditorDesigner{$ELSE}IDesigner{$ENDIF}); reintroduce;
    function GetDam: TDam;
    {$IFDEF FPC}
    //Lazarus Designer replacement (cannot declare Designer field because already exists)
    property Designer: TComponentEditorDesigner read FDesigner write FDesigner;
    {$ELSE}
    procedure DesignerClosed(const Designer: IDesigner; AGoingDormant: Boolean); override;
    procedure ItemsModified(const Designer: IDesigner); override;
    procedure ItemDeleted(const ADesigner: IDesigner; Item: TPersistent); override;
    procedure SelectionChanged(const ADesigner: IDesigner; const ASelection: IDesignerSelections); override;
    {$ENDIF}
  end;

implementation

{$R *.dfm}

uses
{$IFDEF FPC}
  StrUtils, SysUtils, Graphics, Menus, LCLType, IDEWindowIntf,
{$ELSE}
  System.SysUtils, System.StrUtils, Vcl.Graphics, Winapi.Windows,
  Vcl.Clipbrd, System.Win.Registry,
{$ENDIF}
  DamMsgEdit, DamFind, DamFileGenerator;

//

{$IFNDEF FPC}
const REG_PATH = 'Digao\Dam';

procedure SaveFormPos(Reg: TRegistry; F: TForm);
var WP: TWindowPlacement;
begin
  WP.Length := SizeOf( TWindowPlacement );
  GetWindowPlacement( F.Handle, @WP );

  if Reg.OpenKey('Window', True) then //should return always true !
  begin
    Reg.WriteInteger('X', WP.rcNormalPosition.Left);
    Reg.WriteInteger('Y', WP.rcNormalPosition.Top);
    Reg.WriteInteger('W', WP.rcNormalPosition.Width);
    Reg.WriteInteger('H', WP.rcNormalPosition.Height);

    Reg.WriteBool('Max', (F.WindowState=wsMaximized) );
  end;
end;

procedure LoadFormPos(Reg: TRegistry; F: TForm);
begin
  if Reg.OpenKeyReadOnly('Window') then //only if was saved before
  begin
    F.Left := Reg.ReadInteger('X');
    F.Top := Reg.ReadInteger('Y');
    F.Width := Reg.ReadInteger('W');
    F.Height := Reg.ReadInteger('H');

    if Reg.ReadBool('Max') then
      F.WindowState := wsMaximized;

    F.Position := poDesigned;
  end;
end;
{$ENDIF}

//

{$IFDEF FPC}
procedure TFrmDamList.RegisterGlobalDesignHook;
begin
  if Assigned(GlobalDesignHook) then
  begin
    GlobalDesignHook.RemoveAllHandlersForObject(Self);

    GlobalDesignHook.AddHandlerSetSelection(OnSetSelection);
    GlobalDesignHook.AddHandlerModified(OnModified);
    GlobalDesignHook.AddHandlerPersistentDeleting(OnDeleting);
  end;
end;
{$ENDIF}

constructor TFrmDamList.Create(aComp: TComponent; aDesign: {$IFDEF FPC}TComponentEditorDesigner{$ELSE}IDesigner{$ENDIF});
begin
  inherited Create(Application);

  Dam := TDam(aComp);
  Own := Dam.Owner;

  Designer := aDesign;

  Sel := TList<TObject>.Create;

  FillList;
end;

procedure TFrmDamList.FormCreate(Sender: TObject);
{$IFDEF FPC}
  procedure ShowButtonsGlyph;
  var I: Integer;
  begin
    for I := 0 to BoxButtons.ControlCount-1 do
    begin
      if BoxButtons.Controls[I] is TBitBtn then
        TBitBtn(BoxButtons.Controls[I]).GlyphShowMode := gsmAlways;
    end;
  end;
{$ENDIF}
begin
  {$IFDEF FPC}
  RegisterGlobalDesignHook;

  StatusBar.SimplePanel := False;

  ShowButtonsGlyph;

  BtnCut.Visible := False;
  BtnCopy.Visible := False;
  BtnPaste.Visible := False;
  {$ENDIF}

  {$IFNDEF FPC}
  UpdClipboard;
  AddClipboardFormatListener(Handle); //set clipboard monitoring
  {$ENDIF}
end;

procedure TFrmDamList.FormDestroy(Sender: TObject);
begin
  {$IFNDEF FPC}
  RemoveClipboardFormatListener(Handle); //remove clipboard monitoring
  {$ENDIF}

  {$IFDEF FPC}
  if Assigned(GlobalDesignHook) then
    GlobalDesignHook.RemoveAllHandlersForObject(Self);
  {$ELSE}
  Designer := nil;
  {$ENDIF}

  Sel.Free;
end;

procedure TFrmDamList.FormShow(Sender: TObject);
{$IFNDEF FPC}
var Reg: TRegistry;
{$ENDIF}
begin
  //Anchors must be defined on show due to Lazarus incorrect form size behavior
  L.Anchors := [akLeft,akRight,akTop,akBottom];
  BoxButtons.Anchors := [akRight,akTop];

  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;

  {$IFDEF FPC}
  IDEDialogLayoutList.ApplyLayout(Self);
  {$ELSE}
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    Reg.OpenKey(REG_PATH, True);
    LoadFormPos(Reg, Self);
  finally
    Reg.Free;
  end;
  {$ENDIF}
end;

procedure TFrmDamList.FormClose(Sender: TObject; var Action: TCloseAction);
{$IFNDEF FPC}
var Reg: TRegistry;
{$ENDIF}
begin
  Action := caFree;

  {$IFDEF FPC}
  IDEDialogLayoutList.SaveLayout(Self);
  {$ELSE}
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    Reg.OpenKey(REG_PATH, True);
    SaveFormPos(Reg, Self);
  finally
    Reg.Free;
  end;
  {$ENDIF}
end;

procedure TFrmDamList.FormResize(Sender: TObject);
begin
  L.Invalidate;
end;

{$IFNDEF FPC}
procedure TFrmDamList.UpdClipboard;
begin
  BtnPaste.Enabled := Designer.CanPaste;
end;

procedure TFrmDamList.WMClipboardUpdate(var Msg: TMessage);
begin
  //this method is fired when clipboard has changed

  UpdClipboard;
end;
{$ENDIF}

function TFrmDamList.GetDam: TDam;
begin
  Result := Dam;
end;

procedure TFrmDamList.DoModified;
begin
  {$IFDEF FPC}
  GlobalDesignHook.Modified(nil);
  {$ELSE}
  Designer.Modified;
  {$ENDIF}
end;

procedure TFrmDamList.DoSelectObject(C: TDamMsg);
begin
  {$IFDEF FPC}
  GlobalDesignHook.SelectOnlyThis(C);
  {$ELSE}
  Designer.SelectComponent(C);
  {$ENDIF}
end;

function TFrmDamList.SelContains(Obj: TObject): Boolean;
begin
  Result := Sel.IndexOf(Obj) <> -1;
end;

function TFrmDamList.GetFirstSel: TDamMsg;
begin
  Result := TDamMsg(Sel[0]);
end;

procedure TFrmDamList.FillList;
var C: TComponent;
    TopIdx: Integer;
begin
  if Freeze then Exit;
  //

  Caption := Dam.Name;

  L.Items.BeginUpdate;
  try
    TopIdx := L.TopIndex;
    L.Items.Clear;

    for C in Own do
      if C.GetParentComponent = Dam then L.Items.AddObject(string.Empty, C);

    L.TopIndex := TopIdx;
    UpdSelection;
  finally
    L.Items.EndUpdate;
  end;
end;

procedure TFrmDamList.UpdSelection;
var I: Integer;
begin
  if Freeze then Exit;

  L.Items.BeginUpdate;
  try
    for I := 0 to L.Count-1 do
      L.Selected[I] := SelContains(L.Items.Objects[I]);
  finally
    L.Items.EndUpdate;
  end;

  UpdButtons;
end;

procedure TFrmDamList.UpdButtons;
var SelOne, SelVarious: Boolean;
begin
  SelOne := (L.SelCount = 1) and (L.SelCount = Sel.Count);
  SelVarious := (L.SelCount > 0) and (L.SelCount = Sel.Count);

  BtnDel.Enabled := SelVarious;
  BtnPreview.Enabled := SelOne;
  BtnEdit.Enabled := SelOne;

  BtnCut.Enabled := SelVarious;
  BtnCopy.Enabled := SelVarious;

  //Lazarus ListBox.Selected has a strange behavior resulting incorrect flag state
  BtnUp.Enabled := SelVarious and not SelContains(L.Items.Objects[0]); //(not L.Selected[0]);
  BtnDown.Enabled := SelVarious and not SelContains(L.Items.Objects[L.Count-1]); //(not L.Selected[L.Count-1]);

  BtnBuildUnit.Enabled := (Dam.DamUnitName<>string.Empty);
  BtnHide.Enabled := SelVarious;

  BtnFind.Enabled := L.Count>0;

  StatusBar.Panels[0].Text := IfThen(Dam.HandleExceptions, 'Exceptions');
  StatusBar.Panels[1].Text := IfThen(Dam.DamDefault, 'Default');
  StatusBar.Panels[2].Text := Dam.DamUnitName;
  StatusBar.Panels[3].Text := Format('%d/%d', [L.SelCount, L.Count]);
end;

{$IFDEF FPC}
procedure TFrmDamList.OnModified(Sender: TObject);
{$ELSE}
procedure TFrmDamList.ItemsModified(const Designer: IDesigner);
{$ENDIF}
begin
  FillList;
end;

{$IFDEF FPC}
procedure TFrmDamList.OnDeleting(Item: TPersistent);
{$ELSE}
procedure TFrmDamList.ItemDeleted(const ADesigner: IDesigner; Item: TPersistent);
{$ENDIF}
var Idx: Integer;
begin
  if Item=Dam then Close else
  begin
      if Freeze then Exit; //deleted by this form

      //**if re-fill the list, result in very slow deletion when lot of objects
      Idx := L.Items.IndexOfObject(Item);
      if Idx<>(-1) then L.Items.Delete(Idx);
  end;
end;

{$IFNDEF FPC}
procedure TFrmDamList.DesignerClosed(const Designer: IDesigner; AGoingDormant: Boolean);
begin
  Close;
end;
{$ENDIF}

{$IFDEF FPC}
procedure TFrmDamList.OnSetSelection(const ASelection: TPersistentSelectionList);
{$ELSE}
procedure TFrmDamList.SelectionChanged(const ADesigner: IDesigner;
    const ASelection: IDesignerSelections);
{$ENDIF}
var I: Integer;
begin
  Sel.Clear;
  for I := 0 to ASelection.Count-1 do
    Sel.Add(ASelection[I]);

  UpdSelection;
end;

procedure TFrmDamList.LClick(Sender: TObject);
var I: Integer;
    tmpSel: TPersistentSelectionList;
begin
  Sel.Clear;
  for I := 0 to L.Count-1 do
  begin
    if L.Selected[I] then
      Sel.Add(L.Items.Objects[I]);
  end;

  tmpSel := TPersistentSelectionList.Create;
  try
    for I := 0 to Sel.Count-1 do
      tmpSel.Add(TPersistent(Sel[I]));

    Freeze := True;
    try
      {$IFDEF FPC}
      GlobalDesignHook.SetSelection(tmpSel);
      {$ELSE}
      Designer.SetSelections(tmpSel);
      {$ENDIF}
    finally
      Freeze := False;
    end;

  finally
    tmpSel.Free;
  end;

  UpdButtons;
end;

procedure TFrmDamList.LDblClick(Sender: TObject);
begin
  if BtnEdit.Enabled then BtnEdit.Click;
end;

procedure TFrmDamList.LDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
  State: TOwnerDrawState);

var B: {$IFNDEF FPC}Vcl.{$ENDIF}Graphics.TBitmap;
    ObjMsg: TDamMsg;
    A: string;
    X: Integer;
begin
  B := {$IFNDEF FPC}Vcl.{$ENDIF}Graphics.TBitmap.Create;
  try
    B.SetSize(Rect.Width, Rect.Height);

    B.Canvas.Brush.Color := clWindow;
    if (odSelected in State) then B.Canvas.Brush.Color := $00C6FFFF;
    B.Canvas.FillRect({$IFNDEF FPC}System.{$ENDIF}Types.Rect(0, 0, B.Width, B.Height));

    ObjMsg := TDamMsg(L.Items.Objects[Index]);

    IL.Draw(B.Canvas, 3, 2, Integer(ObjMsg.Icon));

    if ObjMsg.RaiseExcept then B.Canvas.Font.Color := clRed;
    A := ObjMsg.Name;
    if A[1] = '_' then
    begin
      B.Canvas.Font.Style := [fsUnderline];
      Delete(A, 1, 1);
    end;
    B.Canvas.TextOut(22, 2, A);

    X := 0;
    if (L.Count*L.ItemHeight)>(L.Height-4) then X := 16;
    IB.Draw(B.Canvas, L.Width-X-68, 3, Integer(ObjMsg.Buttons));
    if ObjMsg.Message='' then IP.Draw(B.Canvas, L.Width-X-85, 2, (0));

    L.Canvas.Draw(0, Rect.Top, B);
  finally
    B.Free;
  end;
end;

procedure TFrmDamList.MessageObjectAdded(C: TDamMsg);
begin
  {$IFDEF FPC}
  GlobalDesignHook.PersistentAdded(C, False); //required for adding object into form unit
  {$ENDIF}

  DoModified;
  DoSelectObject(C);
end;

procedure TFrmDamList.BtnAddClick(Sender: TObject);
var C: TDamMsg;
begin
  C := TDamMsg.Create(Own);
  C.Dam := Dam;
  C.Name := Designer.UniqueName(TDamMsg.ClassName);

  MessageObjectAdded(C);
end;

procedure TFrmDamList.BtnAddWizardClick(Sender: TObject);
var C: TDamMsg;
begin
  FrmDamMsgEdit := TFrmDamMsgEdit.Create(Application);
  try
    FrmDamMsgEdit.Dam := Dam;

    if FrmDamMsgEdit.ShowModal = mrOk then
    begin
      C := TDamMsg.Create(Own);
      C.Dam := Dam;

      FrmDamMsgEdit.StoreComp(C);

      MessageObjectAdded(C);
    end;
  finally
    FrmDamMsgEdit.Free;
  end;
end;

procedure TFrmDamList.BtnEditClick(Sender: TObject);
var C: TDamMsg;
begin
  C := GetFirstSel;

  FrmDamMsgEdit := TFrmDamMsgEdit.Create(Application);
  try
    FrmDamMsgEdit.Dam := Dam;
    FrmDamMsgEdit.DamMsg := C;

    if FrmDamMsgEdit.ShowModal = mrOk then
    begin
      FrmDamMsgEdit.StoreComp(C);

      DoModified;
    end;
  finally
    FrmDamMsgEdit.Free;
  end;
end;

procedure TFrmDamList.BtnDelClick(Sender: TObject);
begin
  Freeze := True;
  try
    {$IFDEF FPC}
    Designer.DeleteSelection;
    {$ELSE}
    Designer.DeleteSelection(True);
    {$ENDIF}
  finally
    Freeze := False;
  end;
  FillList;
end;

procedure TFrmDamList.Action_DelExecute(Sender: TObject);
begin
  if BtnDel.Enabled and L.Focused then
    BtnDel.Click;
end;

procedure TFrmDamList.BtnPreviewClick(Sender: TObject);
begin
  GetFirstSel.Preview;
end;

procedure TFrmDamList.BtnCutClick(Sender: TObject);
begin
  BtnCopy.Click;
  BtnDel.Click;
end;

procedure TFrmDamList.BtnCopyClick(Sender: TObject);
begin
  Designer.CopySelection;
end;

procedure TFrmDamList.BtnPasteClick(Sender: TObject);
{$IFNDEF FPC}
var I: Integer;
    tmpSel: TPersistentSelectionList;
{$ENDIF}
begin
  {$IFNDEF FPC}
  tmpSel := TPersistentSelectionList.Create;
  try
    PasteComponents(Own, Dam, tmpSel);

    for I := 0 to tmpSel.Count-1 do
    begin
      if tmpSel[I] is TDamMsg then
        TDamMsg(tmpSel[I]).Dam := Dam;
    end;

    DoModified;
    Designer.SetSelections(tmpSel);
  finally
    //tmpSel.Free; - as the object is referenced in TDesignWindow, we cannot free it here
  end;
  {$ENDIF}
end;

procedure TFrmDamList.BtnUpOrDownClick(Sender: TObject);
var Move, I, IEnd: Integer;
    C: TComponent;
begin
  if Sender = BtnUp then
  begin
    Move := -1;
    I := 0;
    IEnd := L.Count;
  end else
  begin
    Move := +1;
    I := L.Count-1;
    IEnd := -1;
  end;

  while I <> IEnd do
  begin
    if L.Selected[I] then
    begin
      C := TComponent(L.Items.Objects[I]);
      C.ComponentIndex := C.ComponentIndex + Move;
    end;
    Inc(I, -Move);
  end;

  DoModified;
end;

procedure TFrmDamList.BtnHideClick(Sender: TObject);
var Comp: TComponent;
    I: Integer;
    A: string;
    bClear, Mudou: Boolean;
begin
  bClear := GetFirstSel.Name[1] = '_';
  Mudou := False;

  try
    for I := 0 to Sel.Count-1 do
    begin
      Comp := TComponent(Sel[I]);

      A := Comp.Name;

      case bClear of
        False: if A[1] <> '_' then A := '_' + A;
         True: if A[1] = '_' then A := Copy(A, 2, Length(A));
      end;

      if Comp.Name <> A then
      begin
       try
         Comp.Name := A;
       except
         raise Exception.CreateFmt('Component name "%s" alreay exists', [A]);
       end;
       Mudou := True;
      end;
    end;
  finally
    if Mudou then DoModified;
  end;
end;

procedure TFrmDamList.BtnFindClick(Sender: TObject);
var Msg: TDamMsg;
begin
  if DoFindDamMessage(Dam, Msg) then
    DoSelectObject(Msg);
end;

procedure TFrmDamList.BtnBuildUnitClick(Sender: TObject);
begin
  GenerateFile(Dam, MUnit.Text);
end;

end.
