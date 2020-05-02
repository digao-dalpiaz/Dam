unit DamList;

interface

uses DesignWindows, System.Classes, System.Actions, Vcl.ActnList,
  Vcl.ImgList, Vcl.Controls, Vcl.ComCtrls, Vcl.StdCtrls,
  Vcl.Buttons, Vcl.ExtCtrls,
  //
  System.UITypes, System.Types, Vcl.Forms, Winapi.Messages,
  DesignIntf, ToolsAPI,
  DamUnit;

type
  TFrmDamList = class(TDesignWindow)
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
  private
    Dam: TDam;
    Own: TComponent;
    Sel: IDesignerSelections;

    Freeze: Boolean; //freeze list update

    procedure ClearSel;
    procedure FillList;
    procedure DoSelect;
    procedure UpdButtons;

    procedure WMClipboardUpdate(var Msg: TMessage); message WM_CLIPBOARDUPDATE;
    procedure UpdClipboard;
  public
    constructor Create(aComp: TComponent; aDesign: IDesigner); reintroduce;
    function GetDam: TDam;

    procedure DesignerClosed(const Designer: IDesigner; AGoingDormant: Boolean); override;
    procedure ItemsModified(const Designer: IDesigner); override;
    procedure ItemDeleted(const ADesigner: IDesigner; Item: TPersistent); override;
    procedure SelectionChanged(const ADesigner: IDesigner; const ASelection: IDesignerSelections); override;
  end;

implementation

{$R *.dfm}

uses System.SysUtils, System.StrUtils, Vcl.Graphics, Winapi.Windows,
  DamMsgEdit, Vcl.Clipbrd, System.Win.Registry,
  DamFind;

const REG_PATH = 'Digao\Dam';

//

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

//

constructor TFrmDamList.Create(aComp: TComponent; aDesign: IDesigner);
begin
    inherited Create(Application);

    Dam := TDam(aComp);
    Own := Dam.Owner;
    Designer := aDesign;
    ClearSel; //initialize selection interface

    FillList;
end;

procedure TFrmDamList.FormCreate(Sender: TObject);
var Reg: TRegistry;
begin
    L.Anchors := [akLeft,akRight,akTop,akBottom];
    BoxButtons.Anchors := [akRight,akTop];

    Constraints.MinWidth := Width;
    Constraints.MinHeight := Height;

    UpdClipboard;
    AddClipboardFormatListener(Handle); //set clipboard monitoring

    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      Reg.OpenKey(REG_PATH, True);
      LoadFormPos(Reg, Self);
    finally
      Reg.Free;
    end;
end;

procedure TFrmDamList.FormDestroy(Sender: TObject);
var Reg: TRegistry;
begin
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      Reg.OpenKey(REG_PATH, True);
      SaveFormPos(Reg, Self);
    finally
      Reg.Free;
    end;

    RemoveClipboardFormatListener(Handle); //remove clipboard monitoring

    Sel := nil;
    Designer := nil;
end;

procedure TFrmDamList.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    Action := caFree;
end;

procedure TFrmDamList.FormResize(Sender: TObject);
begin
    L.Invalidate;
end;

procedure TFrmDamList.UpdClipboard;
begin
    BtnPaste.Enabled := Designer.CanPaste;
end;

procedure TFrmDamList.WMClipboardUpdate(var Msg: TMessage);
begin
    //this method is fired when clipboard has changed

    UpdClipboard;
end;

function TFrmDamList.GetDam: TDam;
begin
    Result := Dam;
end;

procedure TFrmDamList.ClearSel;
begin
    Sel := nil;
    Sel := TDesignerSelections.Create;
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
        if C.GetParentComponent = Dam then L.Items.AddObject('', C);

      L.TopIndex := TopIdx;
      DoSelect;
    finally
      L.Items.EndUpdate;
    end;
end;

procedure TFrmDamList.DoSelect;

  function InSel(Obj: TObject): Boolean;
  var X: Integer;
  begin
      Result := False;
      for X := 0 to Sel.Count-1 do
        if Sel[X] = Obj then
        begin
            Result := True;
            Break;
        end;
  end;

var I: Integer;
    bSel: Boolean;
begin
    if Freeze then Exit;
    //

    L.Items.BeginUpdate;
    try
      for I := 0 to L.Count-1 do
      begin
          bSel := InSel(L.Items.Objects[I]);

          if L.Selected[I] <> bSel then
            L.Selected[I] := bSel;
      end;
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

    BtnUp.Enabled := SelVarious and (not L.Selected[0]);
    BtnDown.Enabled := SelVarious and (not L.Selected[L.Count-1]);

    BtnBuildUnit.Enabled := (Dam.DamUnitName<>'');
    BtnHide.Enabled := SelVarious;

    BtnFind.Enabled := L.Items.Count>0;

    StatusBar.Panels[0].Text := IfThen(Dam.HandleExceptions, 'Exceptions');
    StatusBar.Panels[1].Text := IfThen(Dam.DamDefault, 'Default');
    StatusBar.Panels[2].Text := Dam.DamUnitName;
    StatusBar.Panels[3].Text := Format('%d/%d', [L.SelCount, L.Count]);
end;

procedure TFrmDamList.ItemsModified(const Designer: IDesigner);
begin
    FillList;
end;

procedure TFrmDamList.ItemDeleted(const ADesigner: IDesigner; Item: TPersistent);
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

procedure TFrmDamList.DesignerClosed(const Designer: IDesigner; AGoingDormant: Boolean);
begin
    Close;
end;

procedure TFrmDamList.SelectionChanged(const ADesigner: IDesigner;
    const ASelection: IDesignerSelections);
begin
    Sel := ASelection;
    DoSelect; //update list selecions
end;

procedure TFrmDamList.LClick(Sender: TObject);
var I: Integer;
begin
    ClearSel;

    for I := 0 to L.Count-1 do
    begin
        if L.Selected[I] then
          Sel.Add( TPersistent(L.Items.Objects[I]) );
    end;

    Freeze := True;
    try
      Designer.SetSelections(Sel);
    finally
      Freeze := False;
    end;
    UpdButtons;
end;

procedure TFrmDamList.LDblClick(Sender: TObject);
begin
    if BtnEdit.Enabled then BtnEdit.Click;
end;

procedure TFrmDamList.LDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);

var B: Vcl.Graphics.TBitmap;
    ObjMsg: TDamMsg;
    A: String;
    X: Integer;
begin
    B := Vcl.Graphics.TBitmap.Create;
    try
      B.SetSize(Rect.Width, Rect.Height);

      B.Canvas.Brush.Color := clWindow;
      if (odSelected in State) then B.Canvas.Brush.Color := $00C6FFFF;
      B.Canvas.FillRect(System.Types.Rect(0, 0, B.Width, B.Height));

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

procedure TFrmDamList.BtnAddClick(Sender: TObject);
var C: TDamMsg;
begin
    C := TDamMsg.Create(Own);
    C.Dam := Dam;
    C.Name := UniqueName(C);

    Freeze := True;
    try
      Designer.SelectComponent(C);
    finally
      Freeze := False;
    end;
    Designer.Modified;
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

          Freeze := True;
          try
            Designer.SelectComponent(C);
          finally
            Freeze := False;
          end;
          Designer.Modified;
      end;
    finally
      FrmDamMsgEdit.Free;
    end;
end;

procedure TFrmDamList.BtnEditClick(Sender: TObject);
var C: TDamMsg;
begin
    C := TDamMsg(Sel[0]);

    FrmDamMsgEdit := TFrmDamMsgEdit.Create(Application);
    try
      FrmDamMsgEdit.Dam := Dam;
      FrmDamMsgEdit.DamMsg := C;

      if FrmDamMsgEdit.ShowModal = mrOk then
      begin
          FrmDamMsgEdit.StoreComp(C);

          Designer.Modified;
      end;
    finally
      FrmDamMsgEdit.Free;
    end;
end;

procedure TFrmDamList.BtnDelClick(Sender: TObject);
begin
    Freeze := True;
    try
      Designer.DeleteSelection(True);
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
    TDamMsg(Sel[0]).Preview;
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
var I: Integer;
begin
    ClearSel;
    PasteComponents(Own, Dam, Sel);

    for I := 0 to Sel.Count-1 do
    begin
        if Sel[I] is TDamMsg then
          TDamMsg(Sel[I]).Dam := Dam;
    end;

    Freeze := True;
    try
      Designer.SetSelections(Sel);
    finally
      Freeze := False;
    end;
    Designer.Modified;
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
    Designer.Modified;
end;

procedure TFrmDamList.BtnHideClick(Sender: TObject);
var Comp: TComponent;
    I: Integer;
    A: String;
    bClear, Mudou: Boolean;
begin
    bClear := TComponent(Sel[0]).Name[1] = '_';
    Mudou := False;

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

    if Mudou then Designer.Modified;
end;

procedure TFrmDamList.BtnFindClick(Sender: TObject);
begin
  DoFindDamMessage(Dam, Designer);
end;

////////////////////////////////////////////////////////////////
// UNIT BULDER
////////////////////////////////////////////////////////////////

procedure TFrmDamList.BtnBuildUnitClick(Sender: TObject);
var ModServices: IOTAModuleServices;
    Module: IOTAModule;
    C: TComponent;
    Dir: String;
    aFile: String;
    StmUnit: String;
    UseUnit: String;
    Msg: TDamMsg;
    S: TStringList;
    A, aFuncName, aFuncKind, aResFunc, aPreCmd, aCmd, aEv, aPar1, aPar2, aCab, aTxt: String;
    aDecs, Func: String;
    aTime: String;
const ENTER = #13#10;
begin
    ModServices := BorlandIDEServices as IOTAModuleServices;
    Module := ModServices.FindFormModule(Own.Name);

    if Module = nil then
      raise Exception.Create('Form Module not found');

    aTime := DateTimeToStr(Now);

    Dir := ExtractFilePath(Module.FileName);

    UseUnit := ExtractFileName(Module.FileName);
    UseUnit := ChangeFileExt(UseUnit, '');

    StmUnit := TDam(Dam).DamUnitName;

    aFile := StmUnit + '.pas';

    for C in Own do
    if C is TDamMsg then
    begin
        Msg := TDamMsg(C);
        if (Msg.Dam.DamUnitName = StmUnit) then
        begin
          A := Msg.Name;
          if A[1] = '_' then Delete(A, 1, 1);

          aFuncName := A;
          aCmd := Own.Name+'.'+Msg.Name;

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

    A := MUnit.Text;
    A := StringReplace(A, '<UNIT>', StmUnit, []);
    A := StringReplace(A, '<TIMESTAMP>', aTime, []);
    A := StringReplace(A, '<USES>', UseUnit, []);
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
