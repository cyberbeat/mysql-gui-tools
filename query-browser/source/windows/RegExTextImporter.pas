unit RegExTextImporter;

interface

uses
  gnugettext, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, VirtualTrees, StdCtrls, TntStdCtrls, ExtCtrls, TntExtCtrls,
  TntForms, AuxFuncs, TntClasses, ComCtrls, TntComCtrls, RegExpr, Contnrs,
  Buttons, TntButtons, StrUtils, Menus, TntMenus, TntDialogs, Clipbrd,
  MySQLConnection, myx_public_interface;

type
  TStructuredRegEx = class;
  TStructuredRegExMatch = class;

  TRegExMatchType = (
    TREMT_AlwaysAssumeMoreThanOneChar,
    TREMT_AllowEmptyMatches,
    TREMT_MatchExactCount
  );

  TBuildAction = (
    RBA_Preview,
    RBA_Save,
    RBA_Execute
  );
  TBuildActions = set of TBuildAction;

  TRegExTextImporterForm = class(TTntForm)
    InsertPnl: TTntPanel;
    ScrollBox: TTntScrollBox;
    ExtractPnl: TTntPanel;
    ContentPnl: TTntPanel;
    TntSplitter1: TTntSplitter;
    TntPanel3: TTntPanel;
    SourceMemo: TTntMemo;
    TntSplitter3: TTntSplitter;
    StructureVT: TVirtualStringTree;
    RegExPnl: TTntPanel;
    TntPanel2: TTntPanel;
    RegExMemo: TTntMemo;
    TntSplitter4: TTntSplitter;
    StructureTreePnl: TTntPanel;
    TntPanel4: TTntPanel;
    InsertMemoPnl: TTntPanel;
    TntPanel6: TTntPanel;
    InsertTemplateMemo: TTntMemo;
    TntSplitter5: TTntSplitter;
    InsertsMemo: TTntMemo;
    TntPanel7: TTntPanel;
    TntShape1: TTntShape;
    RegExErrorLbl: TTntLabel;
    FoundFuncPnl: TTntPanel;
    CreateNestedExpBtn: TTntButton;
    TntPanel1: TTntPanel;
    ExecuteRegExBtn: TTntButton;
    ExecuteNextRegExBtn: TTntButton;
    TntPanel5: TTntPanel;
    MatchLU: TTntComboBox;
    TntPanel8: TTntPanel;
    TntShape2: TTntShape;
    TntShape3: TTntShape;
    TntShape4: TTntShape;
    TntShape5: TTntShape;
    TntBevel1: TTntBevel;
    LoadFileBtn: TTntSpeedButton;
    ClearBtn: TTntSpeedButton;
    TriggerInsertCBox: TTntCheckBox;
    TntPanel9: TTntPanel;
    PreviewBtn: TTntButton;
    SourceTextPopupMenu: TTntPopupMenu;
    ExtractRegExMI: TTntMenuItem;
    ExtractNumbersMI: TTntMenuItem;
    ExtractTextandNumbersMI: TTntMenuItem;
    ExtractStopwordMI: TTntMenuItem;
    ExtractTextMI: TTntMenuItem;
    N1: TTntMenuItem;
    MatchExactCountMI: TTntMenuItem;
    AlwaysAssumeMoreThanOneCharMI: TTntMenuItem;
    AllowEmptyMatchesMI: TTntMenuItem;
    StoreSQLBtn: TTntButton;
    ExecuteSQLBtn: TTntButton;
    SaveBtn: TTntSpeedButton;
    LoadBtn: TTntSpeedButton;
    StructureVTPopupMenu: TPopupMenu;
    RemoveRegExMI: TMenuItem;
    CreateNestedExpressionMI: TMenuItem;
    N2: TMenuItem;
    RegExPopupMenu: TTntPopupMenu;
    CopyforCcodeMI: TTntMenuItem;
    PasteCcodeMI: TTntMenuItem;
    CaseInsensitiveCBox: TTntCheckBox;
    MultilineStringCBox: TTntCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure StructureVTGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure ExecuteRegExBtnClick(Sender: TObject);
    procedure ExecuteNextRegExBtnClick(Sender: TObject);

    procedure HandleRegExExecution(Success: Boolean);
    procedure RegExMemoChange(Sender: TObject);
    procedure CreateNestedExpBtnClick(Sender: TObject);
    procedure StructureVTInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure StructureVTInitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: Cardinal);
    procedure StructureVTFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure StructureVTFocusChanging(Sender: TBaseVirtualTree; OldNode,
      NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
      var Allowed: Boolean);
    procedure MatchLUCloseUp(Sender: TObject);
    procedure TriggerInsertCBoxClick(Sender: TObject);
    procedure InsertTemplateMemoDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure InsertTemplateMemoDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure StructureVTPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure PreviewBtnClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure InsertTemplateMemoChange(Sender: TObject);
    procedure ExtractTextandNumbersMIClick(Sender: TObject);
    procedure ExtractNumbersMIClick(Sender: TObject);
    procedure ExtractTextMIClick(Sender: TObject);
    procedure ExtractStopwordMIClick(Sender: TObject);

    function GetMatchType: TRegExMatchType;
    procedure AlwaysAssumeMoreThanOneCharMIClick(Sender: TObject);
    procedure LoadFileBtnClick(Sender: TObject);
    procedure StoreSQLBtnClick(Sender: TObject);
    procedure ExecuteSQLBtnClick(Sender: TObject);

    procedure ExecuteSQL(Node: PVirtualNode;
      Action: TBuildActions);
    procedure SaveBtnClick(Sender: TObject);
    procedure SaveRegExes(Node: PVirtualNode;
      var XMLFile: TextFile; Intent: Integer = 0);
    procedure LoadBtnClick(Sender: TObject);
    procedure StructureVTDblClick(Sender: TObject);
    procedure StructureVTPopupMenuPopup(Sender: TObject);
    procedure RemoveRegExMIClick(Sender: TObject);
    procedure DeleteRegEx(Node: PVirtualNode);
    procedure StructureVTFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure RegExErrorLblDblClick(Sender: TObject);
    procedure CopyforCcodeMIClick(Sender: TObject);
    procedure PasteCcodeMIClick(Sender: TObject);
    procedure StructureVTEditing(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure StructureVTNewText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);
    procedure SourceMemoChange(Sender: TObject);
    procedure CaseInsensitiveCBoxClick(Sender: TObject);

  private
    { Private declarations }
    FRegExStructure: TObjectList;
    FRegEx, FParseRegEx: TRegExpr;
    FRegExCompilerErrorPos: Integer;
    FRegExecuted: Boolean;
    FRegExError: WideString;

    FCurrentRegEx: TStructuredRegEx;
    FComponentInit: Boolean;

    FSQLFile: TextFile;

    FMySQLConn: TMySQLConn;
  public
    { Public declarations }
    property MySQLConn: TMySQLConn read FMySQLConn write FMySQLConn;
  end;

  TStructuredRegEx = class(TObject)
    constructor Create(Name: WideString;
      Tree: TBaseVirtualTree; Node: PVirtualNode);
    destructor Destroy; override;
  protected
    function GetMatchesGroupNode: PVirtualNode;
    function GetNestedRegExGroupNode: PVirtualNode;
  private
    FName: WideString;
    FRegEx: WideString;
    FSourceText: WideString;
    FInsertStatement: WideString;
    FTriggerInsertStatement: Boolean;

    FMatches: TObjectList;

    FTree: TBaseVirtualTree;
    FNode: PVirtualNode;

    FParentRegEx: TStructuredRegEx;
    FParentMatch: Integer;
  public
    property Name: WideString read FName write FName;
    property RegEx: WideString read FRegEx write FRegEx;
    property Matches: TObjectList read FMatches;
    property Node: PVirtualNode read FNode write FNode;
    property SourceText: WideString read FSourceText write FSourceText;
    property ParentRegEx: TStructuredRegEx read FParentRegEx write FParentRegEx;
    property ParentMatch: Integer read FParentMatch write FParentMatch;
    property TriggerInsertStatement: Boolean read FTriggerInsertStatement write FTriggerInsertStatement;
    property InsertStatement: WideString read FInsertStatement write FInsertStatement;

    property MatchesGroupNode: PVirtualNode read GetMatchesGroupNode;
    property NestedRegExGroupNode: PVirtualNode read GetNestedRegExGroupNode;
  end;

  TStructuredRegExMatch = class(TObject)
    constructor Create(Index: Integer; MatchPos: Integer;
      MatchLen: Integer; Match: WideString);
  protected
    function GetCaption: WideString;
  private
    FIndex: Integer;
    FMatchPos: Integer;
    FMatchLen: Integer;
    FMatch: WideString;
  public
    property Id: Integer read FIndex write FIndex;
    property MatchPos: Integer read FMatchPos write FMatchPos;
    property MatchLen: Integer read FMatchLen write FMatchLen;
    property Match: WideString read FMatch write FMatch;
    property Caption: WideString read GetCaption;
  end;

  TStructuredRegExNodeType = (
    TSRENT_RegEx,
    TSRENT_Matches, TSRENT_NestedRegEx,
    TSRENT_RegExMatch
  );

  PStructuredRegExNodeData = ^TStructuredRegExNodeData;
  TStructuredRegExNodeData = record
    NodeType: TStructuredRegExNodeType;
    RegEx: TStructuredRegEx;
    RegExMatchIndex: integer;
  end;

  TCurrentScanType = (
    TCST_None, TCST_Text, TCST_Numeric, TCST_Special, TCST_Space
  );

var
  RegExTextImporterForm: TRegExTextImporterForm;

implementation

uses ApplicationDataModule;

{$R *.dfm}

// -----------------------------------------------------------------

constructor TStructuredRegEx.Create(Name: WideString;
  Tree: TBaseVirtualTree; Node: PVirtualNode);

begin
  inherited Create;

  FName := Name;

  FTree := Tree;
  FNode := Node;

  FParentRegEx := nil;
  FTriggerInsertStatement := True;
  FInsertStatement := 'INSERT INTO foo()'+#13#10+'VALUES();';

  FMatches := TObjectList.Create;
end;

// -----------------------------------------------------------------

destructor TStructuredRegEx.Destroy;

begin
  FMatches.Free;

  inherited;
end;

// -----------------------------------------------------------------

function TStructuredRegEx.GetMatchesGroupNode: PVirtualNode;

var SearchedNode: PVirtualNode;
  NodeData: PStructuredRegExNodeData;

begin
  SearchedNode := FTree.GetFirstChild(FNode);
  while (SearchedNode<>nil) do
  begin
    NodeData := FTree.GetNodeData(SearchedNode);
    if (NodeData.NodeType=TSRENT_Matches) then
    begin
      Result := SearchedNode;
      Exit;
    end;

    SearchedNode := FTree.GetNextSibling(SearchedNode);
  end;

  raise EInOutError.Create(_('The Matches Group Node cannot be found'));
end;

// -----------------------------------------------------------------

function TStructuredRegEx.GetNestedRegExGroupNode: PVirtualNode;

var SearchedNode: PVirtualNode;
  NodeData: PStructuredRegExNodeData;

begin
  SearchedNode := FTree.GetFirstChild(FNode);
  while (SearchedNode<>nil) do
  begin
    NodeData := FTree.GetNodeData(SearchedNode);
    if (NodeData.NodeType=TSRENT_NestedRegEx) then
    begin
      Result := SearchedNode;
      Exit;
    end;

    SearchedNode := FTree.GetNextSibling(SearchedNode);
  end;
  raise EInOutError.Create(_('The Nested Group Node cannot be found'));
end;


// -----------------------------------------------------------------
// -----------------------------------------------------------------


constructor TStructuredRegExMatch.Create(Index: Integer; MatchPos: Integer;
  MatchLen: Integer; Match: WideString);

begin
  FIndex := Index;
  FMatchPos := MatchPos;
  FMatchLen := MatchLen;
  FMatch := Match;
end;

// -----------------------------------------------------------------

function TStructuredRegExMatch.GetCaption: WideString;
begin
  Result := Format('$%d [%d - %d]: %s',
    [FIndex, FMatchPos, FMatchPos+FMatchLen-1, FMatch])
end;


// -----------------------------------------------------------------
// -----------------------------------------------------------------


procedure TRegExTextImporterForm.FormCreate(Sender: TObject);

begin
  FRegExStructure := TObjectList.Create;
  FRegEx := TRegExpr.Create;
  FRegEx.ModifierI := CaseInsensitiveCBox.Checked;
  FRegEx.ModifierM := MultilineStringCBox.Checked;

  FParseRegEx := TRegExpr.Create;
  FParseRegEx.ModifierI := CaseInsensitiveCBox.Checked;
  FParseRegEx.ModifierM := MultilineStringCBox.Checked;
  FRegExecuted := False;

  InitForm(self);

  if (Not(ApplicationDM.QBOptions.RestoreWindowPos(self))) then
  begin
    Left := (Screen.Width+Width) div 2 - Width;
    Top := (Screen.Height+Height) div 2 - Height;
  end;

  FComponentInit := False;

  StructureVT.NodeDataSize := SizeOf(TStructuredRegExNodeData);

  StructureVT.RootNodeCount:=1;
end;

// -----------------------------------------------------------------

procedure TRegExTextImporterForm.FormDestroy(Sender: TObject);

begin
  FRegExStructure.Free;
  FRegEx.Free;
  FParseRegEx.Free;
end;

// -----------------------------------------------------------------

procedure TRegExTextImporterForm.FormClose(Sender: TObject; var Action: TCloseAction);

begin
  ApplicationDM.QBOptions.AddWindowPos(self);
end;

// -----------------------------------------------------------------

procedure TRegExTextImporterForm.StructureVTGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: WideString);

var NodeData: PStructuredRegExNodeData;

begin
  NodeData := Sender.GetNodeData(Node);

  if (NodeData<>nil) then
    if (NodeData.NodeType=TSRENT_RegEx) then
    begin
      if (NodeData.RegEx<>nil) then
        CellText := NodeData.RegEx.Name
    end
    else
      if (NodeData.NodeType=TSRENT_RegExMatch) then
      begin
        if (NodeData.RegEx<>nil) and (NodeData.RegExMatchIndex<NodeData.RegEx.Matches.Count) then
          CellText := TStructuredRegExMatch(NodeData.RegEx.Matches[NodeData.RegExMatchIndex]).Caption;
      end
      else
        if (NodeData.NodeType=TSRENT_Matches) then
          CellText := _('Matches')
        else
          if (NodeData.NodeType=TSRENT_NestedRegEx) then
            CellText := _('Nested RegEx');

end;

// -----------------------------------------------------------------

procedure TRegExTextImporterForm.HandleRegExExecution(Success: Boolean);

var MatchesGroupNode: PVirtualNode;
  i: integer;

begin
  CreateNestedExpBtn.Enabled := False;

  //Clear old matches
  if (FCurrentRegEx<>nil) then
    FCurrentRegEx.Matches.Clear;

  if Success then
  begin
    FRegExError := '';

    //Store new matches
    if (FCurrentRegEx<>nil) then
    begin
      for i:=0 to FRegEx.SubExprMatchCount do
        FCurrentRegEx.Matches.Add(TStructuredRegExMatch.Create(i,
          FRegEx.MatchPos[i], FRegEx.MatchLen[i], FRegEx.Match[i]));

      MatchesGroupNode := FCurrentRegEx.GetMatchesGroupNode;
      if (MatchesGroupNode<>nil) then
      begin
        StructureVT.ChildCount[MatchesGroupNode] :=
          FRegEx.SubExprMatchCount+1;

        StructureVT.Expanded[MatchesGroupNode] := True;

        StructureVT.InvalidateChildren(MatchesGroupNode, True);
      end;
    end;

    if FCurrentRegEx.Matches.Count>0 then
    begin
      SourceMemo.SelStart := 0;
      SourceMemo.SelLength := 0;

      SourceMemo.SelStart :=
        TStructuredRegExMatch(FCurrentRegEx.Matches[0]).MatchPos-1;
      SourceMemo.SelLength :=
        TStructuredRegExMatch(FCurrentRegEx.Matches[0]).MatchLen;
    end;

    SourceMemo.SetFocus;

    FRegExecuted:=True;

    RegExErrorLbl.Font.Color := clGreen;
    RegExErrorLbl.Caption := _('Expression found.');
  end
  else
  begin
    FRegExecuted := False;

    MatchesGroupNode := FCurrentRegEx.GetMatchesGroupNode;
    if (MatchesGroupNode<>nil) then
    begin
      StructureVT.ChildCount[MatchesGroupNode] := 0;
      StructureVT.InvalidateNode(MatchesGroupNode);
    end;

    if FRegEx.LastError<>0 then
    begin
      FRegExError := FRegEx.ErrorMsg(FRegEx.LastError);

      RegExErrorLbl.Font.Color := clRed;
      RegExErrorLbl.Caption := FRegExError;
    end
    else
    begin
      RegExErrorLbl.Font.Color := clPurple;
      RegExErrorLbl.Caption := _('Expression not found.');

      FRegEx.Free;
      FRegEx := TRegExpr.Create;
      FRegEx.ModifierI := CaseInsensitiveCBox.Checked;
      FRegEx.ModifierM := MultilineStringCBox.Checked;
    end;
  end;
end;

// -----------------------------------------------------------------

procedure TRegExTextImporterForm.ExecuteRegExBtnClick(Sender: TObject);

begin
  FRegEx.Expression := RegExMemo.Text;

  try
    RegExErrorLbl.Font.Color := clGray;
    RegExErrorLbl.Caption:=_('No Error.');

    HandleRegExExecution(FRegEx.Exec(SourceMemo.Text));
  except
    on x: ERegExpr do
    begin
      FRegExCompilerErrorPos := x.CompilerErrorPos;
      RegExErrorLbl.Font.Color := clRed;
      RegExErrorLbl.Caption := x.Message;
    end;

    on x: Exception do
    begin
      RegExErrorLbl.Font.Color := clRed;
      RegExErrorLbl.Caption := x.Message;
    end;
  end;
end;

// -----------------------------------------------------------------

procedure TRegExTextImporterForm.ExecuteNextRegExBtnClick(Sender: TObject);

begin
  if Not(FRegExecuted) then
    ExecuteRegExBtnClick(self)
  else
    HandleRegExExecution(FRegEx.ExecNext);
end;

// -----------------------------------------------------------------

procedure TRegExTextImporterForm.RegExMemoChange(Sender: TObject);

begin
  if (Not(FComponentInit)) and (FCurrentRegEx<>nil) then
    FCurrentRegEx.RegEx := RegExMemo.Text;
    
  FParseRegEx.Expression:=RegExMemo.Text;

  try
    RegExErrorLbl.Font.Color := clGray;
    RegExErrorLbl.Caption:=_('No Error.');

    if(RegExMemo.Text<>'')then
      FParseRegEx.Compile;
  except
    on x: ERegExpr do
    begin
      FRegExCompilerErrorPos := x.CompilerErrorPos;
      RegExErrorLbl.Font.Color := clRed;
      RegExErrorLbl.Caption := x.Message;
    end;

    on x: Exception do
    begin
      RegExErrorLbl.Font.Color := clRed;
      RegExErrorLbl.Caption := x.Message;
    end;
  end;

  ExecuteRegExBtn.Enabled := RegExMemo.Text<>'';
  ExecuteNextRegExBtn.Enabled := ExecuteRegExBtn.Enabled;
  PreviewBtn.Enabled := (RegExMemo.Text<>'') and
    ((InsertTemplateMemo.Text<>'') or (Not(TriggerInsertCBox.Checked)));
  StoreSQLBtn.Enabled := PreviewBtn.Enabled;
  ExecuteSQLBtn.Enabled := PreviewBtn.Enabled;
end;

// -----------------------------------------------------------------

procedure TRegExTextImporterForm.CreateNestedExpBtnClick(Sender: TObject);

var NestedRegExGroupNode: PVirtualNode;
  NodeData: PStructuredRegExNodeData;

begin
  NodeData := StructureVT.GetNodeData(StructureVT.FocusedNode);

  if (NodeData<>nil) and
    (NodeData.NodeType=TSRENT_RegExMatch) then
  begin
    StructureVT.GetNodeData(StructureVT.FocusedNode.Parent.Parent);

    NestedRegExGroupNode := FCurrentRegEx.GetNestedRegExGroupNode;
    StructureVT.AddChild(NestedRegExGroupNode, nil);

    StructureVT.Expanded[NestedRegExGroupNode] := True;
  end;
end;

// -----------------------------------------------------------------

procedure TRegExTextImporterForm.StructureVTInitNode(
  Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);

var RegEx: TStructuredRegEx;
  NodeData, ParentNodeData, ParentParentNodeData: PStructuredRegExNodeData;

begin
  NodeData := Sender.GetNodeData(Node);
  if (ParentNode<>nil) then
    ParentNodeData := Sender.GetNodeData(ParentNode)
  else
    ParentNodeData := nil;

  if (ParentNode=nil) or
    ((ParentNodeData<>nil) and
      (ParentNodeData.NodeType=TSRENT_NestedRegEx)) then
  begin
    RegEx := TStructuredRegEx.Create(Format(_('RegEx%d'),
      [FRegExStructure.Count+1]), Sender, Node);

    NodeData.NodeType := TSRENT_RegEx;
    NodeData.RegEx := RegEx;

    FRegExStructure.Add(RegEx);

    //If this is a nested RegEx, set ParentRegEx
    if (ParentNodeData<>nil) and
      (ParentNodeData.NodeType=TSRENT_NestedRegEx) then
    begin
      ParentParentNodeData := Sender.GetNodeData(ParentNode.Parent);
      NodeData.RegEx.ParentRegEx := ParentParentNodeData.RegEx;
      NodeData.RegEx.ParentMatch := Sender.FocusedNode.Index;
    end;

    Sender.ClearSelection;
    InitialStates := [ivsSelected, ivsHasChildren, ivsExpanded];
    Sender.FocusedNode := Node;

    Sender.InvalidateNode(Node);
  end
  else
  begin
    if (ParentNodeData.NodeType=TSRENT_RegEx) then
    begin
      if (Node.Index=0)then
        NodeData.NodeType := TSRENT_Matches
      else
        NodeData.NodeType := TSRENT_NestedRegEx;

      NodeData.RegEx := nil;

      InitialStates := [ivsExpanded];
    end
    else
      {if (ParentNodeData.NodeType=TSRENT_NestedRegEx) then
      begin
        ParentParentNodeData := Sender.GetNodeData(ParentNode.Parent);
      end
      else}
        if (ParentNodeData.NodeType=TSRENT_Matches) then
        begin
          ParentParentNodeData := Sender.GetNodeData(ParentNode.Parent);
          NodeData.NodeType := TSRENT_RegExMatch;
          NodeData.RegEx := ParentParentNodeData.RegEx;
          NodeData.RegExMatchIndex := Node.Index;
        end;
  end;
end;

// -----------------------------------------------------------------

procedure TRegExTextImporterForm.StructureVTInitChildren(
  Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);

var NodeData: PStructuredRegExNodeData;

begin
  NodeData := Sender.GetNodeData(Node);

  if (NodeData.NodeType=TSRENT_RegEx) then
    ChildCount := 2;
end;

procedure TRegExTextImporterForm.StructureVTFocusChanged(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);

var NodeData: PStructuredRegExNodeData;
  i: integer;

begin
  NodeData := Sender.GetNodeData(Node);

  if (NodeData<>nil) and
    (NodeData.NodeType=TSRENT_RegEx) and
    (FCurrentRegEx<>NodeData.RegEx) then
  begin
    FCurrentRegEx := NodeData.RegEx;

    StructureVT.InvalidateNode(Node);
    StructureVT.Invalidate;

    RegExMemo.Text := FCurrentRegEx.RegEx;
    SourceMemo.Text := FCurrentRegEx.SourceText;

    MatchLU.Enabled := FCurrentRegEx.FParentRegEx<>nil;
    MatchLU.Items.Clear;

    if (FCurrentRegEx.FParentRegEx<>nil) then
    begin
      for i:=1 to FCurrentRegEx.FParentRegEx.Matches.Count-1 do
        MatchLU.Items.Add(TStructuredRegExMatch(
          FCurrentRegEx.FParentRegEx.Matches[i]).Caption);

      MatchLU.ItemIndex := FCurrentRegEx.ParentMatch-1;

      MatchLUCloseUp(self);
    end;

    TriggerInsertCBox.Checked := FCurrentRegEx.TriggerInsertStatement;
    InsertTemplateMemo.Text := FCurrentRegEx.InsertStatement;
    InsertTemplateMemo.Enabled := FCurrentRegEx.TriggerInsertStatement;
  end;

  if (NodeData<>nil) then
    CreateNestedExpBtn.Enabled := (NodeData.NodeType=TSRENT_RegExMatch) and
      (NodeData.RegExMatchIndex>0)
  else
    CreateNestedExpBtn.Enabled := False;
end;

procedure TRegExTextImporterForm.StructureVTFocusChanging(
  Sender: TBaseVirtualTree; OldNode, NewNode: PVirtualNode; OldColumn,
  NewColumn: TColumnIndex; var Allowed: Boolean);

begin
  if (FCurrentRegEx<>nil)and(Not(FComponentInit))then
  begin
    FCurrentRegEx.RegEx := RegExMemo.Text;
    FCurrentRegEx.SourceText := SourceMemo.Text;

    FCurrentRegEx.TriggerInsertStatement := TriggerInsertCBox.Checked;
    FCurrentRegEx.InsertStatement := InsertTemplateMemo.Text;
  end;
end;

procedure TRegExTextImporterForm.MatchLUCloseUp(Sender: TObject);

begin
  if (FCurrentRegEx<>nil) then
  begin
    FCurrentRegEx.ParentMatch := MatchLU.ItemIndex+1;

    if(FCurrentRegEx.ParentMatch<FCurrentRegEx.FParentRegEx.Matches.Count)then
      SourceMemo.Text := TStructuredRegExMatch(
        FCurrentRegEx.FParentRegEx.Matches[FCurrentRegEx.ParentMatch]).Match;
  end;
end;

procedure TRegExTextImporterForm.TriggerInsertCBoxClick(Sender: TObject);

begin
  if (FCurrentRegEx<>nil) then
    FCurrentRegEx.TriggerInsertStatement := TriggerInsertCBox.Checked;

  InsertTemplateMemo.Enabled := TriggerInsertCBox.Checked;

  //InsertTemplateMemoChange(self);
end;

procedure TRegExTextImporterForm.InsertTemplateMemoDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);

var NodeData: PStructuredRegExNodeData;

begin
  Accept := False;
  if (Source=StructureVT) and (StructureVT.FocusedNode<>nil) then
  begin
    NodeData := StructureVT.GetNodeData(StructureVT.FocusedNode);
    if (NodeData.NodeType = TSRENT_RegExMatch) then
      Accept := True;
  end;
end;

procedure TRegExTextImporterForm.InsertTemplateMemoDragDrop(Sender,
  Source: TObject; X, Y: Integer);

var NodeData: PStructuredRegExNodeData;

begin
  if (Source=StructureVT) and (StructureVT.FocusedNode<>nil) then
  begin
    NodeData := StructureVT.GetNodeData(StructureVT.FocusedNode);
    if (NodeData.NodeType = TSRENT_RegExMatch) then
    begin
      InsertTemplateMemo.SelText := Format('$%s.%d',
        [NodeData.RegEx.Name, NodeData.RegExMatchIndex]);

      InsertTemplateMemo.SetFocus;
    end;
  end;
end;

procedure TRegExTextImporterForm.StructureVTPaintText(
  Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);

var NodeData: PStructuredRegExNodeData;

begin
  NodeData := StructureVT.GetNodeData(Node);

  if (NodeData.RegEx = FCurrentRegEx) and
    (NodeData.NodeType = TSRENT_RegEx) then
    TargetCanvas.Font.Style := [fsBold]
  else
    TargetCanvas.Font.Style := [];
end;

procedure TRegExTextImporterForm.ClearBtnClick(Sender: TObject);

begin
  StructureVT.Clear;
  FRegExStructure.Clear;

  RegExMemo.Text := '';
  MatchLU.Items.Clear;
  MatchLU.Enabled := False;

  SourceMemo.Text := '';

  TriggerInsertCBox.Checked := False;
  InsertTemplateMemo.Text := '';
  InsertsMemo.Text := '';

  StructureVT.RootNodeCount := 1;
end;

procedure TRegExTextImporterForm.InsertTemplateMemoChange(Sender: TObject);

begin
  PreviewBtn.Enabled := (RegExMemo.Text<>'') and
    ((InsertTemplateMemo.Text<>'') or (Not(TriggerInsertCBox.Checked)));
  StoreSQLBtn.Enabled := PreviewBtn.Enabled;
  ExecuteSQLBtn.Enabled := PreviewBtn.Enabled;

  if (FCurrentRegEx<>nil) then
    FCurrentRegEx.InsertStatement := InsertTemplateMemo.Text;
end;

// ------------------------------------------------------------------

procedure TRegExTextImporterForm.PreviewBtnClick(Sender: TObject);

begin
  ExecuteSQL(StructureVT.GetFirst, [RBA_Preview]);
end;

// ------------------------------------------------------------------

{procedure TRegExTextImporterForm.Preview;

var
  RegExpr: TRegExpr;
  Matches, CurrentMatches: TTntStringList;
  i, Index: integer;
  Match: TStructuredRegExMatch;
  MatchRegEx: TStructuredRegEx;
  Insert: WideString;

begin
  InsertsMemo.Text := '';

  RegExpr := TRegExpr.Create;
  RegExpr.ModifierI := CaseInsensitiveCBox.Checked;
  RegExpr.ModifierM := MultilineStringCBox.Checked;
  Matches := TTntStringList.Create;
  CurrentMatches := TTntStringList.Create;
  try
    RegExpr.Expression := '\$(\w+)\.(\d+)';

    if (RegExpr.Exec(InsertTemplateMemo.Text)) then
    begin
      //Get matches from insert command
      // Matches
      //   0 .. complete match
      //   1 .. RegEx name
      //   2 .. RegEx match index
      repeat
        MatchRegEx := nil;
        Match := nil;
        Index := StrToIntDef(RegExpr.Match[2], 0);

        for i:=0 to FRegExStructure.Count-1 do
          if WideSameText(
            TStructuredRegEx(FRegExStructure[i]).Name,
            RegExpr.Match[1]) then
            if Index<TStructuredRegEx(FRegExStructure[i]).Matches.Count then
            begin
              MatchRegEx := TStructuredRegEx(FRegExStructure[i]);
              Match := TStructuredRegExMatch(
                MatchRegEx.Matches[Index]);

              break;
            end;

        if (Match=nil) then
          raise EInOutError.Create(Format(_('Match %s not found in %s.'),
            [RegExpr.Match[0], InsertTemplateMemo.Text]));

        if (MatchRegEx<>FCurrentRegEx) then
          Matches.AddObject(RegExpr.Match[0], Match)
        else
          CurrentMatches.AddObject(RegExpr.Match[0], Match);

      until Not(RegExpr.ExecNext);

      // Loop over current text
      RegExpr.Expression := RegExMemo.Text;

      Index:=0;
      if (RegExpr.Exec(SourceMemo.Text)) then
        repeat
          Insert := InsertTemplateMemo.Text;

          for i:=0 to Matches.Count-1 do
            Insert := AnsiReplaceStr(Insert, Matches[i],
              TStructuredRegExMatch(Matches.Objects[i]).Match);

          for i:=0 to CurrentMatches.Count-1 do
            Insert := AnsiReplaceStr(Insert, CurrentMatches[i],
              RegExpr.Match[TStructuredRegExMatch(
                CurrentMatches.Objects[i]).Id]);

          InsertsMemo.Lines.Add(Insert);

          inc(Index);
        until (Not(RegExpr.ExecNext))or(Index>10);
    end;
  finally
    Matches.Free;
    CurrentMatches.Free;
    RegExpr.Free;
  end;
end;}

function ExtractTextandNumbersAsRegEx(Text: WideString;
  TextFixed: Boolean; NumbersFixed: Boolean;
  MatchType: TRegExMatchType): WideString;

var RegExpr: WideString;
  s: String;
  i, count: Integer;
  Char: Byte;
  CurrentScanType, NewScanType: TCurrentScanType;
  CurrentMatchLen: Integer;

begin
  s := Text;
  count := Length(s);

  RegExpr := '';
  CurrentMatchLen := 0;

  CurrentScanType := TCST_None;

  for i:=0 to count-1 do
  begin
    Char := Ord(s[i+1]);

    if ((Char>=Ord('A')) and (Char<=Ord('Z'))) or
      ((Char>=Ord('a')) and (Char<=Ord('z'))) or
      (Char=Ord('_'))then
      NewScanType := TCST_Text
    else
      if (Char>=Ord('0')) and (Char<=Ord('9')) then
        NewScanType := TCST_Numeric
      else
        if(Chr(Char)=' ')then
          NewScanType := TCST_Space
        else
          NewScanType := TCST_Special;

    if (CurrentScanType=TCST_None)then
    begin
      CurrentScanType := NewScanType;
      CurrentMatchLen := 1;
    end
    else
      if (CurrentScanType<>NewScanType) or (i=count-1) then
      begin
        if Not(
            ((NumbersFixed) and (CurrentScanType=TCST_Numeric)) or
            ((TextFixed) and (CurrentScanType=TCST_Text)) or
            (CurrentScanType=TCST_Special)
           ) then
        begin
          case CurrentScanType of
            TCST_Numeric:
              RegExpr := RegExpr + '(\d';
            TCST_Text:
              RegExpr := RegExpr + '(\w';
            TCST_Space:
              RegExpr := RegExpr + '\s';
          end;

          if (CurrentScanType=TCST_Space) then
            RegExpr := RegExpr + '+'
          else
            if (MatchType=TREMT_AlwaysAssumeMoreThanOneChar) then
              RegExpr := RegExpr + '+'
            else
              if (MatchType=TREMT_AllowEmptyMatches) then
                RegExpr := RegExpr + '*'
              else
                RegExpr := RegExpr + '{' + IntToStr(CurrentMatchLen) + '}';

          if (CurrentScanType<>TCST_Space) then
            RegExpr := RegExpr + ')';
        end;

        CurrentMatchLen := 1;
        CurrentScanType := NewScanType;
      end
      else
        if (CurrentScanType=NewScanType) then
          inc(CurrentMatchLen);

    if ((NumbersFixed) and (NewScanType=TCST_Numeric)) or
       ((TextFixed) and (NewScanType=TCST_Text)) then
      RegExpr := RegExpr + Chr(Char);

    if (NewScanType=TCST_Special) then
    begin
      if (Pos(Chr(Char), '\()[]+*?.$')>0) then
        RegExpr := RegExpr + '\'+Chr(Char)
      else
        RegExpr := RegExpr + Chr(Char);
    end
  end;

  Result := RegExpr;

end;

function TRegExTextImporterForm.GetMatchType: TRegExMatchType;

begin
  if (AlwaysAssumeMoreThanOneCharMI.Checked) then
    Result := TREMT_AlwaysAssumeMoreThanOneChar
  else
    if (AllowEmptyMatchesMI.Checked) then
      Result := TREMT_AllowEmptyMatches
    else
      Result := TREMT_MatchExactCount;
end;

procedure TRegExTextImporterForm.ExtractTextandNumbersMIClick(
  Sender: TObject);

begin
  RegExMemo.SelText := ExtractTextandNumbersAsRegEx(
    SourceMemo.SelText, False, False, GetMatchType);
end;

procedure TRegExTextImporterForm.ExtractNumbersMIClick(
  Sender: TObject);
begin
  RegExMemo.SelText := ExtractTextandNumbersAsRegEx(
    SourceMemo.SelText, True, False, GetMatchType);
end;

procedure TRegExTextImporterForm.ExtractTextMIClick(
  Sender: TObject);
begin
  RegExMemo.SelText := ExtractTextandNumbersAsRegEx(
    SourceMemo.SelText, False, True, GetMatchType);
end;

procedure TRegExTextImporterForm.ExtractStopwordMIClick(Sender: TObject);
begin
  RegExMemo.SelText := '([^'+SourceMemo.SelText+']*)';
end;

procedure TRegExTextImporterForm.AlwaysAssumeMoreThanOneCharMIClick(
  Sender: TObject);
begin
  if Sender is TTntMenuItem then
    TTntMenuItem(Sender).Checked := True;
end;

procedure TRegExTextImporterForm.LoadFileBtnClick(Sender: TObject);

var
  OpenDialog: TTntOpenDialog;

begin
  OpenDialog := TTntOpenDialog.Create(nil);
  try
    OpenDialog.Title := 'Open Source Text File ..';

    OpenDialog.InitialDir := ApplicationDM.GetLastFileDialogPaths(
      'RegExTextImporterTextFileDialog');

    OpenDialog.Filter := _('Any File') + ' (*.*)|*.*';
    OpenDialog.DefaultExt := 'txt';

    if (OpenDialog.Execute) then
    begin
      StructureVT.ClearSelection;
      StructureVT.FocusedNode := StructureVT.GetFirst;
      StructureVT.Selected[StructureVT.FocusedNode] := True;

      SourceMemo.Lines.LoadFromFile(OpenDialog.FileName);
      FCurrentRegEx.SourceText := SourceMemo.Text;

      ApplicationDM.SetLastFileDialogPaths(
        'RegExTextImporterTextFileDialog', ExtractFilePath(OpenDialog.FileName));
    end;

  finally
    OpenDialog.Free;
  end;
end;

procedure TRegExTextImporterForm.StoreSQLBtnClick(Sender: TObject);

var
  SaveDialog: TTntSaveDialog;

begin
  SaveDialog:=TTntSaveDialog.Create(nil);
  try
    SaveDialog.Title := 'Save SQL to File ..';

    SaveDialog.InitialDir := ApplicationDM.GetLastFileDialogPaths(
      'RegExTextImporterSaveSQLDialog');

    SaveDialog.Filter := _('Any File') + ' (*.*)|*.*';

    if (SaveDialog.Execute) then
    begin
      AssignFile(FSQLFile, SaveDialog.FileName);
      Rewrite(FSQLFile);
      try
        ExecuteSQL(StructureVT.GetFirst, [RBA_Save]);
      finally
        CloseFile(FSQLFile);
      end;

      ApplicationDM.SetLastFileDialogPaths(
        'RegExTextImporterSaveSQLDialog', ExtractFilePath(SaveDialog.FileName));
    end;

  finally
    SaveDialog.Free;
  end;
end;



procedure TRegExTextImporterForm.ExecuteSQLBtnClick(Sender: TObject);

begin
  ExecuteSQL(StructureVT.GetFirst, [RBA_Execute]);
end;


procedure TRegExTextImporterForm.ExecuteSQL(
  Node: PVirtualNode; Action: TBuildActions);

var
  RegExpr: TRegExpr;
  Matches, CurrentMatches: TTntStringList;
  i, j, Index: integer;
  Match: TStructuredRegExMatch;
  MatchRegEx: TStructuredRegEx;
  Insert: WideString;
  NodeData: PStructuredRegExNodeData;
  NestedRegExGroupNode, MatchesGroupNode: PVirtualNode;
  SourceText: WideString;
  RowCount: Integer;

begin
  if (Node=nil) then
    Exit;

  NodeData := StructureVT.GetNodeData(Node);

  if (NodeData.NodeType=TSRENT_RegEx) then
  begin
    //Set the SourceText accordingly
    if (NodeData.RegEx.ParentRegEx<>nil) then
    begin
      if (NodeData.RegEx.ParentMatch>=
        NodeData.RegEx.ParentRegEx.Matches.Count) then
        Exit;

      NodeData.RegEx.SourceText := TStructuredRegExMatch(NodeData.RegEx.ParentRegEx.Matches[
        NodeData.RegEx.ParentMatch]).Match;
    end;

    NestedRegExGroupNode := NodeData.RegEx.GetNestedRegExGroupNode;

    RegExpr := TRegExpr.Create;
    RegExpr.ModifierI := CaseInsensitiveCBox.Checked;
    RegExpr.ModifierM := MultilineStringCBox.Checked;
    Matches := TTntStringList.Create;
    CurrentMatches := TTntStringList.Create;
    try
      RegExpr.Expression := '\$(\w+)\.(\d+)';

      if (RegExpr.Exec(NodeData.RegEx.InsertStatement)) then
      begin
        //Get matches from insert command
        // Matches
        //   0 .. complete match
        //   1 .. RegEx name
        //   2 .. RegEx match index
        repeat
          MatchRegEx := nil;
          Match := nil;
          Index := StrToIntDef(RegExpr.Match[2], 0);

          for i:=0 to FRegExStructure.Count-1 do
            if WideSameText(
              TStructuredRegEx(FRegExStructure[i]).Name,
              RegExpr.Match[1]) then
            begin
              //If expression has not been executed till now
              //get matches now
              if TStructuredRegEx(FRegExStructure[i]).Matches.Count=0 then
              begin
                FRegEx.Expression := TStructuredRegEx(FRegExStructure[i]).RegEx;
                FRegEx.Exec(TStructuredRegEx(FRegExStructure[i]).SourceText);

                for j:=0 to FRegEx.SubExprMatchCount do
                  TStructuredRegEx(FRegExStructure[i]).Matches.Add(
                    TStructuredRegExMatch.Create(j,
                    FRegEx.MatchPos[j], FRegEx.MatchLen[j], FRegEx.Match[j]));

                MatchesGroupNode := TStructuredRegEx(FRegExStructure[i]).GetMatchesGroupNode;
                if (MatchesGroupNode<>nil) then
                begin
                  StructureVT.ChildCount[MatchesGroupNode] :=
                    FRegEx.SubExprMatchCount+1;

                  //StructureVT.Expanded[MatchesGroupNode] := True;

                  StructureVT.InvalidateChildren(MatchesGroupNode, True);
                end;
              end;

              if Index<TStructuredRegEx(FRegExStructure[i]).Matches.Count then
              begin
                MatchRegEx := TStructuredRegEx(FRegExStructure[i]);
                Match := TStructuredRegExMatch(
                  MatchRegEx.Matches[Index]);

                break;
              end;
            end;

          if (Match=nil) then
            raise EInOutError.Create(Format(_('Match %s not found in %s.'),
              [RegExpr.Match[0], NodeData.RegEx.InsertStatement]));

          if (MatchRegEx<>NodeData.RegEx) then
            Matches.AddObject(RegExpr.Match[0], Match)
          else
            CurrentMatches.AddObject(RegExpr.Match[0], Match);

        until Not(RegExpr.ExecNext);
      end;

      // Loop over current text
      RegExpr.Expression := NodeData.RegEx.RegEx;

      if (NodeData.RegEx.ParentRegEx<>nil) and
        (NodeData.RegEx.ParentMatch<NodeData.RegEx.ParentRegEx.Matches.Count)then
        SourceText := TStructuredRegExMatch(
          NodeData.RegEx.ParentRegEx.Matches[NodeData.RegEx.ParentMatch]).Match
      else
        SourceText := NodeData.RegEx.SourceText;

      InsertsMemo.Lines.Clear;
      RowCount := 0;

      if (RegExpr.Exec(SourceText)) then
        repeat
          Insert := NodeData.RegEx.InsertStatement;

          for i:=0 to Matches.Count-1 do
            Insert := AnsiReplaceStr(Insert, Matches[i],
              TStructuredRegExMatch(Matches.Objects[i]).Match);

          for i:=0 to CurrentMatches.Count-1 do
            Insert := AnsiReplaceStr(Insert, CurrentMatches[i],
              RegExpr.Match[TStructuredRegExMatch(
                CurrentMatches.Objects[i]).Id]);

          if (NodeData.RegEx.TriggerInsertStatement) then
          begin
            if (RBA_Preview in Action) then
              InsertsMemo.Lines.Add(Insert);

            if (RBA_Save in Action) then
            begin
              //Write to file
              WriteLn(FSQLFile, Insert);
            end;

            if (RBA_Execute in Action) then
            begin
              //Execute

              if (FMySQLConn<>nil) and
                (Not(FMySQLConn.ExecuteDirect(Insert, 5000, False))) then
              begin
                InsertsMemo.Lines.Add('Error: '+ Insert);
                InsertsMemo.Lines.Add(Format('%d - %s',
                  [myx_mysql_errno(FMySQLConn.MySQL),
                    myx_mysql_error(FMySQLConn.MySQL)]));
              end;
            end;

            inc(RowCount);

            //Update Matches
            for i:=0 to RegExpr.SubExprMatchCount do
            begin
              TStructuredRegExMatch(NodeData.RegEx.Matches[i]).Match := RegExpr.Match[i];
              TStructuredRegExMatch(NodeData.RegEx.Matches[i]).MatchPos := RegExpr.MatchPos[i];
              TStructuredRegExMatch(NodeData.RegEx.Matches[i]).MatchLen := RegExpr.MatchLen[i];
            end;
          end;

          //Loop over all Nested RegExes
          Node := StructureVT.GetFirstChild(NestedRegExGroupNode);
          while (Node<>nil) do
          begin
            ExecuteSQL(Node, Action);
            Node := StructureVT.GetNextSibling(Node);
          end;
        until Not(RegExpr.ExecNext);

      InsertsMemo.Lines.Add(Format(_('%d row(s) processed.'), [RowCount]));
    finally
      Matches.Free;
      CurrentMatches.Free;
      RegExpr.Free;
    end;
  end;
end;


procedure TRegExTextImporterForm.SaveBtnClick(Sender: TObject);

var
  XMLFile: TextFile;
  SaveDialog: TTntSaveDialog;

begin
  SaveDialog:=TTntSaveDialog.Create(nil);
  try
    SaveDialog.Title := 'Save to File ..';

    SaveDialog.InitialDir := ApplicationDM.GetLastFileDialogPaths(
      'RegExTextImporterSaveXMLDialog');

    SaveDialog.DefaultExt := 'rex';
    SaveDialog.Filter := _('RegExes File') + ' (*.rex)|*.rex|'+
      _('Any File') + ' (*.*)|*.*';

    if (SaveDialog.Execute) then
    begin
      AssignFile(XMLFile, SaveDialog.FileName);
      Rewrite(XMLFile);
      try
        SaveRegExes(StructureVT.GetFirst, XMLFile);
      finally
        CloseFile(XMLFile);
      end;

      ApplicationDM.SetLastFileDialogPaths(
        'RegExTextImporterSaveXMLDialog', ExtractFilePath(SaveDialog.FileName));
    end;

  finally
    SaveDialog.Free;
  end;
end;

procedure TRegExTextImporterForm.SaveRegExes(Node: PVirtualNode;
  var XMLFile: TextFile; Intent: Integer);

var
  NodeData: PStructuredRegExNodeData;
  IntentStr: WideString;
  ParentName: WideString;

begin
  while (Node<>nil) do
  begin
    NodeData := StructureVT.GetNodeData(Node);

    if (NodeData.NodeType=TSRENT_RegEx) then
    begin
      IntentStr := StringOfChar(' ', Intent);

      if (NodeData.RegEx.ParentRegEx<>nil) then
        ParentName := NodeData.RegEx.ParentRegEx.Name
      else
        ParentName := '';

      WriteLn(XMLFile, IntentStr+
        '<RegEx Name="'+HTMLEncode(NodeData.RegEx.Name)+'"'+
        ' TriggerInsertStatement="'+IntToStr(Ord(NodeData.RegEx.TriggerInsertStatement))+'"'+
        ' Parent="'+ParentName+'" '+
        ' ParentMatch="'+IntToStr(NodeData.RegEx.ParentMatch)+'">');
      WriteLn(XMLFile, IntentStr+
        '  <RegularExpression>'+HTMLEncode(NodeData.RegEx.RegEx)+'</RegularExpression>');
      WriteLn(XMLFile, IntentStr+
        '  <InsertStatement>'+HTMLEncode(NodeData.RegEx.InsertStatement)+'</InsertStatement>');

      //Call subnodes
      SaveRegExes(StructureVT.GetFirstChild(
        NodeData.RegEx.GetNestedRegExGroupNode), XMLFile, Intent+2);

      WriteLn(XMLFile, IntentStr+
        '</RegEx>');

      Node := StructureVT.GetNextSibling(Node);
    end;
  end;
end;

procedure TRegExTextImporterForm.LoadBtnClick(Sender: TObject);

var
  RegExpr: TRegExpr;
  XMLFile: TTntStringList;
  OpenDialog: TTntOpenDialog;
  FileName: WideString;
  NodeData: PStructuredRegExNodeData;
  Node: PVirtualNode;
  i: integer;

begin
  FileName:='';
  OpenDialog := TTntOpenDialog.Create(nil);
  try
    OpenDialog.Title := 'Open Source Text File ..';

    OpenDialog.InitialDir := ApplicationDM.GetLastFileDialogPaths(
      'RegExTextImporterTextFileDialog');

    OpenDialog.FileName := '*.rex';
    OpenDialog.Filter := _('RegExes File') + ' (*.rex)|*.rex|'+
      _('Any File') + ' (*.*)|*.*';
    OpenDialog.DefaultExt := 'rex';

    if (OpenDialog.Execute) then
    begin
      FileName := OpenDialog.FileName;

      ApplicationDM.SetLastFileDialogPaths(
        'RegExTextImporterTextFileDialog', ExtractFilePath(OpenDialog.FileName));
    end
    else
      Exit;

  finally
    OpenDialog.Free;
  end;

  FComponentInit := True;
  try
    StructureVT.Clear;
    FRegExStructure.Clear;

    XMLFile := TTntStringList.Create;
    RegExpr := TRegExpr.Create;
    RegExpr.ModifierI := CaseInsensitiveCBox.Checked;
    RegExpr.ModifierM := MultilineStringCBox.Checked;
    try
      XMLFile.LoadFromFile(FileName);

      RegExpr.Expression := '<RegEx\s+Name="([^"]+)"\s+TriggerInsertStatement="(\d+)"\s+Parent="([^"]*)"\s+ParentMatch="(\d+)">\s*<RegularExpression>([^<]*)</RegularExpression>\s*<InsertStatement>([^<]*)</InsertStatement>';

      if (RegExpr.Exec(XMLFile.Text)) then
        repeat
          //Matches:
          // 1..name
          // 2..trigger insert
          // 3..ParentName
          // 4..ParentMatch
          // 5..RegEx
          // 6..Insert

          if (RegExpr.Match[3]='')then
          begin
            Node := StructureVT.AddChild(nil);
            StructureVT.ReinitNode(Node, False);

            NodeData := StructureVT.GetNodeData(Node);
            NodeData.NodeType := TSRENT_RegEx;
            NodeData.RegEx.Name := HTMLDecode(RegExpr.Match[1]);
            NodeData.RegEx.RegEx := HTMLDecode(RegExpr.Match[5]);
            NodeData.RegEx.InsertStatement := HTMLDecode(RegExpr.Match[6]);
            NodeData.RegEx.TriggerInsertStatement := (RegExpr.Match[2]='1');
          end
          else
          begin
            for i:=0 to FRegExStructure.Count-1 do
              if(WideSameText(TStructuredRegEx(FRegExStructure[i]).Name,
                HTMLDecode(RegExpr.Match[3])))then
              begin
                Node := StructureVT.AddChild(
                  TStructuredRegEx(FRegExStructure[i]
                    ).GetNestedRegExGroupNode);
                StructureVT.ReinitNode(Node, False);

                NodeData := StructureVT.GetNodeData(Node);
                NodeData.NodeType := TSRENT_RegEx;
                NodeData.RegEx.ParentRegEx := TStructuredRegEx(FRegExStructure[i]);
                NodeData.RegEx.ParentMatch := Ord(StrToIntDef(RegExpr.Match[4], 1));
                NodeData.RegEx.Name := HTMLDecode(RegExpr.Match[1]);
                NodeData.RegEx.RegEx := HTMLDecode(RegExpr.Match[5]);
                NodeData.RegEx.InsertStatement := HTMLDecode(RegExpr.Match[6]);
                NodeData.RegEx.TriggerInsertStatement := (RegExpr.Match[2]='1');

                break;
              end;
          end;
          
        until Not(RegExpr.ExecNext);
    finally
      RegExpr.Free;
      XMLFile.Free;
    end;

    StructureVT.ClearSelection;
    StructureVT.FocusedNode := StructureVT.GetFirst;
    StructureVT.Selected[StructureVT.FocusedNode] := True;
    FCurrentRegEx := nil;
    StructureVTFocusChanged(StructureVT, StructureVT.FocusedNode, 0);
  finally
    FComponentInit := False;
  end;
end;

procedure TRegExTextImporterForm.StructureVTDblClick(Sender: TObject);

var
  NodeData: PStructuredRegExNodeData;

begin
  NodeData := StructureVT.GetNodeData(StructureVT.FocusedNode);

  if (NodeData<>nil) and
    (NodeData.NodeType=TSRENT_RegExMatch) and
    (FCurrentRegEx=NodeData.RegEx) and
    (Integer(StructureVT.FocusedNode.Index)<NodeData.RegEx.Matches.Count) then
  begin
    SourceMemo.SelStart := TStructuredRegExMatch(
      NodeData.RegEx.Matches[StructureVT.FocusedNode.Index]).MatchPos-1;
    SourceMemo.SelLength := TStructuredRegExMatch(
      NodeData.RegEx.Matches[StructureVT.FocusedNode.Index]).MatchLen;
  end;
end;

procedure TRegExTextImporterForm.StructureVTPopupMenuPopup(
  Sender: TObject);

var
  NodeData: PStructuredRegExNodeData;

begin
  NodeData := StructureVT.GetNodeData(StructureVT.FocusedNode);

  RemoveRegExMI.Enabled := (NodeData.NodeType=TSRENT_RegEx);
  CreateNestedExpressionMI.Enabled := (NodeData.NodeType=TSRENT_RegExMatch);
end;

procedure TRegExTextImporterForm.RemoveRegExMIClick(Sender: TObject);

begin
  DeleteRegEx(StructureVT.FocusedNode);
end;

procedure TRegExTextImporterForm.DeleteRegEx(Node: PVirtualNode);

var
  NodeData: PStructuredRegExNodeData;

begin
  NodeData := StructureVT.GetNodeData(Node);

  if(NodeData.NodeType = TSRENT_RegEx)then
  begin
    StructureVT.DeleteChildren(Node);
    StructureVT.DeleteNode(NodeData.RegEx.Node);
  end;
end;

procedure TRegExTextImporterForm.StructureVTFreeNode(
  Sender: TBaseVirtualTree; Node: PVirtualNode);

var
  NodeData: PStructuredRegExNodeData;
  i: integer;

begin
  NodeData := StructureVT.GetNodeData(Node);

  if NodeData.NodeType=TSRENT_RegEx then
  begin
    if NodeData.RegEx=FCurrentRegEx then
      FCurrentRegEx := nil;

    i := FRegExStructure.IndexOf(NodeData.RegEx);
    if i>-1 then
      FRegExStructure.Delete(i);
  end;

  NodeData.RegEx := nil;
end;

procedure TRegExTextImporterForm.RegExErrorLblDblClick(Sender: TObject);
begin
  RegExMemo.SelStart := FRegExCompilerErrorPos;
end;

procedure TRegExTextImporterForm.CopyforCcodeMIClick(Sender: TObject);
begin
  Clipboard.AsText := AnsiReplaceStr(
      AnsiReplaceStr(RegExMemo.Text,
        '\', '\\'),
      '"', '\"');
end;

procedure TRegExTextImporterForm.PasteCcodeMIClick(Sender: TObject);

begin
  RegExMemo.SelText := AnsiReplaceStr(AnsiReplaceStr(Clipboard.AsText, '\"', '"'), '\\', '\');
end;

procedure TRegExTextImporterForm.StructureVTEditing(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  var Allowed: Boolean);

var
  NodeData: PStructuredRegExNodeData;

begin
  Allowed := False;

  NodeData := StructureVT.GetNodeData(Node);

  if (NodeData.NodeType=TSRENT_RegEx) and (NodeData.RegEx<>nil) then
    Allowed := True;
end;

procedure TRegExTextImporterForm.StructureVTNewText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  NewText: WideString);

var
  NodeData: PStructuredRegExNodeData;

begin
  NodeData := StructureVT.GetNodeData(Node);

  if (NodeData.NodeType=TSRENT_RegEx) and (NodeData.RegEx<>nil) and
    (NewText<>'') then
  begin
    NodeData.RegEx.Name := NewText;
    Sender.InvalidateNode(Node);
  end;
end;

procedure TRegExTextImporterForm.SourceMemoChange(Sender: TObject);

begin
  if (FCurrentRegEx<>nil) and (Not(FComponentInit)) then
    FCurrentRegEx.SourceText := SourceMemo.Text;
end;

procedure TRegExTextImporterForm.CaseInsensitiveCBoxClick(Sender: TObject);
begin
  FRegEx.ModifierI := CaseInsensitiveCBox.Checked;
  FRegEx.ModifierM := MultilineStringCBox.Checked;
  FParseRegEx.ModifierI := CaseInsensitiveCBox.Checked;
  FParseRegEx.ModifierM := MultilineStringCBox.Checked;
end;

end.
