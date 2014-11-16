unit AdminServerHealthGraphSettings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, myx_admin_public_interface, ComCtrls, AuxFuncs,
  TntForms, TntStdCtrls, TntComCtrls,
  AdminServerHealthLineGraph, AdminServerHealthBarGraph;

type
  TAdminServerHealthGraphSettingsForm = class(TTntForm)
    ValueSettingsGBox: TTntGroupBox;
    CancelBtn: TTntButton;
    ApplyBtn: TTntButton;
    Label3: TTntLabel;
    ValueUnitCBox: TTntComboBox;
    ValueFormulaMemo: TTntMemo;
    Label6: TTntLabel;
    GeneralGBox: TTntGroupBox;
    CaptionLbl: TTntLabel;
    CaptionEd: TTntEdit;
    DisplayCaptionCBox: TTntCheckBox;
    Label2: TTntLabel;
    GraphTypeCBox: TTntComboBox;
    MinMaxGBox: TTntGroupBox;
    Label4: TTntLabel;
    MinValueEd: TTntEdit;
    Label5: TTntLabel;
    MaxValueEd: TTntEdit;
    AutoextendValueCBox: TTntCheckBox;
    Label7: TTntLabel;
    MaxFormularEd: TTntEdit;
    Label8: TTntLabel;
    ValueCaptionEd: TTntEdit;
    MaxCaptionLbl: TTntLabel;
    MaxCaptionEd: TTntEdit;
    Label10: TTntLabel;
    RefreshTimeCBox: TTntComboBox;
    CaptionDescLbl: TTntLabel;
    DisplayCaptionDescLbl: TTntLabel;
    GraphTypeDescLbl: TTntLabel;
    Label14: TTntLabel;
    Label15: TTntLabel;
    Label16: TTntLabel;
    Label17: TTntLabel;
    Label18: TTntLabel;
    Label19: TTntLabel;
    Label20: TTntLabel;
    Label21: TTntLabel;
    MaxCaptionDescLbl: TTntLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ApplyBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure GraphTypeCBoxCloseUp(Sender: TObject);
    procedure ValueFormulaMemoDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure ValueFormulaMemoDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ValueFormulaMemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FCurrentGraph: TMYX_HEALTH_GRAPH;
    FGraphFrameBundle: TGraphFrameBundle;
    FGraphContainer: TTntGroupBox;

    procedure ValdiateAndApply;
  public
    procedure NewGraph(Container: TTntGroupBox);
    procedure SetGraph(GraphFrameBundle: TGraphFrameBundle);
  end;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  AdminServerHealth, ApplicationDataModule, gnugettext, MyxError;

{$R *.dfm}

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthGraphSettingsForm.FormCreate(Sender: TObject);

begin
  InitForm(Self);
  ApplicationDM.Options.RestoreWindowPos(Self);

  FGraphFrameBundle := nil;
  FCurrentGraph := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthGraphSettingsForm.FormClose(Sender: TObject; var Action: TCloseAction);

begin
  ApplicationDM.Options.AddWindowPos(Self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthGraphSettingsForm.SetGraph(GraphFrameBundle: TGraphFrameBundle);

begin
  FGraphFrameBundle := GraphFrameBundle;
  if Assigned(GraphFrameBundle) then
    FCurrentGraph := GraphFrameBundle.graph
  else
    FCurrentGraph := nil;

  if Assigned(FCurrentGraph) then
  begin
    CaptionEd.Text := FCurrentGraph.graph_caption;
    DisplayCaptionCBox.Checked := FCurrentGraph.display_graph_caption = 1;
    GraphTypeCBox.ItemIndex := Ord(FCurrentGraph.graphtype) - 1;
    GraphTypeCBox.Enabled := False;
    GraphTypeCBoxCloseUp(Self);
    GraphTypeCBox.Enabled := False;

    ValueFormulaMemo.Text := FCurrentGraph.value_formula;
    ValueUnitCBox.ItemIndex := Ord(FCurrentGraph.value_unit);
    ValueCaptionEd.Text := FCurrentGraph.value_caption;

    MinValueEd.Text := FormatFloat('#######0.##', FCurrentGraph.min);
    MaxValueEd.Text := FormatFloat('#######0.##', FCurrentGraph.max);
    AutoextendValueCBox.Checked := (FCurrentGraph.autoextend_max=1);
    MaxFormularEd.Text := FCurrentGraph.max_formula;
    MaxCaptionEd.Text := FCurrentGraph.max_caption;
  end
  else
  begin
    CaptionEd.Text := '';
    DisplayCaptionCBox.Checked := False;
    GraphTypeCBox.ItemIndex := 0;
    GraphTypeCBox.Enabled := True;
    GraphTypeCBoxCloseUp(Self);
    GraphTypeCBox.Enabled := True;

    ValueFormulaMemo.Text := '';
    ValueUnitCBox.ItemIndex := 0;
    ValueCaptionEd.Text := '';

    MinValueEd.Text := '0';
    MaxValueEd.Text := '100';
    AutoextendValueCBox.Checked := False;
    MaxFormularEd.Text := '';
    MaxCaptionEd.Text := '';
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthGraphSettingsForm.NewGraph(Container: TTntGroupBox);

begin
  FGraphContainer := Container;
  SetGraph(nil);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthGraphSettingsForm.ValdiateAndApply;

// Check values for plausibility and if all is ok then copy them to the current graph instance.

var
  Min: Double;
  Max: Double;
  
begin
  // Validation
  Min := StrToFloat(MinValueEd.Text);
  Max := StrToFloat(MaxValueEd.Text);
  if Min > Max then
    raise EMyxError.Create(_('Validation failed:') + #13#10#13#10 +
      _('Minimum value is larger than maximum value.'));

  // Application
  FCurrentGraph.graph_caption := CaptionEd.Text;
  FCurrentGraph.display_graph_caption := Ord(DisplayCaptionCBox.Checked);
  case MYX_HEALTH_GRAPH_TYPE(GraphTypeCBox.ItemIndex + 1) of
    MYX_LINE_GRAPH:
      FCurrentGraph.graphtype := MYX_LINE_GRAPH;
    MYX_BAR_GRAPH:
      FCurrentGraph.graphtype := MYX_BAR_GRAPH;
  end;

  FCurrentGraph.value_formula := ValueFormulaMemo.Text;
  case MYX_HEALTH_GRAPH_VALUE_UNIT(ValueUnitCBox.ItemIndex) of
    MYX_HGVU_PERCENTAGE:
      FCurrentGraph.value_unit := MYX_HGVU_PERCENTAGE;
    MYX_HGVU_COUNT:
      FCurrentGraph.value_unit := MYX_HGVU_COUNT;
    MYX_HGVU_BYTE:
      FCurrentGraph.value_unit := MYX_HGVU_BYTE;
    MYX_HGVU_SECONDS:
      FCurrentGraph.value_unit := MYX_HGVU_SECONDS;
  end;

  FCurrentGraph.value_caption := ValueCaptionEd.Text;

  FCurrentGraph.min := Min;
  FCurrentGraph.max := Max;
  FCurrentGraph.autoextend_max := Ord(AutoextendValueCBox.Checked);
  FCurrentGraph.max_formula := MaxFormularEd.Text;
  FCurrentGraph.max_caption := MaxCaptionEd.Text;

  case RefreshTimeCBox.ItemIndex of
    0:
      FCurrentGraph.refreshtime := 200;
    1:
      FCurrentGraph.refreshtime := 500;
    2:
      FCurrentGraph.refreshtime := 1000;
    3:
      FCurrentGraph.refreshtime := 2000;
    4:
      FCurrentGraph.refreshtime := 5000;
    5:
      FCurrentGraph.refreshtime := 10000;
    6:
      FCurrentGraph.refreshtime := 15000;
    7:
      FCurrentGraph.refreshtime := 30000;
    8:
      FCurrentGraph.refreshtime := 60000;
  end;

  if Assigned(FGraphFrameBundle) then
  begin
    // FGraphFrameBundle is only assigned if we edit an existing graph.
    if FGraphFrameBundle.GraphFrame is TAdminServerHealthBarGraphFrame then
    begin
      with TAdminServerHealthBarGraphFrame(FGraphFrameBundle.GraphFrame) do
      begin
        CaptionPnl.Visible := DisplayCaptionCBox.Checked;
        CaptionLbl.Caption := CaptionEd.Text;
        ValueUnit := FCurrentGraph.value_unit;
        ValueCaption := ValueCaptionEd.Text;
        MaxCaption := MaxCaptionEd.Text;
      end;
    end
    else
      if FGraphFrameBundle.GraphFrame is TAdminServerHealthLineGraphFrame then
      begin
        with TAdminServerHealthLineGraphFrame(FGraphFrameBundle.GraphFrame) do
          ValueUnit := FCurrentGraph.value_unit;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthGraphSettingsForm.ApplyBtnClick(Sender: TObject);

var
  current_widget_ypos: Integer;
  I: Integer;
  PageNr: Integer;
  GroupNr: Integer;
  NewGraph: Boolean;

begin
  NewGraph := False;
  if FCurrentGraph = nil then
  begin
    NewGraph := True;
    FCurrentGraph := TMYX_HEALTH_GRAPH.create('', 0, MYX_LINE_GRAPH, MYX_HGVU_PERCENTAGE, 0, 0, 0, '', '', '',
      '', '', '', 1000, FGraphContainer.ComponentCount + 1);
  end;

  // This validation might throw an exception if validation went wrong.
  ValdiateAndApply;

  if NewGraph then
  begin
    // Add graph to HealthPages
    PageNr := TTabSheet(FGraphContainer.Parent.Parent).TabIndex;
    GroupNr := 0;
    for I := 0 to FGraphContainer.Parent.ControlCount-1 do
      if(FGraphContainer=FGraphContainer.Parent.Controls[I])then
      begin
        GroupNr := I;
        break;
      end;

    if(PageNr<TAdminServerHealthForm(Owner).HealthPages.pages.Count)then
      if(GroupNr<TAdminServerHealthForm(Owner).HealthPages.pages[PageNr].groups.Count)then
        TAdminServerHealthForm(Owner).HealthPages.pages[
          PageNr].groups[GroupNr].graphs.Add(FCurrentGraph);

    current_widget_ypos := FGraphContainer.Height;

    TAdminServerHealthForm(Owner).AddGraphToGroupBox(FGraphContainer, FCurrentGraph, current_widget_ypos);

    for I := 0 to FGraphContainer.Owner.ComponentCount-1 do
      if(FGraphContainer.Owner.Components[I] is TTntGroupBox)then
        if(TTntGroupBox(FGraphContainer.Owner.Components[I]).Top>
          FGraphContainer.Top)then
          TTntGroupBox(FGraphContainer.Owner.Components[I]).Top :=
            TTntGroupBox(FGraphContainer.Owner.Components[I]).Top+
            current_widget_ypos-FGraphContainer.Height;
            
    FGraphContainer.Height := current_widget_ypos;
  end;


  TAdminServerHealthForm(Owner).RefreshSystemVarsInFormulas := True;

  FCurrentGraph := nil;
  FGraphContainer := nil;

  ModalResult := mrOK;
  Hide;

  TAdminServerHealthForm(Owner).SaveGraphs;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthGraphSettingsForm.CancelBtnClick(Sender: TObject);

begin
  FCurrentGraph := nil;

  ModalResult := mrCancel;
  Hide;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthGraphSettingsForm.ValueFormulaMemoDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);

begin
  Accept := False;
  if (Source is TTntListView) and (TTntListView(Source).Owner = Owner) then
    Accept := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthGraphSettingsForm.ValueFormulaMemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

// Memos keep escape keystrokes instead to allow the form to handle them. We fix that here.

begin
  if Key = VK_ESCAPE then
    CancelBtn.Click;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthGraphSettingsForm.ValueFormulaMemoDragDrop(Sender, Source: TObject; X, Y: Integer);

var
  I: Integer;
  lastChar: WideString;
  ListView: TTntListView;

begin
  if(Source is TTntListView)and(Sender.InheritsFrom(TCustomEdit))then
  begin
    with Owner as TAdminServerHealthForm do
    begin
      if(HealthPageControl.ActivePage=ServerStatusTabSheet)then
        ListView := ServerStatusListView
      else if(HealthPageControl.ActivePage=ServerVariablesTabSheet)then
        ListView := VariablesListView
      else
        ListView := nil;

      if(ListView<>nil)then
      begin
        for I := 0 to ListView.Items.Count-1 do
          if(ListView.Items[I].Selected)then
          begin
            lastChar := Copy(TCustomEdit(Sender).Text, Length(TCustomEdit(Sender).Text), 1);
            if(lastChar<>'')and(lastChar<>'+')and(lastChar<>'-')and
              (lastChar<>'*')and(lastChar<>'/')then
              TCustomEdit(Sender).Text := TCustomEdit(Sender).Text+'+';

            TCustomEdit(Sender).Text := TCustomEdit(Sender).Text+
              '['+ListView.Items[I].Caption+']';
          end;
      end;
    end;

    TCustomEdit(Sender).SetFocus;
    TCustomEdit(Sender).SelStart := Length(TCustomEdit(Sender).Text);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthGraphSettingsForm.GraphTypeCBoxCloseUp(Sender: TObject);

begin
  if GraphTypeCBox.ItemIndex = 1 then
  begin
    CaptionLbl.Enabled := True;
    CaptionEd.Enabled := True;
    CaptionDescLbl.Enabled := True;

    DisplayCaptionCBox.Enabled := True;
    DisplayCaptionDescLbl.Enabled := True;

    MaxCaptionLbl.Enabled := True;
    MaxCaptionEd.Enabled := True;
    MaxCaptionDescLbl.Enabled := True;
  end
  else
  begin
    CaptionLbl.Enabled := False;
    CaptionEd.Text := '';
    CaptionEd.Enabled := False;
    CaptionDescLbl.Enabled := False;

    DisplayCaptionCBox.Enabled := False;
    DisplayCaptionCBox.Checked := False;
    DisplayCaptionDescLbl.Enabled := False;

    MaxCaptionLbl.Enabled := False;
    MaxCaptionEd.Enabled := False;
    MaxCaptionDescLbl.Enabled := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
