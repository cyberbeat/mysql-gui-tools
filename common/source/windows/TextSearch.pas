unit TextSearch;

interface

uses
  gnugettext, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls, ComCtrls, TntComCtrls, TntForms, ExtCtrls,
  Options, AuxFuncs, UniCodeEditor;

type

  TTextSearchOption = (
    tsoBackwards,             // Search backwards instead of forward.
    tsoEntireScope,           // Search in entire text, not only in selected text.
    tsoIgnoreNonSpacing,      // Ignore non-spacing characters in search.
    tsoMatchCase,             // Case sensitive search.
    tsoPrompt,                // Ask user for each replace action.
    tsoRegularExpression,     // Search using regular expressions.
    tsoReplace,               // Replace, not simple search.
    tsoReplaceAll,            // Replace all occurences
    tsoSelectedOnly,          // Search in selected text only.
    tsoSpaceCompress,         // Handle several consecutive white spaces as one white space,
                             // so "ab   cd" will match "ab cd" and "ab        cd".
    tsoWholeWord              // Match entire words only.
  );
  TTextSearchOptions = set of TTextSearchOption;

  TTextSearchEvent = function (Sender: TObject; SearchText: WideString; ReplaceText: WideString;
    SearchOptions: TTextSearchOptions): Integer of object;

  TTextSearchForm = class(TTntForm)
    PageControl: TTntPageControl;
    SearchTabSheet: TTntTabSheet;
    ReplaceTabSheet: TTntTabSheet;
    SearchLbl: TTntLabel;
    SearchLU: TTntComboBox;
    SearchBtn: TTntButton;
    CloseBtn: TTntButton;
    ReplacesSearchLbl: TTntLabel;
    ReplaceSourceLU: TTntComboBox;
    ReplaceWithLbl: TTntLabel;
    ReplaceWithLU: TTntComboBox;
    ReplaceAllBtn: TTntButton;
    Close2Btn: TTntButton;
    OptionsTabSheet: TTntTabSheet;
    ReplaceBtn: TTntButton;
    DetailsBtn: TTntButton;
    Details2Btn: TTntButton;
    OptionPnl: TPanel;
    ScopeGBox: TTntGroupBox;
    SearchInSelectedColumnRBtn: TTntRadioButton;
    SearchAllColumnsRBtn: TTntRadioButton;
    DirectionGBox: TTntGroupBox;
    SearchDownRBtn: TTntRadioButton;
    SearchUpRBtn: TTntRadioButton;
    OptionsGBox: TTntGroupBox;
    CaseSensitiveCBox: TTntCheckBox;
    WholeWordsOnlyCBox: TTntCheckBox;
    UseRegExCBox: TTntCheckBox;
    TntGroupBox1: TTntGroupBox;
    SearchFromCursorRBtn: TTntRadioButton;
    SearchFromTopRBtn: TTntRadioButton;
    NoMatchFoundPnl: TPanel;

    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CloseBtnClick(Sender: TObject);
    procedure SearchBtnClick(Sender: TObject);
    procedure ReplaceBtnClick(Sender: TObject);
    procedure ReplaceAllBtnClick(Sender: TObject);
    procedure CaseSensitiveCBoxClick(Sender: TObject);
    procedure UpdateCurrentOptions;
    procedure FormDeactivate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DetailsBtnClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
  private
    FOnSearch: TTextSearchEvent;

    FCurrentOptions: TTextSearchOptions;
    FOptionProvider: IOptionProvider;
    FDetailsVisible: Boolean;
    FEditor: TCustomUniCodeEdit;
  protected
    procedure SetIOptionProvider(OptionProvider: IOptionProvider);
    procedure SetDetailsVisible(DetailsVisible: Boolean);
  public
    constructor Create(Owner: TComponent; OptionProvider: IOptionProvider); reintroduce;

    property OnSearch: TTextSearchEvent read FOnSearch write FOnSearch;
    property OptionProvider: IOptionProvider read FOptionProvider write SetIOptionProvider;
    property DetailsVisible: Boolean read FDetailsVisible write SetDetailsVisible;
  end;

var
  TextSearchForm: TTextSearchForm;

//----------------------------------------------------------------------------------------------------------------------

implementation

{$R *.dfm}

//----------------------------------------------------------------------------------------------------------------------

constructor TTextSearchForm.Create(Owner: TComponent; OptionProvider: IOptionProvider);

begin
  inherited Create(nil);

  if Owner is TCustomUniCodeEdit then
    FEditor := Owner as TCustomUniCodeEdit;
    
  InitForm(Self);

  FOptionProvider := OptionProvider;

  if FOptionProvider = nil then
    raise Exception.Create('Internal error: No option provider given.');
end;


//----------------------------------------------------------------------------------------------------------------------

procedure TTextSearchForm.FormCreate(Sender: TObject);

begin
  PageControl.ActivePageIndex := 0;
  OptionsTabSheet.TabVisible := False;

  Height := 198;

  if (FOptionProvider<>nil) and
    (Not(FOptionProvider.RestoreWindowPos(self, False)))then
  begin
    Left:=(Screen.Width+Width) div 2 - Width;
    Top:=(Screen.Height+Height) div 2 - Height;
  end;

  FCurrentOptions := [tsoEntireScope];

  DetailsVisible := FOptionProvider.OptionAsBoolean['ShowTextSearchDetails'];
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTextSearchForm.FormClose(Sender: TObject; var Action: TCloseAction);

begin
  if Assigned(FOptionProvider) then
    FOptionProvider.AddWindowPos(self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTextSearchForm.CloseBtnClick(Sender: TObject);

begin
  Close;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTextSearchForm.SearchBtnClick(Sender: TObject);

var
  ActiveOptions: TTextSearchOptions;

begin
  if Assigned(FOnSearch) then
  begin
    ActiveOptions := FCurrentOptions;
    if Assigned(FEditor) and FEditor.SelectionAvailable then
    begin
      Exclude(ActiveOptions, tsoEntireScope);
      FEditor.CaretXY := FEditor.BlockEnd;
    end;

    NoMatchFoundPnl.Visible := FOnSearch(Self, SearchLU.Text, '', ActiveOptions) = 0;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTextSearchForm.ReplaceBtnClick(Sender: TObject);

var
  ActiveOptions: TTextSearchOptions;

begin
  if Assigned(FOnSearch) then
  begin
    ActiveOptions := FCurrentOptions + [tsoReplace];
    if Assigned(FEditor) and FEditor.SelectionAvailable then
    begin
      Exclude(ActiveOptions, tsoEntireScope);
      FEditor.CaretXY := FEditor.BlockEnd;
    end;

    NoMatchFoundPnl.Visible := FOnSearch(Self, ReplaceSourceLU.Text, ReplaceWithLU.Text, ActiveOptions) = 0;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTextSearchForm.ReplaceAllBtnClick(Sender: TObject);

var
  SearchOptions: TTextSearchOptions;

begin
  if Assigned(FOnSearch) then
  begin
    SearchOptions := FCurrentOptions;
    include(SearchOptions, tsoReplaceAll);

    FOnSearch(Self, ReplaceSourceLU.Text, ReplaceWithLU.Text, SearchOptions);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTextSearchForm.CaseSensitiveCBoxClick(Sender: TObject);

begin
  UpdateCurrentOptions;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTextSearchForm.UpdateCurrentOptions;

begin
  FCurrentOptions := [];

  if CaseSensitiveCBox.Checked then
    include(FCurrentOptions, tsoMatchCase);

  if WholeWordsOnlyCBox.Checked then
    include(FCurrentOptions, tsoWholeWord);

  if UseRegExCBox.Checked then
    include(FCurrentOptions, tsoRegularExpression);

  if SearchInSelectedColumnRBtn.Checked then
    include(FCurrentOptions, tsoSelectedOnly);

  if SearchUpRBtn.Checked then
    include(FCurrentOptions, tsoBackwards);

  if SearchFromTopRBtn.Checked then
    include(FCurrentOptions, tsoEntireScope);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTextSearchForm.FormDeactivate(Sender: TObject);

begin
  if not MyxCommonOptions.DisableTransparencyEffects then
    AlphaBlend := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTextSearchForm.FormActivate(Sender: TObject);

begin
  AlphaBlend := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTextSearchForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  if (Key=VK_RETURN) and (Shift=[]) and
    (PageControl.ActivePage=SearchTabSheet) then
    SearchBtnClick(self)
  else
    if (Key=VK_ESCAPE) and (Shift=[]) then
     Close;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTextSearchForm.DetailsBtnClick(Sender: TObject);

begin
  DetailsVisible := Not(DetailsVisible);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTextSearchForm.PageControlChange(Sender: TObject);

begin
  if OptionPnl.Parent<>OptionsTabSheet then
  begin
    OptionPnl.Parent := PageControl.ActivePage;
    OptionPnl.Left := 0;
    OptionPnl.Top := 82;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTextSearchForm.SetIOptionProvider(OptionProvider: IOptionProvider);

begin
  FOptionProvider := OptionProvider;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTextSearchForm.SetDetailsVisible(DetailsVisible: Boolean);

begin
  FDetailsVisible := DetailsVisible;

  FOptionProvider.OptionAsBoolean['ShowTextSearchDetails'] := DetailsVisible;

  if DetailsVisible and (OptionPnl.Parent = OptionsTabSheet) then
  begin
    Height := 198 + 148;
    OptionPnl.Align := alNone;
    OptionPnl.Parent := PageControl.ActivePage;
    OptionPnl.Left := 0;
    OptionPnl.Top := 82;
    OptionPnl.Height := 160;

    DetailsBtn.Caption := _('<< Details');
    Details2Btn.Caption := DetailsBtn.Caption;
  end
  else
    if not DetailsVisible and (OptionPnl.Parent <> OptionsTabSheet) then
    begin
      OptionPnl.Parent := OptionsTabSheet;
      Height := 198;

      DetailsBtn.Caption := _('Details >>');
      Details2Btn.Caption := DetailsBtn.Caption;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

initialization
finalization
  TextSearchForm.Free;
end.
