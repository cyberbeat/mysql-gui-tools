unit QueryBrowserOptionPages;

// Copyright (C) 2003, 2004 MySQL AB
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Options, TntStdCtrls, AuxFuncs, TntComCtrls;

type
  TQueryBrowserOptionPagesForm = class(TApplicationOptionsForm)
    OptionPageControl: TTntPageControl;
    BrowserTabSheet: TTntTabSheet;
    GroupBox4: TTntGroupBox;
    HideTabWhenOneOpenCBox: TTntCheckBox;
    ToolbarGradientCBox: TTntCheckBox;
    TntGroupBox1: TTntGroupBox;
    EnforceEditableQueriesCBox: TTntCheckBox;
    OpenExportedResultsetCBox: TTntCheckBox;
    TntGroupBox2: TTntGroupBox;
    AssociateFileExtensionsCBox: TTntCheckBox;
    ShowAdvancedToolbarMI: TTntCheckBox;
    ShowFieldOverlayImagesCBox: TTntCheckBox;
    EnableFriendlyLineBreaksCBox: TTntCheckBox;
    SubstLFLU: TTntComboBox;
    TntLabel1: TTntLabel;
    TntLabel2: TTntLabel;
    SubstCRLU: TTntComboBox;
    CreateWindowsStyleLineBreaksCBox: TTntCheckBox;
    ShowMouseCursorToolbarGroupCBox: TTntCheckBox;
    AlignNumericColsRightCBox: TTntCheckBox;
    AutoEditCheckbox: TTntCheckBox;
    ForceQueryCheckbox: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    procedure SetControls(PageNr: integer); override;
    procedure ApplyChanges(PageNr: integer); override;

    procedure DoChange(Sender: TObject);
    procedure EnableDisableControls;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

//----------------------------------------------------------------------------------------------------------------------

implementation

//----------------------------------------------------------------------------------------------------------------------

uses
  ApplicationDataModule;

{$R *.dfm}

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserOptionPagesForm.FormCreate(Sender: TObject);

begin
  InitForm(self);

  OptionsImgNames.Add('options_querybrowser');

  DockOptionPageControl := OptionPageControl;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserOptionPagesForm.FormDestroy(Sender: TObject);

begin
  //
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserOptionPagesForm.FormClose(Sender: TObject; var Action: TCloseAction);

begin
  //
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserOptionPagesForm.SetControls(PageNr: integer);

var
  Options: IOptionProvider;

begin
  Options := ApplicationDM.QBOptions;

  if (PageNr = 0) then
  begin
    ToolbarGradientCBox.Checked := ApplicationDM.QBOptions.UseToolbarGradient;
    HideTabWhenOneOpenCBox.Checked := ApplicationDM.QBOptions.HideTabWhenOneOpen;
    ShowFieldOverlayImagesCBox.Checked := ApplicationDM.QBOptions.ShowFieldOverlayImages;
    ShowMouseCursorToolbarGroupCBox.Checked := ApplicationDM.QBOptions.ShowMouseCursorToolbarGroup;

    EnforceEditableQueriesCBox.Checked := ApplicationDM.QBOptions.EnforceEditableQueries;
    OpenExportedResultsetCBox.Checked := ApplicationDM.QBOptions.OpenExportedResultset;

    EnableFriendlyLineBreaksCBox.Checked := ApplicationDM.QBOptions.FriendlyLineBreaks;
    SubstLFLU.Text := ApplicationDM.QBOptions.FriendlyLineBreaksLF;
    SubstCRLU.Text := ApplicationDM.QBOptions.FriendlyLineBreaksCR;
    CreateWindowsStyleLineBreaksCBox.Checked := ApplicationDM.QBOptions.CreateWindowsStyleLineBreaks;

    AlignNumericColsRightCBox.Checked := ApplicationDM.QBOptions.AlignNumericColsRight;
    AssociateFileExtensionsCBox.Checked := ApplicationDM.QBOptions.AssociateFileExtensions;
    ShowAdvancedToolbarMI.Checked := ApplicationDM.QBOptions.ShowAdvancedToolbar;

    AutoEditCheckbox.Checked := Options.OptionAsBoolean['ResultsetAutoEdit'];
    ForceQueryCheckbox.Checked := Options.OptionAsBoolean['ForceScriptOnErrors'];

    EnableDisableControls;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserOptionPagesForm.ApplyChanges(PageNr: integer);

var
  Options: IOptionProvider;

begin
  Options := ApplicationDM.QBOptions;
  
  if (PageNr = 0) then
  begin
    ApplicationDM.QBOptions.UseToolbarGradient := ToolbarGradientCBox.Checked;
    ApplicationDM.QBOptions.HideTabWhenOneOpen := HideTabWhenOneOpenCBox.Checked;
    ApplicationDM.QBOptions.ShowFieldOverlayImages := ShowFieldOverlayImagesCBox.Checked;
    ApplicationDM.QBOptions.ShowMouseCursorToolbarGroup := ShowMouseCursorToolbarGroupCBox.Checked;


    ApplicationDM.QBOptions.EnforceEditableQueries := EnforceEditableQueriesCBox.Checked;
    ApplicationDM.QBOptions.OpenExportedResultset := OpenExportedResultsetCBox.Checked;

    ApplicationDM.QBOptions.FriendlyLineBreaks := EnableFriendlyLineBreaksCBox.Checked;
    ApplicationDM.QBOptions.FriendlyLineBreaksLF := SubstLFLU.Text[1];
    ApplicationDM.QBOptions.FriendlyLineBreaksCR := SubstCRLU.Text[1];
    ApplicationDM.QBOptions.CreateWindowsStyleLineBreaks := CreateWindowsStyleLineBreaksCBox.Checked;

    ApplicationDM.QBOptions.AlignNumericColsRight := AlignNumericColsRightCBox.Checked;
    ApplicationDM.QBOptions.ShowAdvancedToolbar := ShowAdvancedToolbarMI.Checked;
    ApplicationDM.QBOptions.AssociateFileExtensions := AssociateFileExtensionsCBox.Checked;

    Options.OptionAsBoolean['ResultsetAutoEdit'] := AutoEditCheckbox.Checked;
    Options.OptionAsBoolean['ForceScriptOnErrors'] := ForceQueryCheckbox.Checked;

    ApplicationDM.QBOptions.StoreOptions;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserOptionPagesForm.DoChange(Sender: TObject);

begin
  if (@DoPageContentChanged <> nil) then
    DoPageContentChanged(Sender);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserOptionPagesForm.EnableDisableControls;

begin
  //
end;

//----------------------------------------------------------------------------------------------------------------------

end.

