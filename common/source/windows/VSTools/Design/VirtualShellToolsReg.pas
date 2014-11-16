unit VirtualShellToolsReg;
                   
// Version 1.2.0
//   The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except
// in compliance with the License. You may obtain a copy of the
// License at
//
// http://www.mozilla.org/MPL/
//
//   Software distributed under the License is distributed on an
// " AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either expressed or
// implied. See the License for the specific language governing rights
// and limitations under the License.
//
//
//   Alternatively, the contents of this file may be used under
// the terms of the GNU General Public License Version 2 or later
// (the "GPL"), in which case the provisions of the GPL are applicable
// instead of those above. If you wish to allow use of your version of
// this file only under the terms of the GPL and not to allow others to
// use your version of this file under the MPL, indicate your decision
// by deleting the provisions above and replace them with the notice and
// other provisions required by the GPL. If you do not delete the provisions
// above, a recipient may use your version of this file under either the
// MPL or the GPL.
//
// The initial developer of this code is Jim Kueneman <jimdk@mindspring.com>


interface

{$include Compilers.inc}
{$include ..\Include\VSToolsAddIns.inc}

// If you are upgrading from VSTools 0.9.xx or earlier uncomment the conditional
// define.  By doing so the old components ExplorerTreeview, ExplorerListview,
// and ExplorerCombobox will also be installed in the palette.  This way your
// projects can be opened then show the form as text and change the TExplorerXXXX
// object types to TVirtualExplorerXXXXX then let Delphi update what it needs to
// by answering yes to the questions when the text is then shown again as a Form.
// I updated all the demos this way and it was suprisingly painless.


// {$DEFINE VSTOOLS_UPGRADE}

uses
  Windows, Messages, SysUtils, Classes, Controls,
  VirtualShellToolbar, VirtualExplorerTree, StdCtrls, VirtualShellUtilities,
  VirtualShellNewMenu, AppletsAndWizards, VirtualUnicodeControls, VirtualShellHistory,
  VirtualShellAutoComplete, VirtualSendToMenu, VirtualRedirector,
  {$IFNDEF COMPILER_7_UP}
  Exptintf,
  {$ENDIF}
  {$IFDEF COMPILER_6_UP}
    DesignIntf, DesignEditors
  {$ELSE}
    DsgnIntf
  {$ENDIF};

procedure Register;

implementation

procedure Register;
begin
  {$IFDEF EXPLORERCOMBOBOX}
  // Unicode Controls
  RegisterComponents('Virtual Controls', [TWideEdit, TWideSpeedButton]);
  RegisterComponents('VirtualShellTools', [TVirtualExplorerComboBox]);
  {$ENDIF}

  // Virtual Explorer Tree Components
  RegisterComponents('VirtualShellTools', [TVirtualExplorerTree, TVirtualExplorerTreeview,
    TVirtualExplorerListview]);

  // Virtual Virtual Shell Toolbar Components
  RegisterComponents('VirtualShellTools', [TVirtualShellToolbar, TVirtualDriveToolbar,
    TVirtualSpecialFolderToolbar]);

  // Assorted ShellTools Components
  RegisterComponents('VirtualShellTools', [TVirtualShellLink, TVirtualShellNewMenu,
    TVirtualShellHistory, TVirtualShellMRU, TVirtualShellAutoComplete, TVirtualSendToMenu]);

  // Applets, Wizards, and Shell Dialogs Components
  RegisterComponents('VirtualShellTools', [TVirtualRunFileDialog]);

  // Redirector Components
  RegisterComponents('VirtualShellTools', [TVirtualCommandLineRedirector,
    TVirtualRedirector]);

  {$IFDEF VSTOOLS_UPGRADE}
  // Virtual Explorer Tree Components
  RegisterComponents('VirtualShellTools', [TVirtualExplorerTree, TExplorerTreeview,
    TExplorerListview, TExplorerComboBox, TWideEdit]);
  {$ENDIF}

end;

end.


