unit Password;

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
//----------------------------------------------------------------------------------------------------------------------
//
// This dialog is used to ask for username and password for a scheduled task.

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  MsTask, TntForms, StdCtrls, TntStdCtrls;

type
  TPasswordDialog = class(TTntForm)
    TntLabel1: TTntLabel;
    TntLabel2: TTntLabel;
    TntLabel3: TTntLabel;
    UserNameEdit: TTntEdit;
    PasswordEdit: TTntEdit;
    CancelButton: TTntButton;
    OKButton: TTntButton;
    procedure FormCreate(Sender: TObject);
  public
    class function DetermineUserCredentials(Task: ITask): Boolean;
  end;

var
  PasswordDialog: TPasswordDialog;

//----------------------------------------------------------------------------------------------------------------------
implementation

{$R *.dfm}

uses
  ActiveX, AuxFuncs;

//----------------- TPasswordDialog ------------------------------------------------------------------------------------

class function TPasswordDialog.DetermineUserCredentials(Task: ITask): Boolean;

// Creates a temporary instance of the password dialog and asks for username and password.

var
  Raw: PWideChar;
  Buffer: array[0..1024] of WideChar;
  Size: Cardinal;

begin
  Result := false;
  with TPasswordDialog.Create(nil) do
  try
    // Initialize with existing account information if any.
    if Task.GetAccountInformation(Raw) = S_OK then
    begin
      UserNameEdit.Text := Raw;
      CoTaskMemFree(Raw);
    end
    else
    begin
      Size := SizeOf(Buffer);
      GetUserNameW(@Buffer, Size);
      UserNameEdit.Text := Buffer;
    end;
    PasswordEdit.Text := '';

    while True do
    begin
      Result := ShowModal = mrOK;
      if Result then
      begin
        if PasswordEdit.Text = '' then
        begin
          if Application.MessageBox('Password field is empty. Do you want to continue?', 'Empty password', MB_YESNO) <> IDYES then
            Continue;
        end;
        Task.SetAccountInformation(PWideChar(UserNameEdit.Text), PWideChar(PasswordEdit.Text));
        Break;
      end
      else
        Break;
    end;
  finally
    Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPasswordDialog.FormCreate(Sender: TObject);

begin
  InitForm(Self);

  // Set the small black circle as pasword char.
  PasswordEdit.PasswordChar := #$2022;
end;

//----------------------------------------------------------------------------------------------------------------------

end.

